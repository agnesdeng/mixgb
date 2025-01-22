# Multiple imputation using xgboost (without bootstrap) updated version
mixgb_null <- function(Obs.m, matrix.method, cbind.types, pmm.type, pmm.link, pmm.k, yobs.list, yhatobs.list = NULL, sorted.dt, missing.vars, sorted.names, Na.idx, missing.types, Ncol,
                       xgb.params = list(),
                       nrounds, early_stopping_rounds, print_every_n, verbose,
                       ...) {
  nthread <- xgb.params$nthread


  for (var in missing.vars) {
    na.idx <- Na.idx[[var]]
    obs.y <- yobs.list[[var]]

    # Mis.vars: missing variables except the current imputed variable (as response)
    if (length(missing.vars) != 1) {
      Mis.vars <- missing.vars[missing.vars != var]




      if (matrix.method == "as.matrix") {
        Mis.m <- as.matrix(sorted.dt[, Mis.vars, with = FALSE])
      } else {
        Mis.list <- lapply(Mis.vars, function(feature) {
          if (cbind.types[feature] %in% c("numeric", "integer")) {
            as.matrix(sorted.dt[[feature]])
          } else if (cbind.types[feature] == "ordered") {
            Matrix::t(fac2Sparse(sorted.dt[[feature]], drop.unused.levels = FALSE, factorPatt12 = c(T, F), contrasts.arg = "contr.poly")[[1]])
          } else {
            Matrix::t(fac2sparse(sorted.dt[[feature]], drop.unused.levels = FALSE))[, -1, drop = FALSE]
          }
        })


        if (matrix.method == "cpp.combo") {
          Mis.m <- cbind_combo(Mis.list)
        } else if (matrix.method == "cpp.factor") {
          Mis.m <- cbind_sparse_matrix(Mis.list)
        }
      }

      All.m <- cbind2(Mis.m, Obs.m)
    } else {
      All.m <- Obs.m
    }



    obs.data <- All.m[-na.idx, , drop = FALSE]
    mis.data <- All.m[na.idx, , drop = FALSE]




    # numeric or integer ---------------------------------------------------------------------------
    if (missing.types[var] == "numeric") {
      dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dobs)
      } else {
        watchlist <- list(train = dobs)
        # to be done, have eval
        # watchlist <- list(train = dobs,eval=dmis)
      }


      obj.type <- "reg:squarederror"
      xgb.fit <- xgb.train(
        data = dobs, objective = obj.type, watchlist = watchlist,
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
        print_every_n = print_every_n, verbose = verbose, ...
      )

      yhatmis <- predict(xgb.fit, dmis)


      if (!is.null(pmm.type)) {
        if (pmm.type != 1) {
          # for pmm.type=0 or 2 or auto (type 2 for numeric or integer)
          yhatobs <- predict(xgb.fit, dobs)
        } else {
          # for pmm.type=1
          yhatobs <- yhatobs.list[[var]]
        }
        yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = obs.y, k = pmm.k)
      }
      # update dataset

      sorted.dt[na.idx, (var) := yhatmis]
    } else if (missing.types[var] == "integer") {
      dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dobs)
      } else {
        watchlist <- list(train = dobs)
        # to be done, have eval
        # watchlist <- list(train = dobs,eval=dmis)
      }

      obj.type <- "reg:squarederror"
      xgb.fit <- xgb.train(
        data = dobs, objective = obj.type, watchlist = watchlist,
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
        print_every_n = print_every_n, verbose = verbose, ...
      )

      yhatmis <- predict(xgb.fit, dmis)
      if (!is.null(pmm.type)) {
        if (pmm.type != 1) {
          # for pmm.type=0 or 2 or auto (type 2 for numeric or integer)
          yhatobs <- predict(xgb.fit, dobs)
        } else {
          # for pmm.type=1
          yhatobs <- yhatobs.list[[var]]
        }
        yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = obs.y, k = pmm.k)
        sorted.dt[na.idx, (var) := yhatmis]
      } else {
        # round to integer when PMM is not used
        sorted.dt[na.idx, (var) := round(yhatmis)]
      }
    } else if (missing.types[var] == "binary") {
      # binary ---------------------------------------------------------------------------
      obs.y <- as.integer(obs.y) - 1
      bin.t <- sort(table(obs.y))

      dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dobs)
      } else {
        watchlist <- list(train = dobs)
        # to be done, have eval
        # watchlist <- list(train = dobs,eval=dmis)
      }

      # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
      # when bin.t only has one value: bin.t[1] the only existent class
      if (is.na(bin.t[2])) {
        # this binary variable only has a single class being observed (e.g., observed values are all "0"s)
        # skip xgboost training, just impute the only existent class
        yhatmis <- levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1]
        sorted.dt[na.idx, (var) := yhatmis]
        msg <- paste("The binary variable", var, "in the data only have single class. Imputation models can't be built.")
        stop(msg)
      } else {
        if (!is.null(pmm.type) & isFALSE(pmm.type == "auto") & pmm.link == "logit") {
          # pmm by "logit" value, only when pmm.type is not null and not "auto"
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" and for no pmm
          obj.type <- "binary:logistic"
        }
        xgb.fit <- xgb.train(
          data = dobs, objective = obj.type, watchlist = watchlist,
          eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )

        yhatmis <- predict(xgb.fit, dmis)

        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto": no pmm is used, obj.type="binary:logistic
          yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
          yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatobs <- yhatobs.list[[var]]
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, dobs)
          }


          yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          sorted.dt[na.idx, (var) := yhatmis]
        }
      }
    } else if (missing.types[var] == "logical") {
      dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dobs)
      } else {
        watchlist <- list(train = dobs)
        # to be done, have eval
        # watchlist <- list(train = dobs,eval=dmis)
      }

      bin.t <- sort(table(obs.y))
      # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
      # when bin.t only has one value: bin.t[1] the only existent class
      if (is.na(bin.t[2])) {
        # this binary variable only has a single class being observed (e.g., observed values are all "0"s)
        # skip xgboost training, just impute the only existent class
        yhatmis <- as.logical(names(bin.t[1]))
        sorted.dt[na.idx, (var) := yhatmis]
        msg <- paste("The logical variable", var, "in the data only have single class. Imputation models can't be built.")
        stop(msg)
      } else {
        if (!is.null(pmm) & pmm.link == "logit") {
          # pmm by "logit" value
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" and for no pmm
          obj.type <- "binary:logistic"
        }

        xgb.fit <- xgb.train(
          data = dobs, objective = obj.type, watchlist = watchlist,
          eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )
        yhatmis <- predict(xgb.fit, dmis)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, T, F)
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatobs <- yhatobs.list[[var]]
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, dobs)
          }

          yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          sorted.dt[na.idx, (var) := yhatmis]
        }
      }
    } else {
      # multiclass ---------------------------------------------------------------------------
      obs.y <- as.integer(obs.y) - 1

      dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dobs)
      } else {
        watchlist <- list(train = dobs)
        # to be done, have eval
        # watchlist <- list(train = dobs,eval=dmis)
      }

      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        obj.type <- "multi:softmax"
      } else {
        # use probability to do matching
        obj.type <- "multi:softprob"
      }
      N.class <- length(levels(sorted.dt[[var]]))


      xgb.fit <- xgb.train(
        data = dobs, num_class = N.class,
        objective = obj.type, watchlist = watchlist,
        eval_metric = "mlogloss",
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
        print_every_n = print_every_n, verbose = verbose, ...
      )


      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        # use softmax, predict returns class
        # for pmm.type=NULL or "auto"
        yhatmis <- predict(xgb.fit, dmis)
        yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
        sorted.dt[na.idx, (var) := yhatmis]
      } else {
        # predict returns probability matrix for each class
        # yhatmis <- predict(xgb.fit, mis.data, reshape = TRUE)

        yhatmis <- predict(xgb.fit, dmis, reshape = TRUE)
        if (pmm.type == 1) {
          # for pmm.type=1
          yhatobs <- yhatobs.list[[var]]
        } else {
          # for pmm.type=0 or 2
          # probability matrix for each class
          # yhatobs <- predict(xgb.fit, obs.data, reshape = TRUE)
          yhatobs <- predict(xgb.fit, dobs, reshape = TRUE)
        }
        yhatmis <- pmm.multiclass(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        yhatmis <- levels(sorted.dt[[var]])[yhatmis]
        sorted.dt[na.idx, (var) := yhatmis]
      }
    }
    # for each var
  }
  # end of for each missing variable
  sorted.dt
}
