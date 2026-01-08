# Multiple imputation using xgboost with bootstrap (save models using xgb.save() and imputations)
mixgb_boot_xgb_save <- function(save.models.folder, i, BNa.idx, boot.dt, save.vars, save.p, extra.vars = NULL, extra.types = NULL, pmm.type, pmm.link, pmm.k,
                                yobs.list, yhatobs.list = NULL, sorted.dt,
                                missing.vars, sorted.names, Na.idx, missing.types, Ncol,
                                xgb.params = list(),
                                nrounds, early_stopping_rounds, print_every_n, verbose,
                                ...) {
  # yhatobs.list if it is pmm.type 1, must feed in the yhatobs.list

  # pre-allocation for models
  xgb.models <- vector("list", save.p)
  names(xgb.models) <- save.vars

  # pre-allocation for models for pmm.type=0,2,or "auto"
  if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2) | isTRUE(pmm.type == "auto")) {
    yhatobs.list <- vector("list", save.p)
    names(yhatobs.list) <- save.vars
  }

  for (var in missing.vars) {
    features <- setdiff(sorted.names, var)
    form <- reformulate(termlabels = features, response = var)
    # bootstrap

    if (length(BNa.idx[[var]]) == 0) {
      # bootstrap sample for this variable contains no missing value
      obs.y <- boot.dt[[var]]
      if (Ncol == 2) {
        obs.data <- sparse.model.matrix(form, data = boot.dt)
      } else {
        obs.data <- sparse.model.matrix(form, data = boot.dt)[, -1, drop = FALSE]
      }
    } else {
      # bootstrap sample for this variable contains missing values
      bna.idx <- BNa.idx[[var]]
      obs.y <- boot.dt[[var]][-bna.idx]
      if (Ncol == 2) {
        obs.data <- sparse.model.matrix(form, data = boot.dt[-bna.idx, ])
      } else {
        obs.data <- sparse.model.matrix(form, data = boot.dt[-bna.idx, ])[, -1, drop = FALSE]
      }
    }

    na.idx <- Na.idx[[var]]

    if (Ncol == 2) {
      Obs.data <- sparse.model.matrix(form, data = sorted.dt[-na.idx, ])
      mis.data <- sparse.model.matrix(form, data = sorted.dt[na.idx, ])
    } else {
      Obs.data <- sparse.model.matrix(form, data = sorted.dt[-na.idx, ])[, -1, drop = FALSE]
      mis.data <- sparse.model.matrix(form, data = sorted.dt[na.idx, ])[, -1, drop = FALSE]
    }



    # numeric or integer ---------------------------------------------------------------------------
    if (missing.types[var] == "numeric" | missing.types[var] == "integer") {
      obj.type <- "reg:squarederror"
      # use bootstrap sample to build models
      xgb.fit <- xgboost(
        data = obs.data, label = obs.y, objective = obj.type,
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
        ...
      )
      # impute the NAs in the full dataset
      yhatmis <- predict(xgb.fit, mis.data)
      if (!is.null(pmm.type)) {
        if (pmm.type != 1) {
          # for pmm.type=0 or 2 or auto (type 2 for numeric or integer)
          # get yhatobs of the observed data in the full dataset (full: Obs.data->  bootstrap:obs.data)
          yhatobs <- predict(xgb.fit, Obs.data)
          yhatobs.list[[var]] <- yhatobs
        } else {
          # for pmm.type=1
          yhatobs <- yhatobs.list[[var]]
        }
        yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
      }
      # update dataset
      sorted.dt[[var]][na.idx] <- yhatmis
      # save models
      filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
      filedir <- paste(filedir, ".json", sep = "")
      xgb.save(model = xgb.fit, fname = filedir)
      xgb.models[[var]] <- filedir
    } else if (missing.types[var] == "binary") {
      # binary ---------------------------------------------------------------------------
      obs.y <- as.integer(obs.y) - 1
      bin.t <- sort(table(obs.y))
      # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
      # when bin.t only has one value: bin.t[1] the only existent class
      if (is.na(bin.t[2])) {
        # this binary variable only have one class being observed (e.g., observed values are all "0"s)
        # skip xgboost training, just impute the only existent class
        msg <- paste("The binary variable", var, "in the bootstrapped sample only has a single class. The only existent class will be used to impute NAs. Imputation model for this variable may not be reliable. Recommend to get more data. ")
        warning(msg)
        sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1]
        # save models
        xgb.models[[var]] <- levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1]
        yhatobs.list[[var]] <- rep(levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1], length(yobs.list[[var]]))
      } else {
        if (!is.null(pmm) & pmm.link == "logit") {
          # pmm by "logit" value
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" and for no pmm
          obj.type <- "binary:logistic"
        }
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, objective = obj.type, eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        # save models
        filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
        filedir <- paste(filedir, ".json", sep = "")
        xgb.save(model = xgb.fit, fname = filedir)
        xgb.models[[var]] <- filedir

        yhatmis <- predict(xgb.fit, mis.data)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
          sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatobs <- yhatobs.list[[var]]
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, Obs.data)
            yhatobs.list[[var]] <- yhatobs
          }
          sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
      }
    } else if (missing.types[var] == "logical") {
      # binary ---------------------------------------------------------------------------

      bin.t <- sort(table(obs.y))
      # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
      # when bin.t only has one value: bin.t[1] the only existent class
      if (is.na(bin.t[2])) {
        # this binary variable only have one class being observed (e.g., observed values are all "0"s)
        # skip xgboost training, just impute the only existent class
        msg <- paste("The logical variable", var, "in the bootstrapped sample only has a single class. The only existent class will be used to impute NAs. Imputation model for this variable may not be reliable. Recommend to get more data. ")
        warning(msg)
        sorted.dt[[var]][na.idx] <- as.logical(names(bin.t[1]))
        # save models
        xgb.models[[var]] <- as.logical(names(bin.t[1]))
        yhatobs.list[[var]] <- rep(as.logical(names(bin.t[1])), length(yobs.list[[var]]))
      } else {
        if (!is.null(pmm) & pmm.link == "logit") {
          # pmm by "logit" value
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" and for no pmm
          obj.type <- "binary:logistic"
        }
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, objective = obj.type, eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        # save models
        filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
        filedir <- paste(filedir, ".json", sep = "")
        xgb.save(model = xgb.fit, fname = filedir)
        xgb.models[[var]] <- filedir

        yhatmis <- predict(xgb.fit, mis.data)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, T, F)
          sorted.dt[[var]][na.idx] <- yhatmis
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatobs <- yhatobs.list[[var]]
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, Obs.data)
            yhatobs.list[[var]] <- yhatobs
          }
          sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
      }
    } else {
      # multiclass ---------------------------------------------------------------------------
      obs.y <- as.integer(obs.y) - 1
      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        obj.type <- "multi:softmax"
      } else {
        # use probability to do matching
        obj.type <- "multi:softprob"
      }
      N.class <- length(levels(sorted.dt[[var]]))
      xgb.fit <- xgboost(
        data = obs.data, label = obs.y, num_class = N.class, objective = obj.type, eval_metric = "mlogloss",
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
        ...
      )
      # save models
      filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
      filedir <- paste(filedir, ".json", sep = "")
      xgb.save(model = xgb.fit, fname = filedir)
      xgb.models[[var]] <- filedir

      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        # use softmax, predict returns class
        # for pmm.type=NULL or "auto"
        yhatmis <- predict(xgb.fit, mis.data)
        sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
      } else {
        # predict returns probability matrix for each class
        yhatmis <- predict(xgb.fit, mis.data, reshape = TRUE)
        if (pmm.type == 1) {
          # for pmm.type=1
          yhatobs <- yhatobs.list[[var]]
        } else {
          # for pmm.type=0 or 2
          # probability matrix for each class
          yhatobs <- predict(xgb.fit, Obs.data, reshape = TRUE)
          yhatobs.list[[var]] <- yhatobs
        }

        yhatmis <- pmm.multiclass(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis]
      }
    }
  } # end of for each missing variable

  # for extra variables---------------------------------------------------------------------------------------------
  if (!is.null(extra.vars)) {
    for (var in extra.vars) {
      features <- setdiff(sorted.names, var)
      form <- reformulate(termlabels = features, response = var)

      # bootstrap sample for this variable contains no missing value
      obs.y <- boot.dt[[var]]
      if (Ncol == 2) {
        obs.data <- sparse.model.matrix(form, data = boot.dt)
      } else {
        obs.data <- sparse.model.matrix(form, data = boot.dt)[, -1, drop = FALSE]
      }

      # use it to get yhatobs.list for pmm
      na.idx <- Na.idx[[var]]
      if (Ncol == 2) {
        Obs.data <- sparse.model.matrix(form, data = sorted.dt[-na.idx, ])
      } else {
        Obs.data <- sparse.model.matrix(form, data = sorted.dt[-na.idx, ])[, -1, drop = FALSE]
      }

      ############# use bootstrap sample (fully observed) to save xgb.models

      if (extra.types[var] == "numeric" | extra.types[var] == "integer") {
        obj.type <- "reg:squarederror"
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, objective = obj.type,
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
        filedir <- paste(filedir, ".json", sep = "")
        xgb.save(model = xgb.fit, fname = filedir)
        xgb.models[[var]] <- filedir
        if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2) | isTRUE(pmm.type == "auto")) {
          yhatobs.list[[var]] <- predict(xgb.fit, Obs.data)
        }
      } else if (extra.types[var] == "binary") {
        obs.y <- as.integer(obs.y) - 1
        bin.t <- sort(table(obs.y))
        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class
        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class
          msg <- paste("The binary variable", var, "in the bootstrapped sample only has a single class. The only existent class will be used to impute NAs. Imputation model for this variable may not be reliable. Recommend to get more data. ")
          warning(msg)

          xgb.models[[var]] <- levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1]
          yhatobs.list[[var]] <- rep(levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1], length(yobs.list[[var]]))
        } else {
          # general case
          if (pmm.link == "logit") {
            # pmm by "logit" value
            obj.type <- "binary:logitraw"
          } else {
            # pmm by "prob" value
            obj.type <- "binary:logistic"
          }
          xgb.fit <- xgboost(
            data = obs.data, label = obs.y, objective = obj.type, eval_metric = "logloss",
            params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
            ...
          )
          filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
          filedir <- paste(filedir, ".json", sep = "")
          xgb.save(model = xgb.fit, fname = filedir)
          xgb.models[[var]] <- filedir
          # if pmm.link="logit", these would be logit values, otherwise would be probability values
          if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2)) {
            yhatobs.list[[var]] <- predict(xgb.fit, Obs.data)
          }
        }
      } else if (extra.types[var] == "logical") {
        bin.t <- sort(table(obs.y))
        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class
        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class
          msg <- paste("The logical variable", var, "in the bootstrapped sample only has a single class. The only existent class will be used to impute NAs. Imputation model for this variable may not be reliable. Recommend to get more data. ")
          warning(msg)

          xgb.models[[var]] <- as.logical(names(bin.t[1]))
          yhatobs.list[[var]] <- rep(as.logical(names(bin.t[1])), length(yobs.list[[var]]))
        } else {
          # general case
          if (pmm.link == "logit") {
            # pmm by "logit" value
            obj.type <- "binary:logitraw"
          } else {
            # pmm by "prob" value
            obj.type <- "binary:logistic"
          }
          xgb.fit <- xgboost(
            data = obs.data, label = obs.y, objective = obj.type, eval_metric = "logloss",
            params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
            ...
          )
          filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
          filedir <- paste(filedir, ".json", sep = "")
          xgb.save(model = xgb.fit, fname = filedir)
          xgb.models[[var]] <- filedir
          # if pmm.link="logit", these would be logit values, otherwise would be probability values
          if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2)) {
            yhatobs.list[[var]] <- predict(xgb.fit, Obs.data)
          }
        }
      } else {
        # multiclass
        obs.y <- as.integer(obs.y) - 1
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          obj.type <- "multi:softmax"
        } else {
          # use probability to do matching
          obj.type <- "multi:softprob"
        }

        N.class <- length(levels(sorted.dt[[var]]))
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, num_class = N.class, objective = obj.type, eval_metric = "mlogloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        filedir <- paste(save.models.folder, paste("/xgb.model.", var, i, sep = ""), sep = "")
        filedir <- paste(filedir, ".json", sep = "")
        xgb.save(model = xgb.fit, fname = filedir)
        xgb.models[[var]] <- filedir

        # prediction returns probability for matching: probability matrix for each class
        if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2)) {
          yhatobs.list[[var]] <- predict(xgb.fit, Obs.data, reshape = TRUE)
        }
      }
    } # end of for each extra variable
  } # end of extra.vars

  if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2) | isTRUE(pmm.type == "auto")) {
    return(list("sorted.dt" = sorted.dt, "xgb.models" = xgb.models, "yhatobs.list" = yhatobs.list))
  } else {
    # for pmm.type=NULL or pmm.type=1
    return(list("sorted.dt" = sorted.dt, "xgb.models" = xgb.models))
  }
}
