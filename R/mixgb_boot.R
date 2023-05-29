# Multiple imputation using xgboost with bootstrap

mixgb_boot <- function(BNa.idx, boot.dt, pmm.type, pmm.link, pmm.k, yobs.list, yhatobs.list, sorted.dt, missing.vars, sorted.names, Na.idx, missing.types, Ncol,
                       xgb.params = list(),
                       nrounds = 100, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0,
                       ...) {
  for (var in missing.vars) {
    features <- setdiff(sorted.names, var)
    form <- reformulate(termlabels = features, response = var)


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

    # obs.data: observed data from the bootstrapped sample
    # Obs.data: observed data from the whole sample
    # mis.data: missing data from the whole sample
    # ................................................................
    if (missing.types[var] == "numeric" | missing.types[var] == "integer") {
      obj.type <- "reg:squarederror"
      xgb.fit <- xgboost(
        data = obs.data, label = obs.y, objective = obj.type,
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
        ...
      )

      yhatmis <- predict(xgb.fit, mis.data)
      if (!is.null(pmm.type)) {
        if (pmm.type != 1) {
          # for pmm.type=0 or 2 or auto (type 2 for numeric or integer)
          yhatobs <- predict(xgb.fit, Obs.data)
          yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        } else {
          # for pmm.type=1
          yhatmis <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
      }
      # update dataset
      sorted.dt[[var]][na.idx] <- yhatmis
    } else if (missing.types[var] == "binary") {
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
      } else {
        if (!is.null(pmm) & pmm.link == "logit") {
          # pmm by "logit" value
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" , and for no pmm
          obj.type <- "binary:logistic"
        }
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, objective = obj.type, eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        yhatmis <- predict(xgb.fit, mis.data)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
          sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1  (yobs.list is of original form "Yes" "No"  but obs.y is 0 or 1)
            sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, Obs.data)
            sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          }
        }
      }
    } else if (missing.types[var] == "logical") {
      bin.t <- sort(table(obs.y))
      # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
      # when bin.t only has one value: bin.t[1] the only existent class
      if (is.na(bin.t[2])) {
        # this binary variable only have one class being observed (e.g., observed values are all "0"s)
        # skip xgboost training, just impute the only existent class
        msg <- paste("The logical variable", var, "in the bootstrapped sample only has a single class. The only existent class will be used to impute NAs. Imputation model for this variable may not be reliable. Recommend to get more data. ")
        warning(msg)
        sorted.dt[[var]][na.idx] <- as.logical(names(bin.t[1]))
      } else {
        if (!is.null(pmm) & pmm.link == "logit") {
          # pmm by "logit" value
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" , and for no pmm
          obj.type <- "binary:logistic"
        }
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, objective = obj.type, eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        yhatmis <- predict(xgb.fit, mis.data)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, T, F)
          sorted.dt[[var]][na.idx] <- yhatmis
        } else {
          if (pmm.type == 1) {
            sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, Obs.data)
            sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          }
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

      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        # use softmax, predict returns class
        # for pmm.type=NULL or "auto"
        yhatmis <- predict(xgb.fit, mis.data)
        sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
      } else {
        # predict returns probability matrix for each class
        yhatmis <- predict(xgb.fit, mis.data, reshape = TRUE)

        if (pmm.type == 1) {
          yhatobs <- yhatobs.list[[var]]
        } else {
          yhatobs <- predict(xgb.fit, Obs.data, reshape = TRUE)
        }

        yhatmis <- pmm.multiclass(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis]
      }
    }
  } # end of for each missing variable
  return(sorted.dt)
}


# helper function for mixgb_boot()
boot <- function(Nrow, sorted.dt, sortedNA.dt, missing.vars, mp) {
  # use bootstrap to achieve multiple imputation
  boot.idx <- sample(Nrow, Nrow, replace = TRUE)
  # bootstrapped data with initial imputed values
  boot.dt <- sorted.dt[boot.idx, ]
  # bootstrapped data with NAs
  bootNA.dt <- sortedNA.dt[boot.idx, ]
  # use bootstrap data to build xgboost model, then impute missing data in the whole dataset
  BNa.idx <- vector("list", mp)
  names(BNa.idx) <- missing.vars
  for (var in missing.vars) {
    bna.idx <- which(is.na(bootNA.dt[[var]]))
    BNa.idx[[var]] <- bna.idx
  }

  # checking the bootstrapped sample
  if (any(sapply(BNa.idx, length) == Nrow)) {
    stop("At least one variable in the boostrapped sample has 100% missing values.\n
                                  This implies that there is at least one variable in the original dataset has too many missing entries.\n
                                  Imputation procedure aborts.\n
                                  Please consider removing variables with too many missing values before imputation.")
  }

  return(list("BNa.idx" = BNa.idx, "boot.dt" = boot.dt))
}
