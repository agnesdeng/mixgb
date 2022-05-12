# A function use to obtain yhatobs using the whole dataset (use for pmm.type=1)

save_yhatobs <- function(yobs.list, maxit, pmm.link, sorted.dt, missing.vars, extra.vars = NULL, extra.types = NULL, sorted.names, Na.idx, missing.types, Ncol,
                         xgb.params = list(max_depth = 6, gamma = 0.1, eta = 0.3, colsample_bytree = 1, min_child_weight = 1, subsample = 1, tree_method = "auto", gpu_id = 0, predictor = "auto", scale_pos_weight = 1),
                         nrounds = 50, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0,
                         ...) {

  #param xgb.params NULL if XGBmodels was fed in
  #return a list of yhatobs values for specified variables
  #check whether xgb.params contains sample related hyperparameters, need to coerce to 1 as we want to obtain yhatobs using the whole dataset

  sample.params <- grepl("sample", names(xgb.params))
  if (any(sample.params == TRUE)) {
    xgb.params[sample.params] <- 1
  }

  save.p <- length(missing.vars) + length(extra.vars)
  yhatobs.list <- vector("list", save.p)
  names(yhatobs.list) <- c(missing.vars, extra.vars)

  # general case : save all variables with NAs
  for (i in seq_len(maxit)) {
    for (var in missing.vars) {
      features <- setdiff(sorted.names, var)
      form <- reformulate(termlabels = features, response = var)

      na.idx <- Na.idx[[var]]
      obs.y <- yobs.list[[var]]

      if (length(obs.y) == 1) {
        warning(paste("PMM to this dataset may not be sensible as there is only one observed entry in the variable ", var, sep = ""))
      }

      if (Ncol == 2) {
        obs.data <- sparse.model.matrix(form, data = sorted.dt[-na.idx, ])
        mis.data <- sparse.model.matrix(form, data = sorted.dt[na.idx, ])
      } else {
        obs.data <- sparse.model.matrix(form, data = sorted.dt[-na.idx, ])[, -1, drop = FALSE]
        mis.data <- sparse.model.matrix(form, data = sorted.dt[na.idx, ])[, -1, drop = FALSE]
      }


      if (missing.types[var] == "numeric" | missing.types[var] == "integer") {
        obj.type <- "reg:squarederror"
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, objective = obj.type,
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        if (i != maxit) {
          yhatmis <- predict(xgb.fit, mis.data)
          sorted.dt[[var]][na.idx] <- yhatmis
        } else {
          # last iteration
          yhatobs.list[[var]] <- predict(xgb.fit, obs.data)
        }
      } else if (missing.types[var] == "binary") {
        obs.y <- as.integer(obs.y) - 1
        bin.t <- sort(table(obs.y))
        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class
        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class
          if (i != maxit) {
            sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1]
          } else {
            yhatobs.list[[var]] <- rep(levels(sorted.dt[[var]])[as.integer(sorted.names(bin.t[1])) + 1], length(obs.y))
          }
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
          if (i != maxit) {
            yhatmis <- predict(xgb.fit, mis.data)
            if (pmm.link == "logit") {
              # if prediction is logit
              prob <- exp(yhatmis) / (1 + exp(yhatmis))
            }
            yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
            sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
          } else {
            # if pmm.link="logit", these would be logit values, otherwise would be probability values
            yhatobs.list[[var]] <- predict(xgb.fit, obs.data)
          }
        }
      } else {
        # multiclass
        obs.y <- as.integer(obs.y) - 1
        yobs.list[[var]] <- obs.y
        if (i != maxit) {
          # predict return class
          obj.type <- "multi:softmax"
        } else {
          # predict return prob
          obj.type <- "multi:softprob"
        }

        N.class <- length(levels(sorted.dt[[var]]))
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, num_class = N.class, objective = obj.type, eval_metric = "mlogloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        if (i != maxit) {
          # only update datset, prediction return predicted class
          yhatmis <- predict(xgb.fit, mis.data)
          sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
        } else {
          # prediction returns probability for matching: probability matrix for each class
          yhatobs.list[[var]] <- predict(xgb.fit, obs.data, reshape = TRUE)
        }
      }
    } # end of for each missing variable
  }

  # for extra variables -----------------------------------------------------


  if (!is.null(extra.vars)) {
    # if there are extra.vars, use the sorted.dt after the last iteration to obtain models

    for (var in extra.vars) {
      features <- setdiff(sorted.names, var)
      form <- reformulate(termlabels = features, response = var)

      obs.y <- yobs.list[[var]]


      if (Ncol == 2) {
        obs.data <- sparse.model.matrix(form, data = sorted.dt)
      } else {
        obs.data <- sparse.model.matrix(form, data = sorted.dt)[, -1, drop = FALSE]
      }


      if (extra.types[var] == "numeric" | extra.types[var] == "integer") {
        obj.type <- "reg:squarederror"
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, objective = obj.type,
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        yhatobs.list[[var]] <- predict(xgb.fit, obs.data)
      } else if (extra.types[var] == "binary") {
        obs.y <- as.integer(obs.y) - 1
        bin.t <- sort(table(obs.y))
        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class
        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class
          yhatobs.list[[var]] <- rep(levels(sorted.dt[[var]])[as.integer(sorted.names(bin.t[1])) + 1], length(obs.y))
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
          # if pmm.link="logit", these would be logit values, otherwise would be probability values
          yhatobs.list[[var]] <- predict(xgb.fit, obs.data)
        }
      } else {
        # multiclass
        obs.y <- as.integer(obs.y) - 1
        yobs.list[[var]] <- obs.y
        if (i != maxit) {
          # predict return class
          obj.type <- "multi:softmax"
        } else {
          # predict return prob
          obj.type <- "multi:softprob"
        }

        N.class <- length(levels(sorted.dt[[var]]))
        xgb.fit <- xgboost(
          data = obs.data, label = obs.y, num_class = N.class, objective = obj.type, eval_metric = "mlogloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = verbose,
          ...
        )
        # prediction returns probability for matching: probability matrix for each class
        yhatobs.list[[var]] <- predict(xgb.fit, obs.data, reshape = TRUE)
      }
    } # end of for each extra variable
  }


  return(yhatobs.list)
}
