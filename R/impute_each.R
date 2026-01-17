impute_numeric <- function(save, obs.data, mis.data, obs.y, xgb.params,
                           nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
                           pmm.type, pmm.k, yhatobs = NULL, round_nopmm = FALSE) {
  dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
  dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
  evals <- list(train = dobs)

  xgb.params$objective <- "reg:squarederror"
  xgb.params$eval_metric <- "rmse"
  xgb.params$num_class <- NULL

  fit <- xgb.train(
    data = dobs, evals = evals,
    params = xgb.params, nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    print_every_n = print_every_n, verbose = verbose
  )

  yhatmis <- predict(fit, dmis)

  if (!is.null(pmm.type)) {
    if (pmm.type != 1) {
      # for pmm.type=0 or 2 or auto (type 2 for numeric or integer)
      yhatobs <- predict(fit, dobs)
    } else if (!is.null(yhatobs)) {
      # for pmm.type=1
      # use predicted yhatobs passed from outside
      # yhatobs <- yhatobs.list[[var]]
      # yhatobs passed from outside
    }
    yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = obs.y, k = pmm.k)
  } else {
    if (round_nopmm) {
      yhatmis <- round(yhatmis)
    }
  }


  if (save) {
    list(yhatmis = yhatmis, fit = fit)
  } else {
    yhatmis
  }
}


impute_binary <- function(var, save, obs.data, mis.data, obs.y, xgb.params,
                          nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
                          pmm.type, pmm.link, pmm.k, yhatobs = NULL, original_levels) {
  original_labels <- obs.y
  obs.y <- as.integer(obs.y) - 1 # binary labels must be 0/1
  bin.t <- sort(table(obs.y))

  # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
  # when bin.t only has one value: bin.t[1] the only existent class
  if (is.na(bin.t[2])) {
    # this binary variable only has a single class being observed (e.g., observed values are all "0"s)
    # skip xgboost training, just impute the only existent class
    yhatmis <- original_levels[as.integer(names(bin.t[1])) + 1]
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

    dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
    dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
    evals <- list(train = dobs)

    xgb.params$objective <- "binary:logistic"
    xgb.params$eval_metric <- "logloss"
    xgb.params$num_class <- NULL

    fit <- xgb.train(
      data = dobs, evals = evals,
      params = xgb.params, nrounds = nrounds,
      early_stopping_rounds = early_stopping_rounds,
      print_every_n = print_every_n, verbose = verbose
    )

    yhatmis <- predict(fit, dmis)

    # Convert probability to class if PMM not used
    if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
      yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
      # if (!is.null(original_levels))
      yhatmis <- original_levels[yhatmis + 1]
    } else {
      if (pmm.type == 1 && !is.null(yhatobs)) {
        # yhatobs passed - use outside yhatobs.list
      } else {
        # for pmm.type=0 or 2
        yhatobs <- predict(fit, dobs)
      }
      yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = original_labels, k = pmm.k)
    }
  }

  if (save) {
    list(yhatmis = yhatmis, fit = fit)
  } else {
    yhatmis
  }
}


impute_logical <- function(var, save, obs.data, mis.data, obs.y, xgb.params,
                           nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
                           pmm.type, pmm.link, pmm.k, yhatobs = NULL, original_levels) {
  original_labels <- obs.y
  bin.t <- sort(table(obs.y))

  # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
  # when bin.t only has one value: bin.t[1] the only existent class
  if (is.na(bin.t[2])) {
    # this binary variable only has a single class being observed (e.g., observed values are all "0"s)
    # skip xgboost training, just impute the only existent class
    yhatmis <- as.logical(names(bin.t[1]))
    msg <- paste("The logical variable", var, "in the data only have single class. Imputation models can't be built.")
    stop(msg)
  } else {
    if (!is.null(pmm.type) & pmm.link == "logit") {
      # !is.null(pmm.type) & isFALSE(pmm.type == "auto") & pmm.link == "logit"
      # pmm by "logit" value, only when pmm.type is not null and not "auto"
      obj.type <- "binary:logitraw"
    } else {
      # pmm by "prob" and for no pmm
      obj.type <- "binary:logistic"
    }

    dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
    dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
    evals <- list(train = dobs)

    xgb.params$objective <- "binary:logistic"
    xgb.params$eval_metric <- "logloss"
    xgb.params$num_class <- NULL

    fit <- xgb.train(
      data = dobs, evals = evals,
      params = xgb.params, nrounds = nrounds,
      early_stopping_rounds = early_stopping_rounds,
      print_every_n = print_every_n, verbose = verbose
    )

    yhatmis <- predict(fit, dmis)

    # Convert probability to class if PMM not used
    if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
      yhatmis <- ifelse(yhatmis >= 0.5, T, F)
    } else {
      if (pmm.type == 1 && !is.null(yhatobs)) {
        # yhatobs passed - use outside yhatobs.list
      } else {
        # for pmm.type=0 or 2
        yhatobs <- predict(fit, dobs)
      }
      yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = original_labels, k = pmm.k)
    }
  }

  if (save) {
    list(yhatmis = yhatmis, fit = fit)
  } else {
    yhatmis
  }
}


impute_multiclass <- function(save, obs.data, mis.data, obs.y, xgb.params,
                              nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
                              pmm.type, pmm.k, yhatobs = NULL, original_levels) {
  original_labels <- obs.y
  obs.y <- as.integer(obs.y) - 1


  dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
  dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
  evals <- list(train = dobs)

  if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
    obj.type <- "multi:softmax"
  } else {
    obj.type <- "multi:softprob"
  }

  # N.class <- length(levels(obs.y))
  N.class <- length(levels(original_labels))
  xgb.params$objective <- obj.type
  xgb.params$eval_metric <- "mlogloss"
  xgb.params$num_class <- N.class

  fit <- xgb.train(
    data = dobs, evals = evals,
    params = xgb.params, nrounds = nrounds,
    early_stopping_rounds = early_stopping_rounds,
    print_every_n = print_every_n, verbose = verbose
  )

  yhatmis <- predict(fit, dmis)

  if (obj.type == "multi:softmax") {
    yhatmis <- original_levels[yhatmis + 1]
  } else {
    if (pmm.type == 1 && !is.null(yhatobs)) {
      # yhatobs passed - use outside yhatobs.list
    } else {
      # for pmm.type=0 or 2
      yhatobs <- predict(fit, dobs)
    }
    yhatmis <- pmm.multiclass(yhatobs = yhatobs, yhatmis = yhatmis, yobs = original_labels, k = pmm.k)
    yhatmis <- original_levels[yhatmis]
  }

  if (save) {
    list(yhatmis = yhatmis, fit = fit)
  } else {
    yhatmis
  }
}
