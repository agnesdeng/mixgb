#' Impute new data using saved imputation models from training data
#' @param  object training imputer object
#' @param  newdata a data.frame or data.table
#' @param  initial.newdata whether or not to use the information of the new data to initially impute new data (mean,median,sd etc). Default: FALSE (use training set information instead)
#' @param  pmm.k the number of donors for predictive mmean matching. Default: NULL
#' @param  m the number of imputed datasets. Default: NULL (m would be set to the same value as the training imputer object)
#' @export



impute_new <- function(object, newdata, initial.newdata = FALSE, pmm.k = NULL, m = NULL) {
  origin.names <- colnames(newdata)


  # extract imputer models from the training object
  XGB.models <- object$XGB.models



  # extract one imputed set from the training object for initial imputation
  if (initial.newdata == FALSE) {
    train.dt <- object$imputed.data[[1]]
  }


  # extract params from the training object
  params <- object$params



  if (is.null(pmm.k)) {
    pmm.k <- params$pmm.k
  } else {
    pmm.k <- pmm.k
  }

  if (is.null(m)) {
    m <- params$m
  } else {
    if (m <= params$m) {
      m <- m
    } else {
      stop("The value of m in impute.new() cannot be larger than the value of m in $impute().")
    }
  }



  # must have...........................................--------------------------------
  # need to use the order of sorted variables in the original training data: otherwise xgboost has errors
  sorted.names <- params$sorted.names
  sorted.types <- params$sorted.types
  # missing.vars in original data
  omissing.vars <- params$missing.vars
  Na.idx <- params$Na.idx
  # vars saved from imputer
  save.vars <- params$save.vars
  # initial imputation methods should be the same as the training imputer
  initial.num <- params$initial.num
  initial.fac <- params$initial.fac
  bootstrap <- params$bootstrap

  # for PMM
  yhatobs.list <- params$yhatobs.list
  yobs.list <- params$yobs.list

  pmm.type <- params$pmm.type
  pmm.link <- params$pmm.link


  # new data variables should be in the same order as the training dataset
  sorted.idx <- object$params$sorted.idx

  ############################### -----------------------------------------------


  # sort the newdata according to the sorted order of the training dataset
  if (!is.data.table(newdata)) {
    newdata <- as.data.table(newdata)
  }
  # sortedNA.dt: sorted newdata according to the original training dataset
  sortedNA.dt <- newdata[, ..sorted.names]


  # newdata data structure
  Ncol <- ncol(sortedNA.dt)
  Nrow <- nrow(sortedNA.dt)
  naSums <- colSums(is.na(sortedNA.dt))


  # check new data and give some warning messages (unfinished)...........................................................................
  if (all(naSums == 0)) {
    stop("No missing values in new data.")
  }

  missing.vars <- names(which(naSums != 0))

  if (!all(missing.vars %in% save.vars)) {
    stop("Some variables in the new data has missing values but their models are not saved. Please re-specify save.vars and re-train the imputer.")
    # more detail information....................................
    unsaved <- missing.vars[which(!missing.vars %in% save.vars)]
    msg1 <- paste("There exists at least one missing value in the following variable(s): ", paste(unsaved, collapse = ";"),
      ".",
      sep = ""
    )
    msg2 <- paste("However, your hadn't specified to save imputation models for these variables.")
    msg3 <- paste("Please either add these variables in the argument save.vars or set save.vars=colnames(data) and re-train the imputer.")
    stop(paste(msg1, msg2, msg3, sep = "\n"))
  }


  # initial imputation.......................................................................................................
  trainNA.dt <- train.dt
  for (var in omissing.vars) {
    na.idx <- Na.idx[[var]]
    trainNA.dt[[var]][na.idx] <- NA
  }
  # traindata=train.dt (one imputed train set)  or traindata=trainNA.dt (the original train set with NAs)

  initial.obj <- initial_impnew(initial.newdata = initial.newdata, new.sorted = sortedNA.dt, traindata = trainNA.dt, sorted.names = sorted.names, sorted.types = sorted.types, initial.num = initial.num, initial.fac = initial.fac, bootstrap = bootstrap)
  # if initial.newdata=TRUE use newdata information to initially impute newdata
  # if initial.newdata=FALSE use training data information to initially impute newdata



  # After initial imputation of newdata
  missing.vars <- initial.obj$missing.vars
  missing.types <- initial.obj$missing.types
  Na.idx <- initial.obj$Na.idx
  sorted.dt <- initial.obj$sorted.dt

  imputed.data <- vector("list", m)


    for (i in seq_len(m)) {
      cat("Imputing new dataset", i, "with mixgb\n")
      # feed in the initial imputed dataset
      sorted.dt <- initial.obj$sorted.dt
      sorted.dt <- mixgb_use(
        m.set = i, xgb.models = XGB.models[[i]], pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt,
        missing.vars = missing.vars, sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol
      )

      imputed.data[[i]] <- sorted.dt[, ..origin.names]
    }


  # ...............................................................



  return(imputed.data)
}
