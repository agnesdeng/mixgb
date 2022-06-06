#' Impute new data with a saved \code{mixgb} imputer object
#' @param  object A saved imputer object created by \code{mixgb(..., save.models = TRUE)}
#' @param  newdata A data.frame or data.table. New data with missing values.
#' @param  initial.newdata Whether to use the information of the new data to initially impute new data. By default, this is set to \code{FALSE}, the original data passed to \code{MIXGB$new()} will be used for initial imputation.
#' @param  pmm.k The number of donors for predictive mean matching. If \code{NULL} (the default), the \code{pmm.k} value in the saved imputer object will be used.
#' @param  m The number of imputed datasets. If \code{NULL} (the default), the \code{m} value in the saved imputer object will be used.
#' @param  verbose Verbose setting for mixgb. If \code{TRUE}, will print out the progress of imputation. Default: \code{FALSE}.
#' @return A list of \code{m} imputed datasets for new data.
#' @export
#' @examples
#' set.seed(2022)
#' n <- nrow(nhanes3)
#' idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
#' train.data <- nhanes3[idx, ]
#' test.data <- nhanes3[-idx, ]
#'
#' mixgb.obj <- mixgb(data = train.data, m = 2, save.models = TRUE)
#'
#' #obtain m imputed datasets for train.data
#' train.imputed <- mixgb.obj$imputed.data
#' train.imputed
#'
#' #use the saved imputer to impute new data
#' test.imputed <- impute_new(object = mixgb.obj, newdata = test.data)
impute_new <- function(object, newdata, initial.newdata = FALSE, pmm.k = NULL, m = NULL, verbose = FALSE) {


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

  # sort the newdata according to the sorted order of the training dataset
  if (!is.data.table(newdata)) {
    newdata <- as.data.table(newdata)
  }

  ordinalAsInteger <- params$ordinalAsInteger
  if (ordinalAsInteger == TRUE) {
    ord.fac <- names(Filter(is.ordered, newdata))
    if(length(ord.fac)>0){
      newdata[, c(ord.fac) := lapply(.SD, fac2int), .SDcols = ord.fac]
    }
  }


  # sortedNA.dt: sorted newdata according to the original training dataset
  # sortedNA.dt <- newdata[, ..sorted.names]
  sortedNA.dt <- newdata[, sorted.names, with = FALSE]


  # newdata data structure
  Ncol <- ncol(sortedNA.dt)
  Nrow <- nrow(sortedNA.dt)
  naSums <- colSums(is.na(sortedNA.dt))


  origin.names <- colnames(newdata)


  # extract imputer models from the training object
  XGB.models <- object$XGB.models



  # extract one imputed set from the training object for initial imputation
  if (initial.newdata == FALSE) {
    train.dt <- object$imputed.data[[1]]
    # initial imputation.......................................................................................................
    trainNA.dt <- train.dt
    for (var in omissing.vars) {
      na.idx <- Na.idx[[var]]
      trainNA.dt[[var]][na.idx] <- NA
    }
    # traindata=train.dt (one imputed train set)  or traindata=trainNA.dt (the original train set with NAs)
  } else {
    trainNA.dt <- NULL
  }


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




  initial.obj <- initial_impnew(initial.newdata = initial.newdata, new.sorted = sortedNA.dt, traindata = trainNA.dt, sorted.names = sorted.names, sorted.types = sorted.types, initial.num = initial.num, initial.fac = initial.fac, bootstrap = bootstrap)
  # if initial.newdata=TRUE use newdata information to initially impute newdata
  # if initial.newdata=FALSE use training data information to initially impute newdata


  # After initial imputation of newdata
  missing.vars <- initial.obj$missing.vars
  missing.types <- initial.obj$missing.types
  Na.idx <- initial.obj$Na.idx
  sorted.dt <- initial.obj$sorted.dt

  imputed.data <- vector("list", m)

  if(verbose){
    cat("Imputing new data with mixgb: ", "set")
  }


  for (i in seq_len(m)) {
    if(verbose){
      cat(" --", i)
    }
    # feed in the initial imputed dataset
    sorted.dt <- initial.obj$sorted.dt
    sorted.dt <- mixgb_use(
      m.set = i, xgb.models = XGB.models[[i]], pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt,
      missing.vars = missing.vars, sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol
    )

    imputed.data[[i]] <- sorted.dt[, origin.names, with = FALSE]
  }


  # ...............................................................
  if(verbose){
    cat("\n")
  }

  return(imputed.data)
}
