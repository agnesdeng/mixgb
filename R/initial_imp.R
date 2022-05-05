#' This function is use to initially impute missing value and validate the feed-in dataset
#' @param data A data table (with missing values NA's)
#' @param initial.num initial imputation method for numeric type data ("normal","mean","median","mode","sample"). Default: "normal"
#' @param initial.fac initial imputation method for factor type data ("mode","sample"). Default: "mode"
#' @param bootstrap whether or not use bootstrap for multiple imputation. If TRUE, also return sortedNA.dt
#' @return A list of items including data table with initial imputation, sorted index, Types, Names etc.
#' @export


initial_imp <- function(data, initial.num = "normal", initial.fac = "mode", bootstrap = TRUE) {
  if (!(is.data.frame(data) || is.matrix(data))) {
    stop("Data need to be a data frame or a matrix.")
  }

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  Ncol <- ncol(data)
  if (Ncol < 2) {
    stop("Data need to have a least two columns.")
  }
  Nrow <- nrow(data)


  ## Data preprocessing
  # 1) sort the dataset with increasing NAs
  origin.names <- colnames(data)
  sort.result <- sortNA(data)
  sorted.dt <- sort.result$sorted.dt
  sorted.idx <- sort.result$sorted.idx
  sorted.names <- sort.result$sorted.names
  sorted.types <- feature_type(sorted.dt)


  # 2)initial imputation & data validation


  sorted.naSums <- colSums(is.na(sorted.dt))
  missing.idx <- which(sorted.naSums != 0)
  missing.vars <- sorted.names[missing.idx]
  missing.types <- sorted.types[missing.idx]
  missing.method <- ifelse(missing.types == "numeric", initial.num, initial.fac)


  if (all(sorted.naSums == 0)) {
    stop("No missing values in this data frame.")
  }

  if (any(sorted.naSums == Nrow)) {
    stop("At least one variable in the data frame has 100% missing values.")
  }



  if (any(sorted.naSums >= 0.9 * Nrow)) {
    warning("Some variables have more than 90% miss entries.")
  }

  mp <- length(missing.vars)
  Obs.idx <- vector("list", mp)
  names(Obs.idx) <- missing.vars
  Na.idx <- vector("list", mp)
  names(Na.idx) <- missing.vars


  for (var in missing.vars) {
    na.idx <- which(is.na(sorted.dt[[var]]))
    Na.idx[[var]] <- na.idx



    if (missing.method[[var]] == "normal") {
      # only works for numeric
      sorted.dt[[var]] <- imp.normal(vec = sorted.dt[[var]], na.idx = na.idx)
    } else if (missing.method[[var]] == "mean") {
      # only works for numeric
      sorted.dt[[var]] <- imp.mean(vec = sorted.dt[[var]], na.idx = na.idx)
    } else if (missing.method[[var]] == "median") {
      # only works for numeric
      sorted.dt[[var]] <- imp.median(vec = sorted.dt[[var]], na.idx = na.idx)
    } else if (missing.method[[var]] == "mode") {
      # work for both numeric (only recommend for integer type) and factor
      sorted.dt[[var]] <- imp.mode(vec = sorted.dt[[var]], na.idx = na.idx)
    } else if (missing.method[[var]] == "sample") {
      # work for both numeric (only recommend for integer type) and factor
      sorted.dt[[var]] <- imp.sample(vec = sorted.dt[[var]], na.idx = na.idx)
    } else {
      stop("Please specify an acceptable initial imputation method.")
    }

    # To do: include initial imputation using models
  }

  if (bootstrap == TRUE) {
    return(list(
      "sortedNA.dt" = sort.result$sorted.dt, "sorted.dt" = sorted.dt, "sorted.idx" = sorted.idx, "sorted.names" = sorted.names, "sorted.types" = sorted.types, "sorted.naSums" = sorted.naSums,
      "origin.names" = origin.names, "Nrow" = Nrow, "Ncol" = Ncol, "mp" = mp,
      "missing.idx" = missing.idx, "missing.vars" = missing.vars, "missing.types" = missing.types, "missing.method" = missing.method,
      "Obs.idx" = Obs.idx, "Na.idx" = Na.idx
    ))
  } else {
    return(list(
      "sorted.dt" = sorted.dt, "sorted.idx" = sorted.idx, "sorted.names" = sorted.names, "sorted.types" = sorted.types, "sorted.naSums" = sorted.naSums,
      "origin.names" = origin.names, "Nrow" = Nrow, "Ncol" = Ncol, "mp" = mp,
      "missing.idx" = missing.idx, "missing.vars" = missing.vars, "missing.types" = missing.types, "missing.method" = missing.method,
      "Obs.idx" = Obs.idx, "Na.idx" = Na.idx
    ))
  }
}




# method ------------------------------------------------------------------
#' impute the missing values of a vector with sampled observed values
#' @param vec a vector of numeric or factor values
#' @param na.idx indices of missing values
#' @export
imp.sample <- function(vec, na.idx = NULL) {
  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  n.na <- length(na.idx)

  obs.idx <- which(!is.na(vec))

  if (length(obs.idx) == 1) {
    # if the column only has one observed value, use this value to impute all missing values.
    vec[na.idx] <- rep(vec[obs.idx], n.na)
  } else {
    # otherwise, impute NAs with sampled values
    vec[na.idx] <- sample(vec[obs.idx], n.na, replace = TRUE)
  }
  vec
}

#' impute the missing values of a vector with randomly selected values from a normal distribution with mean and sd extracted from observed values
#' @param vec a vector of numeric values
#' @param na.idx indices of missing values
#' @export
imp.normal <- function(vec, na.idx = NULL) {
  if (!is.numeric(vec)) {
    stop("imp.normal(vec,...) only applies to a numeric vector")
  }

  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  var.mean <- mean(vec, na.rm = TRUE)
  n.na <- length(na.idx)
  obs.idx <- which(!is.na(vec))
  if (length(obs.idx) == 1) {
    # if the column only has one observed value, use this value to impute all missing values.
    vec[na.idx] <- rep(vec[obs.idx], n.na)
  } else {
    # otherwise, impute NAs with sampled values from a normal distribution
    var.sd <- sd(vec, na.rm = TRUE)
    vec[na.idx] <- rnorm(n = n.na, mean = var.mean, sd = var.sd)
  }
  vec
}


#' impute the missing values of a vector with the mean of observed values
#' @param vec a vector of numeric values
#' @param na.idx indices of missing values
#' @export
imp.mean <- function(vec, obs.idx = NULL, na.idx = NULL) {
  if (!is.numeric(vec)) {
    stop("imp.mean(vec,...) only applies to a numeric vector")
  }

  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  var.mean <- mean(vec, na.rm = TRUE)

  n.na <- length(na.idx)
  vec[na.idx] <- rep(var.mean, n.na)
  vec
}


#' impute the missing values of a vector with the median of observed values
#' @param vec a vector of numeric values
#' @param na.idx indices of missing values
#' @export
imp.median <- function(vec, obs.idx = NULL, na.idx = NULL) {
  if (!is.numeric(vec)) {
    stop("imp.median(vec,...) only applies to a numeric vector")
  }

  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  var.median <- median(vec, na.rm = TRUE)

  n.na <- length(na.idx)
  vec[na.idx] <- rep(var.median, n.na)
  vec
}

#' impute the missing values of a vector with the mode (majority class) of observed values
#' @param vec a vector of numeric values (ideally integer type) or factor
#' @param na.idx indices of missing values
#' @export
imp.mode <- function(vec, obs.idx = NULL, na.idx = NULL) {
  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }


  unique.values <- unique(na.omit(vec))
  tab <- tabulate(match(vec, unique.values))
  var.mode <- unique.values[tab == max(tab)]

  n.na <- length(na.idx)
  if (length(var.mode) == 1) {
    # if mode is unique
    vec[na.idx] <- rep(var.mode, n.na)
  } else {
    # if mode is not unique, impute with randomly sampled modes
    vec[na.idx] <- sample(var.mode, size = n.na, replace = TRUE)
  }
  vec
}
