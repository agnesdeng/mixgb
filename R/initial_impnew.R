# Initially impute missing values in the new data using information of a previously trained imputer object.
initial_impnew <- function(initial.newdata = FALSE, new.sorted, traindata, sorted.types, sorted.names, initial.num = "normal", initial.int = "mode", initial.fac = "mode") {
  # @param initial.newdata Whether or not to use the information of the new data to initially impute new data (mean,median,sd etc). Default: FALSE (use training set information instead)
  # @param new.sorted A data.table (with missing values NA's). Must be of the same order as sorted training data.
  # @param traindata A data.table
  # @param initial.num Initial imputation method for numeric type data ("normal","mean","median","mode","sample"). Default: "normal".
  # @param initial.int Initial imputation method for integer type data ("mode","sample"). Default: "mode".
  # @param initial.fac Initial imputation method for factor type data ("mode","sample"). Default: "mode".
  # @return A list of objects that will be used for imputation later.


  # newdata has the same order as new.sorted in traindata
  if (!(is.data.frame(new.sorted) || is.matrix(new.sorted))) {
    stop("Data need to be a data frame, data table or a matrix.")
  }

  if (!is.data.table(new.sorted)) {
    new.sorted <- as.data.table(new.sorted)
  }

  Ncol <- ncol(new.sorted)
  if (Ncol < 2) {
    stop("Data need to have a least two columns.")
  }
  Nrow <- nrow(new.sorted)


  sorted.naSums <- colSums(is.na(new.sorted))
  missing.idx <- which(sorted.naSums != 0)
  missing.vars <- sorted.names[missing.idx]
  missing.types <- sorted.types[missing.idx]
  missing.method <- ifelse(missing.types == "numeric", initial.num,
    ifelse(missing.types == "integer", initial.int, initial.fac)
  )


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
    na.idx <- which(is.na(new.sorted[[var]]))
    Na.idx[[var]] <- na.idx

    if (initial.newdata == FALSE) {
      if (missing.method[[var]] == "normal") {
        # only works for numeric
        new.sorted[[var]] <- imp.normal2(vec = new.sorted[[var]], na.idx = na.idx, traindata.vec = traindata[[var]])
      } else if (missing.method[[var]] == "mean") {
        # only works for numeric
        new.sorted[[var]] <- imp.mean2(vec = new.sorted[[var]], na.idx = na.idx, traindata.vec = traindata[[var]])
      } else if (missing.method[[var]] == "median") {
        # only works for numeric
        new.sorted[[var]] <- imp.median2(vec = new.sorted[[var]], na.idx = na.idx, traindata.vec = traindata[[var]])
      } else if (missing.method[[var]] == "mode") {
        # work for both numeric (only recommend for integer type) and factor
        new.sorted[[var]] <- imp.mode2(vec = new.sorted[[var]], na.idx = na.idx, traindata.vec = traindata[[var]])
      } else if (missing.method[[var]] == "sample") {
        # work for both numeric (only recommend for integer type) and factor
        new.sorted[[var]] <- imp.sample2(vec = new.sorted[[var]], na.idx = na.idx, traindata.vec = traindata[[var]])
      } else {
        stop("Please specify an acceptable initial imputation method.")
      } # To do: include initial imputation using models
    } else {
      # if initial.newdata=TRUE, do initial imputation using information from newdata
      if (missing.method[[var]] == "normal") {
        # only works for numeric
        new.sorted[[var]] <- imp.normal(vec = new.sorted[[var]], na.idx = na.idx)
      } else if (missing.method[[var]] == "mean") {
        # only works for numeric
        new.sorted[[var]] <- imp.mean(vec = new.sorted[[var]], na.idx = na.idx)
      } else if (missing.method[[var]] == "median") {
        # only works for numeric
        new.sorted[[var]] <- imp.median(vec = new.sorted[[var]], na.idx = na.idx)
      } else if (missing.method[[var]] == "mode") {
        # work for both numeric (only recommend for integer type) and factor
        new.sorted[[var]] <- imp.mode(vec = new.sorted[[var]], na.idx = na.idx)
      } else if (missing.method[[var]] == "sample") {
        # work for both numeric (only recommend for integer type) and factor
        new.sorted[[var]] <- imp.sample(vec = new.sorted[[var]], na.idx = na.idx)
      } else {
        stop("Please specify an acceptable initial imputation method.")
      } # To do: include initial imputation using models
    }
  }


  return(list("sorted.dt" = new.sorted, "missing.vars" = missing.vars, "missing.types" = missing.types, "Na.idx" = Na.idx, "sorted.names" = sorted.names))
}




# method ------------------------------------------------------------------
# Impute the missing values of a vector with sampled observed values
imp.sample2 <- function(vec, na.idx = NULL, traindata.vec) {
  # @param vec A vector of numeric or factor values in the newdata set
  # @param na.idx Indices of missing values
  # @param traindata.vec The corresponding vector in the train set
  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  n.na <- length(na.idx)

  trainobs.idx <- which(!is.na(traindata.vec))

  if (length(trainobs.idx) == 1) {
    # if the column in training data only has one observed value, use this value to impute all missing values.
    vec[na.idx] <- rep(traindata.vec[trainobs.idx], n.na)
  } else {
    # otherwise, impute NAs with sampled values
    vec[na.idx] <- sample(traindata.vec[trainobs.idx], n.na, replace = TRUE)
  }
  vec
}

# Impute the missing values of a vector with randomly selected values from a normal distribution with mean and sd extracted from observed values
# @param vec A vector of numeric values
# @param na.idx Indices of missing values
# @param traindata.vec The corresponding vector in the train set

imp.normal2 <- function(vec, na.idx = NULL, traindata.vec) {
  if (!is.numeric(vec)) {
    stop("imp.normal(vec,...) only applies to a numeric vector")
  }

  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  var.mean <- mean(traindata.vec, na.rm = TRUE)

  n.na <- length(na.idx)

  trainobs.idx <- which(!is.na(traindata.vec))
  if (length(trainobs.idx) == 1) {
    # if the column in the training data only has one observed value, use this value to impute all missing values.
    vec[na.idx] <- rep(traindata.vec[trainobs.idx], n.na)
  } else {
    # otherwise, impute NAs with sampled values from a normal distribution
    var.sd <- sd(traindata.vec, na.rm = TRUE)
    vec[na.idx] <- stats::rnorm(n = n.na, mean = var.mean, sd = var.sd)
  }
  vec
}


# Impute the missing values of a vector with the mean of observed values
imp.mean2 <- function(vec, na.idx = NULL, traindata.vec) {
  # @param vec A vector of numeric values
  # @param na.idx Indices of missing values
  # @param traindata.vec The corresponding vector in the train set

  if (!is.numeric(vec)) {
    stop("imp.mean(vec,...) only applies to a numeric vector")
  }

  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  var.mean <- mean(traindata.vec, na.rm = TRUE)

  n.na <- length(na.idx)
  vec[na.idx] <- rep(var.mean, n.na)
  vec
}


# Impute the missing values of a vector with the median of observed values
# @param vec A vector of numeric values
# @param na.idx Indices of missing values
# @param traindata.vec The corresponding vector in the train set
imp.median2 <- function(vec, na.idx = NULL, traindata.vec) {
  if (!is.numeric(vec)) {
    stop("imp.median(vec,...) only applies to a numeric vector")
  }

  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }

  var.median <- stats::median(traindata.vec, na.rm = TRUE)

  n.na <- length(na.idx)
  vec[na.idx] <- rep(var.median, n.na)
  vec
}

# Impute the missing values of a vector with the mode (majority class) of observed values
imp.mode2 <- function(vec, na.idx = NULL, traindata.vec) {
  # @param vec A vector of numeric values (ideally integer type) or factor
  # @param na.idx Indices of missing values
  # @param traindata.vec The corresponding vector in the train set

  if (is.null(na.idx)) {
    na.idx <- which(is.na(vec))
  }

  if (length(na.idx) == 0) {
    stop("This vector contains no missing value.")
  }


  unique.values <- unique(na.omit(traindata.vec))
  tab <- tabulate(match(traindata.vec, unique.values))
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
