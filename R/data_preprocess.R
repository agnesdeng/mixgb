#' Return the type ("numeric", "integer","binary","multiclass") of each variable in the dataset
#' @param data A data.frame or a data.table
#' @export
feature_type <- function(data) {
  Types <- sapply(data, class)
  if (any(Types == "character")) {
    stop("Data contains variables of character type. Please change them into factor.")
  }
  factor.vars <- which(Types == "factor" | Types == "Ord.factor")
  for (fac in factor.vars) {
    if (length(levels(data[[fac]])) == 2) {
      Types[fac] <- "binary"
    } else {
      Types[fac] <- "multiclass"
    }
  }

  return(Types)
}


#' Sort data by increasing number of missing values
#' @param data A data table (with missing values NA's)
#' @export
sortNA <- function(data) {
  Names <- colnames(data)
  na.loc <- is.na(data)
  sorted.idx <- order(colSums(na.loc))
  sorted.names <- Names[sorted.idx]

  if (is.data.table(data)) {
    # data.table
    sorted.data <- data[, ..sorted.names]
  } else {
    # data.frame
    sorted.data <- data[, sorted.names]
    sorted.data <- as.data.table(sorted.data)
  }

  # setcolorder(data,sorted.names)
  # will change data
  return(list("sorted.dt" = sorted.data, "sorted.idx" = sorted.idx, "sorted.names" = sorted.names))
}



#' This function is used to obtain a list of binary and multiclass variables
#' @param  data A data frame
#' @export
variable_class <- function(data) {
  Types <- feature_type(data)
  Names <- names(data)
  binary <- Names[Types == "binary"]
  multiclass <- Names[Types == "multiclass"]
  return(list("binary" = binary, "multiclass" = multiclass))
}



#' Sort the dataset by increasing number of missing values (use in old version Xgb-imputer0)
#' @param data A data frame (with missing values NA's)
#' @export
sortNA_df <- function(data) {
  na.loc <- is.na(data)
  sorted.idx <- order(colSums(na.loc))
  sorted.df <- data[sorted.idx]
  return(list("sorted.df" = sorted.df, "sorted.idx" = sorted.idx))
}
