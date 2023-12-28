# Convert a factor to an integer variable.
fac2int <- function(vec) {
  suppressWarnings(int.levels <- as.integer(levels(vec)))
  if (any(is.na(int.levels))) {
    as.integer(vec)
  } else {
    int.levels[vec]
  }
}

# Classify the type of each variable in a dataset
feature_type <- function(data) {
  # @param data A data.frame or a data.table
  # @return The type (numeric/integer/binary/multiclass) of each variable in a dataset

  Types <- sapply(data, class)

  if (any(Types == "character")) {
    stop("Data contains variables of character type. Please change them into factor.")
  }

  # ordinal.idx<-grep("ordered",Types)
  ord.fac <- names(Filter(is.ordered, data))
  if (length(ord.fac) > 0) {
    Types[ord.fac] <- "factor"
  }

  factor.vars <- which(Types == "factor")
  for (fac in factor.vars) {
    if (length(levels(data[[fac]])) == 2) {
      Types[fac] <- "binary"
    } else {
      Types[fac] <- "multiclass"
    }
  }

  return(Types)
}


feature_type2 <- function(data) {
  # Define type for each column
  Types <- sapply(data, function(var) {
    # Check primary class of the column
    var.class <- class(var)[1]
    switch(var.class,
      numeric = "numeric",
      integer = "integer",
      logical = "logical",
      factor = ifelse(nlevels(var) == 2, "binary", "multiclass"),
      ordered = ifelse(nlevels(var) == 2, "binary", "multiclass"),
      character = stop("Data contains variables of character type. Please change them into factor."),
      stop(paste0("Unsupported data type: ", var.class))
    )
  })

  Types
}



cbind_type <- function(data) {
  # Define type for each column
  Types <- sapply(data, function(var) {
    # Check primary class of the column
    var.class <- class(var)[1]
    }
    )

  Types
}







#' Sort data by increasing number of missing values
#' @import data.table
#' @keywords internal
sortNA <- function(data) {
  # @param data A data table (with missing values NA's)
  # @return A list whose first component is the sorted data, second component is the sorted indices and third component is the sorted variable names according to the amount of missingness.

  Names <- colnames(data)
  na.loc <- is.na(data)
  sorted.idx <- order(colSums(na.loc))
  sorted.names <- Names[sorted.idx]

  if (is.data.table(data)) {
    # data.table
    # sorted.data <- data[, ..sorted.names]
    sorted.data <- data[, sorted.names, with = FALSE]
  } else {
    # data.frame
    sorted.data <- data[, sorted.names]
    sorted.data <- as.data.table(sorted.data)
  }

  # setcolorder(data,sorted.names)
  # will change data
  return(list("sorted.dt" = sorted.data, "sorted.idx" = sorted.idx, "sorted.names" = sorted.names))
}


# Obtain a list of the names of binary and multiclass variables.
variable_class <- function(data) {
  # @param  data A data frame
  # @return A list whose first component is the names of binary variables and the second component is the names of multiclass variables.
  Types <- feature_type(data)
  Names <- names(data)
  binary <- Names[Types == "binary"]
  multiclass <- Names[Types == "multiclass"]
  return(list("binary" = binary, "multiclass" = multiclass))
}
