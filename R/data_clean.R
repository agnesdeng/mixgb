#' Data cleaning
#' @description  Check some common errors of a raw dataset and return a suitable dataset to be fed into the imputer. Note that this function is just a preliminary check. It will not guarantee the output dataset is fully cleaned.
#' @param  rawdata A data frame.
#' @param  levels.tol Tolerant proportion of the number of levels to the number of observations in a multiclass variable. Default: 0.2
#' @return A preliminary cleaned dataset
#' @export
#' @examples
#' rawdata <- nhanes3
#'
#' rawdata[4, 4] <- NaN
#' rawdata[5, 5] <- Inf
#' rawdata[6, 6] <- -Inf
#'
#' cleandata <- data_clean(rawdata = rawdata)
data_clean <- function(rawdata, levels.tol = 0.2) {
  names <- colnames(rawdata)

  if (!is.data.frame(rawdata)) {
    warning("rawdata is not a data frame. It is now converted to a data frame instead.")
    rawdata <- as.data.frame(rawdata)
  }


  # location of NaN in numeric columns only
  nan.loc <- do.call(cbind, lapply(rawdata, is.nan))
  if (any(nan.loc)) {
    col.idx <- which(nan.loc, arr.ind = T)[, 2]
    nan.col <- names[unique(col.idx)]
    msg1 <- paste("There exists at least one entry coded as NaN in the following numeric variable(s): ", paste(nan.col, collapse = ";"),
      ".",
      sep = ""
    )
    msg2 <- paste("It is now coverted to NA instead.")
    warning(paste(msg1, msg2, sep = "\n"))
    # replace NaN in numeric columns with NA
    rawdata[nan.loc] <- NA
  }


  if (any(rawdata == "NaN", na.rm = TRUE)) {
    col.idx <- which(rawdata == "NaN", arr.ind = T)[, 2]
    nan.col <- names[unique(col.idx)]
    msg1 <- paste("There exists at least one entry coded as NaN in the following factor or character variable(s): ", paste(nan.col, collapse = ";"),
      ".",
      sep = ""
    )
    msg2 <- paste("It is now coverted to NA instead.")
    warning(paste(msg1, msg2, sep = "\n"))
    # replace NaN in charater/factor columns with NA
    rawdata[rawdata == "NaN"] <- NA
    # drop NaN level from factor variables
    rawdata <- droplevels(rawdata)
  }


  inf.loc <- do.call(cbind, lapply(rawdata, is.infinite))
  if (any(inf.loc)) {
    col.idx <- which(inf.loc, arr.ind = T)[, 2]
    inf.col <- names[unique(col.idx)]
    msg1 <- paste("There exists at least one entry coded as Inf or -Inf in the following variable(s): ", paste(inf.col, collapse = ";"),
      ".",
      sep = ""
    )
    msg2 <- paste("It is now coverted to NA instead.")
    warning(paste(msg1, msg2, sep = "\n"))
    # replace Inf or -Inf in numeric columns with NA
    rawdata[inf.loc] <- NA
  }


  if (any(rawdata == "Inf", na.rm = TRUE) | any(rawdata == "-Inf", na.rm = TRUE)) {
    col.idx <- c(which(rawdata == "Inf", arr.ind = T)[, 2], which(rawdata == "-Inf", arr.ind = T)[, 2])
    inf.col <- names[unique(col.idx)]
    msg1 <- paste("There exists at least one entry coded as Inf or -Inf in the following factor or character variable(s): ", paste(inf.col, collapse = ";"),
      ".",
      sep = ""
    )
    msg2 <- paste("It is now coverted to NA instead.")
    warning(paste(msg1, msg2, sep = "\n"))
    # replace Inf or -Inf in charater/factor columns with NA
    rawdata[rawdata == "Inf"] <- NA
    rawdata[rawdata == "-Inf"] <- NA
    # drop NaN level from factor variables
    rawdata <- droplevels(rawdata)
  }

  if (any(rawdata == "", na.rm = TRUE)) {
    col.idx <- which(rawdata == "", arr.ind = T)[, 2]
    empty.col <- names[unique(col.idx)]
    msg1 <- paste("There exists at least one entry coded as empty cell \"\" in the following variable(s): ", paste(empty.col, collapse = ";"),
      ".",
      sep = ""
    )
    msg2 <- paste("Empty cells are now coverted to NA instead.")
    warning(paste(msg1, msg2, sep = "\n"))
    # replace NaN in charater/factor columns with NA
    rawdata[rawdata == ""] <- NA
    # drop NaN level from factor variables
    rawdata <- droplevels(rawdata)
  }

  var.type <- sapply(rawdata, class)
  if (any(var.type == "character")) {
    idx <- which(var.type == "character")
    chr.col <- names[idx]
    if (length(chr.col) == 1) {
      msg1 <- paste("The following variable is of character type: ", paste(chr.col),
        ".",
        sep = ""
      )
      msg2 <- paste("It is now converted to factor instead.")
    } else {
      msg1 <- paste("The following variables are of character type: ", paste(chr.col, collapse = ";"),
        ".",
        sep = ""
      )
      msg2 <- paste("They are now converted to factor instead.")
    }


    warning(paste(msg1, msg2, sep = "\n"))
    rawdata <- as.data.frame(unclass(rawdata), stringsAsFactors = TRUE)
  }


  idx <- which(sapply(rawdata, nlevels) == 1)
  if (length(idx) >= 1) {
    single.col <- names[idx]
    msg1 <- paste("Factor variable(s) with only one level: ", paste(single.col, collapse = ";"),
      ".",
      sep = ""
    )
    msg2 <- paste("A factor must has at least two levels. Any factor with only one level is now removed from the dataset. ")
    warning(paste(msg1, msg2, sep = "\n"))
    rawdata <- rawdata[, -idx]
  }

  num.levels <- sapply(rawdata, nlevels)
  num.row <- nrow(rawdata)
  num.tol <- round(num.row * levels.tol)
  idx <- which(num.levels > num.tol)
  if (length(idx) > 0) {
    toomany.col <- names[idx]
    msg1 <- paste("The number of observations in this dataset is ", paste(num.row, ".", sep = ""), sep = "")
    msg2 <- paste("However, some factor variables have more than ", paste(num.tol, "levels."), sep = "")
    msg3 <- paste("Please check the following variables and consider removing them from the dataset: ", paste(toomany.col, collapse = ";"),
      ".",
      sep = ""
    )
    warning(paste(msg1, msg2, msg3, sep = "\n"))
  }

  rawdata
}
