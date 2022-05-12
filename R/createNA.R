#' Create missing values for a dataset
#' @description This function creates missing values under the missing complete at random mechanism (MCAR). It is for demonstration purposes only.
#' @param data A complete data frame.
#' @param names The names of variables where missing values will be generated.
#' @param p The proportion of missing values in the data frame or the proportions of missing values corresponding to the variables specified in \code{names}.
#' @param seed An integer value. Default: \code{NULL}
#' @return A data frame with artificial missing values
#' @export
#' @examples
#' # Create 30% MCAR data across all variables in a dataset
#' withNA.df <- createNA(iris, p = 0.3)
#'
#' # Create 30% MCAR data in a specified variable in a dataset
#' withNA.df <- createNA(iris, names = c("Sepal.Length"), p = 0.3)
#'
#' # Create MCAR data in several specified variables in a dataset
#' withNA.df <- createNA(iris, names = c("Sepal.Length", "Petal.Width", "Species"),
#'                       p = c(0.3, 0.2, 0.1))
createNA <- function(data, names = NULL, p = 0.3, seed = NULL) {
  Nrow <- nrow(data)
  Ncol <- ncol(data)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  if (is.null(names)) {
    if (length(p) == 1) {
      total <- Nrow * Ncol
      NAloc <- rep(FALSE, total)
      NAloc[sample(total, floor(total * p))] <- TRUE
      data[matrix(NAloc, nrow = Nrow, ncol = Ncol)] <- NA
    } else {
      for (i in 1:length(p)) {
        data[, i][sample(Nrow, round(p[i] * Nrow))] <- NA
      }
    }
    return(data)
  } else {
    Names <- colnames(data)
    idx <- match(names, Names)
    k <- length(idx)
    for (i in 1:k) {
      data[, idx[i]][sample(Nrow, round(p[i] * Nrow))] <- NA
    }
    return(data)
  }
}
