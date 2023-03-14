#' PMM for numeric or binary variable
#' @param yhatobs The predicted values of observed entries in a variable
#' @param yhatmis The predicted values of missing entries in a variable
#' @param yobs The actual observed values of observed entries in a variable
#' @param k The number of donors.
#' @return The matched observed values of all missing entries
#' @importFrom mice matchindex
#' @keywords internal
pmm <- function(yhatobs, yhatmis, yobs, k) {
  # idx=.Call('_mice_matcher', PACKAGE = 'mice', yhatobs, yhatmis, k)
  idx <- mice::matchindex(d = yhatobs, t = yhatmis, k = k)
  yobs[idx]
}


#' PMM for multiclass variable
#' @param yhatobs The predicted values of observed entries in a variable
#' @param yhatmis The predicted values of missing entries in a variable
#' @param yobs The actual observed values of observed entries in a variable
#' @param k The number of donors.
#' @return The matched observed values of all missing entries
#' @importFrom  Rfast knn
#' @keywords internal
pmm.multiclass <- function(yhatobs, yhatmis, yobs, k) {
  # @param yhatobs The predicted values of observed entries in a variable
  # @param yhatmis The predicted values of missing entries in a variable
  # @param yobs The actual observed values of observed entries in a variable
  # @param k The number of donors.
  # @return The matched observed values of all missing entries
  # @importFrom  Rfast knn

  # shuffle donors to break ties
  donor.size <- length(yobs)
  idx <- sample(donor.size, replace = F)
  random.yhatobs <- yhatobs[idx, ]
  random.yobs <- yobs[idx]
  # matching (return column)
  match.class <- Rfast::knn(xnew = yhatmis, y = random.yobs, x = random.yhatobs, k = k)
  # return a row vector
  as.vector(match.class)
}
