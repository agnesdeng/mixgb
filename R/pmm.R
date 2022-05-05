#' PMM for numeric or binary variable
#' @importFrom mice matchindex
#' @export
pmm <- function(yhatobs, yhatmis, yobs, k = pmm.k) {
  # idx=.Call('_mice_matcher', PACKAGE = 'mice', yhatobs, yhatmis, k)
  idx <- mice::matchindex(d = yhatobs, t = yhatmis, k = k)
  yobs[idx]
}


#' PMM for multiclass variable
#' @importFrom  Rfast knn
#' @export
pmm.multiclass <- function(donor.pred, target.pred, donor.obs, k = pmm.k) {
  # shuffle donors to break ties
  donor.size <- length(donor.obs)
  idx <- sample(donor.size, replace = F)
  donor.randompred <- donor.pred[idx, ]
  donor.randomobs <- donor.obs[idx]
  # matching
  match.class <- Rfast::knn(xnew = target.pred, y = donor.randomobs, x = donor.randompred, k = k)
  as.vector(match.class)
}
