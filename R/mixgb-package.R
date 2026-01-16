#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @aliases mixgb-package
#' @import data.table
#' @importFrom Matrix sparse.model.matrix fac2sparse fac2Sparse t cbind2
#' @importFrom stats median rnorm sd complete.cases na.omit reformulate predict quantile rbinom
#' @importFrom xgboost xgboost xgb.train xgb.cv xgb.save xgb.load xgb.DMatrix
#' @useDynLib mixgb
#' @importFrom Rcpp sourceCpp
#' @importFrom utils packageVersion askYesNo
#' @references
#' Deng, Y., & Lumley, T. (2024), Multiple Imputation Through XGBoost, Journal of Computational and Graphical Statistics, DOI: 10.1080/10618600.2023.2252501.
#'
#' Chen, T., & Guestrin, C. (2016), XGBoost: A Scalable Tree Boosting System,
#' Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (pp. 785-794).
#'
## usethis namespace: end
NULL
