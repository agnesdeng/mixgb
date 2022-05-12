#' \pkg{mixgb}: Multiple Imputation Through XGBoost
#' @name mixgb-package
#' @docType package
#' @description Mixgb is a scalable multiple imputation framework based on XGBoost, bootstrapping and predictive mean matching. By fitting XGBoost models under fully conditional specification, \pkg{mixgb} is able to automatically capture complex data structure such as interactions and non-linearity. We have shown that our framework obtains less biased estimates and reflects appropriate imputation variability, while achieving high computational efficiency.
#' @import xgboost
#' @import Matrix
#' @importFrom R6  R6Class
#' @importFrom Rfast knn
#' @importFrom mice matchindex
#' @importFrom stats median rnorm sd complete.cases na.omit reformulate predict
#' @references
#' Deng, Y., & Lumley, T. (2021). Multiple Imputation Through XGBoost.
#' arXiv preprint arXiv:2106.01574.
#'
#' Chen, T., & Guestrin, C. (2016, August). Xgboost: A scalable tree boosting system.
#' In Proceedings of the 22nd acm sigkdd international conference on knowledge discovery
#'and data mining (pp. 785-794).
#'
#' van Buuren, S., Brand, J.P.L., Groothuis-Oudshoorn C.G.M., Rubin, D.B. (2006)
#' Fully conditional specification in multivariate imputation.  \emph{Journal of
#' Statistical Computation and Simulation}, \bold{76}, 12, 1049--1064.
#'
#' Van Buuren, S. (2018).
#' \href{https://stefvanbuuren.name/fimd/}{\emph{Flexible Imputation of Missing Data. Second Edition.}}
#' Chapman & Hall/CRC. Boca Raton, FL.
NULL
