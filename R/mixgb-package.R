#' \pkg{mixgb}: Multiple Imputation Through XGBoost
#' @name mixgb-package
#' @docType package
#' @description Mixgb offers a scalable solution for imputing large datasets using XGBoost, bootstrapping and predictive mean matching. Mixgb is built under Fully Conditional Specification (FCS), where XGBoost imputation models are built for each incomplete variable. Mixgb can automatically handle different types of variables and users do not need to encode categorical variables themselves. Users can also choose different settings regarding bootstrapping and predictive mean matching to enhance imputation performance.
#' @import data.table
#' @importFrom Matrix sparse.model.matrix
#' @importFrom xgboost xgboost xgb.cv
#' @importFrom stats median rnorm sd complete.cases na.omit reformulate predict quantile rbinom
#' @references
#' Deng, Y., & Lumley, T. (2021). Multiple Imputation Through XGBoost.
#' arXiv:2106.01574.
#'
#' Chen, T., & Guestrin, C. (2016, August). XGBoost: A Scalable Tree Boosting System.
#' Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (pp. 785-794).
#'
#' van Buuren, S., Brand, J. P., Groothuis-Oudshoorn, C. G., & Rubin, D. B. (2006). Fully conditional specification in
#' multivariate imputation. Journal of Statistical Computation and Simulation, 76(12), 1049-1064.
#'
#' van Buuren, S. (2018). Flexible Imputation of Missing Data. Second Edition.
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Rubin, D. B. (1986). Statistical matching using file concatenation with adjusted weights and multiple imputations.
#' Journal of Business & Economic Statistics, 4(1), 87.
#'
#' Little, R. J. (1988). Missing-data adjustments in large surveys.
#' Journal of Business & Economic Statistics, 6(3), 287.
#'
#' Efron, B. (1979). Bootstrap methods: Another look at the jackknife.
#' The Annals of Statistics, 7(1).
NULL
