#' \pkg{mixgb}: Multiple Imputation Through XGBoost
#' @name mixgb-package
#' @docType package
#' @description Multiple imputation using 'XGBoost', subsampling, and predictive mean matching as described in Deng and Lumley (2023) <arXiv:2106.01574>. Our method utilizes the capabilities of XGBoost, a highly efficient implementation of gradient boosted trees, to capture interactions and non-linear relations automatically. Moreover, we have integrated subsampling and predictive mean matching to minimize bias and reflect appropriate imputation variability. This package supports various types of variables and offers flexible settings for subsampling and predictive mean matching. Additionally, it includes diagnostic tools for evaluating the quality of the imputed values.
#' @import data.table
#' @importFrom Matrix sparse.model.matrix
#' @importFrom xgboost xgboost xgb.cv
#' @importFrom stats median rnorm sd complete.cases na.omit reformulate predict quantile rbinom
#' @references
#' Deng, Y., & Lumley, T. (2023). Multiple Imputation Through XGBoost.
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
NULL
