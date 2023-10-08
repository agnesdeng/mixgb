#' \pkg{mixgb}: Multiple Imputation Through XGBoost
#' @name mixgb-package
#' @docType package
#' @description Multiple imputation using 'XGBoost', subsampling, and predictive mean matching as described in Deng and Lumley (2023) <arXiv:2106.01574>. Our method utilizes the capabilities of XGBoost, a highly efficient implementation of gradient boosted trees, to capture interactions and non-linear relations automatically. Moreover, we have integrated subsampling and predictive mean matching to minimize bias and reflect appropriate imputation variability. This package supports various types of variables and offers flexible settings for subsampling and predictive mean matching. Additionally, it includes diagnostic tools for evaluating the quality of the imputed values.
#' @import data.table
#' @importFrom Matrix sparse.model.matrix
#' @importFrom xgboost xgboost xgb.cv xgb.save xgb.load
#' @importFrom stats median rnorm sd complete.cases na.omit reformulate predict quantile rbinom
#' @references
#' Deng, Y., & Lumley, T. (2023), Multiple Imputation Through XGBoost, Journal of Computational and Graphical Statistics, DOI: 10.1080/10618600.2023.2252501.
#'
#' Chen, T., & Guestrin, C. (2016), XGBoost: A Scalable Tree Boosting System,
#' Proceedings of the 22nd ACM SIGKDD International Conference on Knowledge Discovery and Data Mining (pp. 785-794).
#'
#' van Buuren, S., Brand, J. P., Groothuis-Oudshoorn, C. G., & Rubin, D. B. (2006), Fully Conditional Specification in
#' Multivariate Imputation, Journal of Statistical Computation and Simulation, 76(12), 1049-1064.
#'
#' van Buuren, S. (2018), Flexible Imputation of Missing Data, Second Edition,
#' Chapman & Hall/CRC. Boca Raton, FL.
#'
#' Rubin, D. B. (1986), Statistical Matching Using File Concatenation With Adjusted Weights and Multiple Imputations,
#' Journal of Business & Economic Statistics, 4(1), 87.
#
#' Little, R. J. (1988), Missing-data Adjustments in Large Surveys,
#' Journal of Business & Economic Statistics, 6(3), 287.
#'
NULL
