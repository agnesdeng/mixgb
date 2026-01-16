#' Show multiply imputed values for a single variable
#' @description Show m sets of imputed values for a specified variable.
#' @param data The original data with missing data.
#' @param imp_list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer.
#' @param x The name of a variable of interest.
#' @param true_values A vector of the true values (if known) of the missing values. In general, this is unknown.
#' @return A data.table with \code{m} columns, each column represents the imputed values of all missing entries in the specified variable. If \code{true_values} is provided, the last column will be the true values of the missing values.
#' @export
#' @examples
#' #obtain m multiply datasets
#' library(mixgb)
#' imp_list<- mixgb(data = nhanes3, m = 3)
#'
#' imp_head <- show_var(imp_list = imp_list, x = "head_circumference_cm",
#'   data = nhanes3)
show_var <- function(data, imp_list, x, true_values = NULL) {
  na.idx <- which(is.na(data[[x]]))
  result.l <- lapply(imp_list, function(dt) dt[[x]][na.idx])
  names(result.l) <- paste("imp", 1:length(result.l))
  result.df <- do.call(cbind.data.frame, result.l)
  result.dt <- as.data.table(result.df)
  if (!is.null(true_values)) {
    result.dt$true <- true_values
  }
  result.dt
}
