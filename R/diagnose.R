#' Show the imputed values of missing entries in a variable
#' @description Show m sets of imputed values for a specified variable.
#' @param imputation.list A list of \code{m} imputed datasets returned by the \code{mixgb} imputer
#' @param var.name The name of a variable of interest
#' @param original.data The original data with missing data
#' @param true.values A vector of the true values (if known) of the missing values. In general, this is unknown.
#' @return A data.table with \code{m} columns, each column represents the imputed values of all missing entries in the specified variable. If \code{true.values} is provided, the last column will be the true values of the missing values.
#' @examples
#' MIXGB <- Mixgb$new(data = nhanes3_newborn)
#' imputed.data <- MIXGB$impute(m = 5)
#' imputed.BMPHEAD <- imputed_var(imputation.list = imputed.data, var.name = "BMPHEAD",
#'                                original.data = nhanes3_newborn)
#' imputed.BMPHEAD
#' @export
imputed_var<- function(imputation.list, var.name, original.data,true.values=NULL) {
  na.idx <- which(is.na(original.data[[var.name]]))
  result.l <- lapply(imputation.list, function(dt) dt[[var.name]][na.idx])
  names(result.l) <- paste("m", 1:length(result.l), sep = "")
  result.df <- do.call(cbind.data.frame, result.l)
  result.dt<-as.data.table(result.df)
  if(!is.null(true.values)){
    result.dt$true<-true.values
  }
  result.dt
}


