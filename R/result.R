#' Use to return a data.table with imputed values of a variable with missing values
#' @param imputation.list a list of m imputed dataset return by mixgb imputer
#' @param var.name the name of a variable of interest
#' @param na.idx the indices of the missing values of this variable
#' @return a data.table with m columns, each columns represent the imputed values of the missing values in this variable
#' @export
imputed_values <- function(imputation.list, var.name, na.idx,true.values=NULL) {
  result.l <- lapply(imputation.list, function(dt) dt[[var.name]][na.idx])
  names(result.l) <- paste("m", 1:length(result.l), sep = "")
  result.df <- do.call(cbind.data.frame, result.l)
  result.dt<-as.data.table(result.df)
  if(!is.null(true.values)){
    result.dt$true<-true.values
  }
  result.dt
}
