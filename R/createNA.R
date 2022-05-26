#' Create missing values for a dataset
#' @description This function creates missing values under the missing complete at random mechanism (MCAR). It is for demonstration purposes only.
#' @param data A complete data frame.
#' @param var.names The var.names of variables where missing values will be generated.
#' @param p The proportion of missing values in the data frame or the proportions of missing values corresponding to the variables specified in \code{var.names}.
#' @return A data frame with artificial missing values
#' @export
#' @examples
#' # Create 30% MCAR data across all variables in a dataset
#' withNA.df <- createNA(data = iris, p = 0.3)
#'
#' # Create 30% MCAR data in a specified variable in a dataset
#' withNA.df <- createNA(data = iris, var.names = c("Sepal.Length"), p = 0.3)
#'
#' # Create MCAR data in several specified variables in a dataset
#' withNA.df <- createNA(data = iris,
#'   var.names = c("Sepal.Length", "Petal.Width", "Species"),
#'   p = c(0.3, 0.2, 0.1)
#' )
createNA <- function(data, var.names = NULL, p = 0.3) {
  N.colNA <- colSums(is.na(data))



  Nrow <- nrow(data)
  Ncol <- ncol(data)

  N.p <- length(p)

  if (is.null(var.names)) {
    if (N.p == 1) {
      if (any(N.colNA != 0)) {
        warning("There are missing values in the original dataset. The proportion of missing values in the output data may be larger than the value specified in `p`.\n")
      }
      total <- Nrow * Ncol
      NAloc <- rep(FALSE, total)
      NAloc[sample(total, floor(total * p))] <- TRUE
      data[matrix(NAloc, nrow = Nrow, ncol = Ncol)] <- NA
    } else if (N.p == Ncol) {
      warning("There are missing values in the original dataset. The proportion of missing values in the output data may be larger than the value specified in `p`.\n")
      for (i in 1:Ncol) {
        data[sample(Nrow, round(p[i] * Nrow)), i] <- NA
      }
    } else {
      stop("When `var.names` is not specified, the length of `p` should be either one or ncol(data).")
    }
  } else {
    Names <- colnames(data)
    missing.vars <- Names[N.colNA != 0]
    k <- length(var.names)

    if (N.p == 1) {
      p <- rep(p, k)
    } else if (N.p != k) {
      stop("The length of `p` should be either one or the same as the length of `var.names`.")
    }

    names(p) <- var.names

    for (var in var.names) {
      if (any(missing.vars == var)) {
        warning(paste("Variable", var, "has missing values in the original dataset. The proportion of missing values for this variable in the output data may be larger than the value specified in `p`.\n"))
      }
      data[sample(Nrow, round(p[var] * Nrow)), var] <- NA
    }
  }

  return(data)
}


#create missing data under MAR
marNA<-function(data,missing.var,depend.var,cutoff.probs=c(0,1/3,2/3,1),missing.probs=c(0.2,0.8,0.2)){
  cutoff.values<-quantile(data[[depend.var]],probs=cutoff.probs,na.rm=T)
  depend.groups<-findInterval(data[[depend.var]],vec=cutoff.values,all.inside = TRUE)
  p<-missing.probs[depend.groups]
  miss<-rbinom(n=nrow(data),size = 1,p)
  data[[missing.var]][miss==1]<-NA
  data
}

#plot masked missing data
plot_mar<-function(NAdata, missing.var, depend.var, original.data){
  idx<-which(is.na(NAdata[[missing.var]]))
  original.data$missing<-rep("observed",nrow(original.data))
  original.data$missing[idx]<-"missing"
  ggplot(data=original.data,aes(y=.data[[missing.var]],x=.data[[depend.var]],color=missing))+
    geom_point()+
    facet_grid(~missing)
}

