#' Create missing value for a dataset
#' @param data a dataframe
#' @param p the proportion of missing values in the dataframe
#' @param seed random seed, a singlue value
#' @export
createNA <- function(data,names=NULL,p=0.3,seed=NULL){
  Nrow=nrow(data)
  Ncol=ncol(data)

  if(!is.null(seed)){
    set.seed(seed)
  }

  if(is.null(names)){

    if(length(p)==1){
      total<-Nrow*Ncol
      NAloc <- rep(FALSE, total)
      NAloc[sample(total, floor(total * p))] <- TRUE
      data[matrix(NAloc, nrow = Nrow, ncol = Ncol)] <- NA
    }else{
      for(i in 1:length(p)){
        data[,i][sample(Nrow,round(p[i]*Nrow))]<-NA
      }
    }
    return(data)

  }else{
    Names<-colnames(data)
    idx<-match(names,Names)
    k<-length(idx)
    for(i in 1:k){
      data[,idx[i]][sample(Nrow,round(p[i]*Nrow))]<-NA
    }
    return(data)
  }

}



