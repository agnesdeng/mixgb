
#'This function is used to detect the type(numeric,binary,multiclass) of each feature
#' @param  data A data frame
#' @export
variable_class<-function(data){
  binary<-NULL
  multiclass<-NULL
  feature<-colnames(data)
  for(i in 1:ncol(data)){
    if(length(levels(data[,i]))==2){
      binary=c(binary,feature[i])
    }else if(length(levels(data[,i]))>2){
      multiclass=c(multiclass,feature[i])
    }
  }
  return(list("binary"=binary,"multiclass"=multiclass))
}



#'Sort the dataset by increasing number of missing values
#'@param data A data frame (with missing values NA's)
#'@export

sortNA<-function(data){
  na.loc=is.na(data)
  sorted.idx<-order(colSums(na.loc))
  sorted.df<-data[sorted.idx]
  return(list("sorted.df"=sorted.df,"sorted.idx"=sorted.idx))
}


#'Return the type of each variable in the dataset
#'@param data A data frame
#'@export

feature_type<-function(data){
  type<-rep("numeric",ncol(data))
  binary<-variable_class(data)$binary
  multiclass<-variable_class(data)$multiclass
  if(is.null(binary)==FALSE){
    binary.index<-which(colnames(data) %in% binary)
    type[binary.index]<-"binary"
  }

  if(is.null(multiclass)==FALSE){
    multiclass.index<-which(colnames(data) %in% multiclass)
    type[multiclass.index]<-"multiclass"
  }
  return(type)
}




