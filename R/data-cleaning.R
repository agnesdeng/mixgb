#'This function is used to check some common errors of a raw dataset and return a suitable dataset for feeding in the imputer later on. Note that this would not guarantee the dataset is fully cleaned.
#' @param  rawdata A data frame. (a raw dataset)
#' @export


data_clean<-function(rawdata){

  if(!is.data.frame(rawdata)){
    warning("rawdata is not a data frame. It is now converted to a data frame instead.")
    rawdata<-as.data.frame(rawdata)
  }


  #location of NaN in numeric columns only
  nan.loc<-do.call(cbind,lapply(rawdata,is.nan))
  if(any(nan.loc)){
    warning("There is at least one entry in some numeric variables of your data coded as NaN. It is now coverted to NA instead")
    #replace NaN in numeric columns with NA
    rawdata[nan.loc]<-NA
  }


  if(any(rawdata=="NaN",na.rm=TRUE)){
    #replace NaN in charater/factor columns with NA
    warning("There is at least one entry in some factor or character variables of your data coded as NaN. It is now coverted to NA instead")
    rawdata[rawdata=="NaN"]<-NA
    rawdata=rawdata
    #drop NaN level from factor variables
    rawdata=droplevels(rawdata)
  }

  inf.loc<-do.call(cbind,lapply(rawdata,is.infinite))
  if(any(inf.loc)){
    warning("There is at least one entry in some numeric variables of your data coded as Inf or -Inf. It is now coverted to NA instead")
    #replace Inf or -Inf in numeric columns with NA
    rawdata[inf.loc]<-NA
  }


  if(any(rawdata=="Inf",na.rm=TRUE) | any(rawdata=="-Inf",na.rm=TRUE)){
    #replace Inf or -Inf in charater/factor columns with NA
    warning("There is at least one entry in some factor or character variables of your data coded as Inf or -Inf. It is now coverted to NA instead")
    rawdata[rawdata=="Inf"]<-NA
    rawdata[rawdata=="-Inf"]<-NA
    #drop NaN level from factor variables
    rawdata=droplevels(rawdata)
  }


  var.type<-sapply(rawdata,class)
  if(any(var.type=="character")){
    warning("There is at least one variable of character type. It is now converted to factor instead" )
    rawdata<-as.data.frame(unclass(rawdata),stringsAsFactors = TRUE)
  }


  idx<-which(sapply(rawdata, nlevels) == 1)
  if(length(idx)>=1){
    warning("There is at least one factor variable with only one level. A factor must has at least two levels. Any factor with only one level is now removed from the dataset. ")
    rawdata<-rawdata[, -idx]
  }

  rawdata

}




