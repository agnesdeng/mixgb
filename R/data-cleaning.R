#'This function is used to check some common errors of a raw dataset and return a suitable dataset for feeding in the imputer later on. Note that this would not guarantee the dataset is fully cleaned.
#' @param  rawdata A data frame. (a raw dataset)
#' @export


data_clean<-function(rawdata){

  names<-colnames(rawdata)

  if(!is.data.frame(rawdata)){
    warning("rawdata is not a data frame. It is now converted to a data frame instead.")
    rawdata=as.data.frame(rawdata)
  }


  #location of NaN in numeric columns only
  nan.loc<-do.call(cbind,lapply(rawdata,is.nan))
  if(any(nan.loc)){
    col.idx=which(nan.loc,arr.ind = T)[,2]
    nan.col=names[col.idx]
    msg1=paste("There exists at least one entry coded as NaN in the following numeric variable(s): ", paste(nan.col,collapse=";"),
               ".",sep="")
    msg2=paste("It is now coverted to NA instead.")
    warning(paste(msg1,msg2,sep="\n"))
    #replace NaN in numeric columns with NA
    rawdata[nan.loc]<-NA
  }


  if(any(rawdata=="NaN",na.rm=TRUE)){
    col.idx=which(rawdata=="NaN",arr.ind=T)[,2]
    nan.col=names[col.idx]
    msg1=paste("There exists at least one entry coded as NaN in the following factor or character variable(s): ", paste(nan.col,collapse=";"),
               ".",sep="")
    msg2=paste("It is now coverted to NA instead.")
    warning(paste(msg1,msg2,sep="\n"))
    #replace NaN in charater/factor columns with NA
    rawdata[rawdata=="NaN"]<-NA
    #drop NaN level from factor variables
    rawdata=droplevels(rawdata)
  }


  inf.loc<-do.call(cbind,lapply(rawdata,is.infinite))
  if(any(inf.loc)){
    col.idx=which(inf.loc,arr.ind = T)[,2]
    inf.col=names[col.idx]
    msg1=paste("There exists at least one entry coded as Inf or -Inf in the following variable(s): ", paste(inf.col,collapse=";"),
              ".",sep="")
    msg2=paste("It is now coverted to NA instead.")
    warning(paste(msg1,msg2,sep="\n"))
    #replace Inf or -Inf in numeric columns with NA
    rawdata[inf.loc]<-NA
  }


  if(any(rawdata=="Inf",na.rm=TRUE) | any(rawdata=="-Inf",na.rm=TRUE)){
    col.idx=c(which(rawdata=="Inf",arr.ind=T)[,2],which(rawdata=="-Inf",arr.ind=T)[,2])
    inf.col=names[col.idx]
    msg1=paste("There exists at least one entry coded as Inf or -Inf in the following factor or character variable(s): ", paste(inf.col,collapse=";"),
               ".",sep="")
    msg2=paste("It is now coverted to NA instead.")
    warning(paste(msg1,msg2,sep="\n"))
    #replace Inf or -Inf in charater/factor columns with NA
    rawdata[rawdata=="Inf"]<-NA
    rawdata[rawdata=="-Inf"]<-NA
    #drop NaN level from factor variables
    rawdata=droplevels(rawdata)
  }


  var.type<-sapply(rawdata,class)
  if(any(var.type=="character")){
    idx=which(var.type=="character")
    chr.col=names[idx]
    if(length(chr.col)==1){
      msg1=paste("The following variable is of character type: ", paste(chr.col),
                 ".",sep="")
      msg2=paste("It is now converted to factor instead.")
    }else{
      msg1=paste("The following variables are of character type: ", paste(chr.col,collapse=";"),
                 ".",sep="")
      msg2=paste("They are now converted to factor instead.")
    }


    warning(paste(msg1,msg2,sep="\n"))
    rawdata<-as.data.frame(unclass(rawdata),stringsAsFactors = TRUE)
  }


  idx<-which(sapply(rawdata, nlevels) == 1)
  if(length(idx)>=1){
    single.col=names[idx]
    msg1=paste("Factor variable(s) with only one level: ", paste(single.col,collapse=";"),
               ".",sep="")
    msg2=paste("A factor must has at least two levels. Any factor with only one level is now removed from the dataset. ")
    warning(paste(msg1,msg2,sep="\n"))
    rawdata<-rawdata[, -idx]
  }

  rawdata

}




