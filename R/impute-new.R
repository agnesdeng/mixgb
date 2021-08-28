#'This function is used to impute new data using training imputer models
#' @param  object training imputer object
#' @param  newdata a data frame
#' @param  pmm.new whether or not to apply predictive mean matching with the new dataset. Default: FALSE
#' @param  pmm.k the number of donors for predictive mmean matching. Default: NULL
#' @param  m the number of imputed datasets. Default: NULL
#' @export



impute.new<-function(object,newdata,pmm.new=FALSE,pmm.k=NULL,m=NULL){


  #extract imputer models from the training object
  xgb.fit=object$saved.models


  #extract params from the training object
  if(is.null(pmm.k)){
    pmm.k=object$params$pmm.k
  }else{
    pmm.k=pmm.k
  }

  if(is.null(m)){
    m=object$params$m
  }else{
    if(m <= object$params$m){
      m=m
    }else{
      stop("The value of m in impute.new() cannot be larger than the value of m in $impute().")
    }

  }


  Names=object$params$Names
  type=object$params$type
  #new data variables should be in the same order as the training dataset
  sorted.idx=object$params$sorted.idx


  initial.imp=object$params$initial.imp
  pmm.type=object$params$pmm.type
  pmm.link=object$params$pmm.link

  #
  p=ncol(newdata)
  Nrow=nrow(newdata)
  #sort the newdata according to the sorted order of the training dataset
  sorted.df=newdata[Names]


  initial.df=sorted.df
  num.na=colSums(is.na(sorted.df))


  #check new data and give some warning messages (unfinished)
  if(all(num.na==0)){
    stop("No missing values in new data.")
  }


  #check if there is only one observation in new data, cannot use pmm.new=TRUE, force to match with the training data instead.
  if(Nrow == 1 & pmm.new == TRUE){
    stop("There is only one observation in new data.\n No other observations in new data to match.\n Please set pmm.new = FALSE")
  }

  #check if the number of observations is less than pmm.k when pmm.new=TRUE
  if(Nrow <= pmm.k & pmm.new ==TRUE){
    stop("The number of observations in new data is less than the value of pmm.k. \n Please either set pmm.new = FALSE or set pmm.k = 1")
  }

  if(any(num.na == Nrow) & pmm.new == TRUE){
    stop("There is at least one variable in new data with 100% missing values.\n No observed values in at least one variable in the new data to match.\n Please set pmm.new = FALSE")
  }

  if(any(Nrow-num.na < pmm.k) & pmm.new == TRUE){
    maxNA=max(num.na)
    minObs=Nrow-maxNA
    s1=paste("In this dataset, the minimum number of observed values in a variable is ", minObs, ".",sep="")
    s2=paste("However, pmm.k=",pmm.k,".",sep="")
    if(minObs == 1){
      s3=paste("Please either set pmm.new = FALSE or set pmm.k = 1 .")
    }else{
      s3=paste("Please either set pmm.new = FALSE or set the value of pmm.k less than or equal to ",minObs,".",sep="")
    }

    stop(paste(s1,s2,s3,sep="\n"))
  }


  na.newdata=list()

  #initial imputation
  if(pmm.new==FALSE){
    #extract a set of observed training data (use for initial imputation and pmm)
    train.df=object$imputed.data[[1]]
    Obs.index=object$params$Obs.index

    for(i in 1:p){


      #location of the missing new data
      na.index=which(is.na(sorted.df[,i]))
      na.newdata[[i]]=na.index
      #location of observed training data
      obs.index=Obs.index[[i]]


      if(initial.imp=="random"){

        initial.df[na.index,i]<-sample(train.df[Names][,i][obs.index],num.na[i],replace=TRUE)


      }else if(initial.imp=="rnorm"){

        if(type[i]=="numeric"){
          var.mean=mean(train.df[Names][,i],na.rm = T)
          var.sd=sd(train.df[Names][,i],na.rm = T)
          initial.df[na.index,i]<-rnorm(num.na[i],var.mean,var.sd)

        }else{
          initial.df[na.index,i]<-sample(train.df[Names][,i][obs.index],num.na[i],replace=TRUE)
        }



      }else{


        #numeric: initial impute median
        if(type[i]=="numeric"){
          initial.df[na.index,i]<-median(train.df[Names][,i],na.rm = T)
        }else{
          #factor:  initial impute major class
          initial.df[na.index,i]<-names(which.max(table(train.df[Names][,i])))
        }
      }
    }


  }else{

    #match to the new dataset



    for(i in 1:p){


      #location of the missing new data
      na.index=which(is.na(sorted.df[,i]))
      na.newdata[[i]]=na.index




      if(initial.imp=="random"){

        if(num.na[i] == Nrow-1){
          initial.df[na.index,i]<-rep(sorted.df[,i][-na.index],num.na[i])
        }else{
          initial.df[na.index,i]<-sample(sorted.df[,i][-na.index],num.na[i],replace=TRUE)
        }



      }else if(initial.imp=="rnorm"){

        if(type[i]=="numeric"){
          var.mean=mean(sorted.df[,i],na.rm = T)
          if(num.na[i] == Nrow-1){
            var.sd=0
          }else{
            var.sd=sd(sorted.df[,i],na.rm = T)
          }

          initial.df[na.index,i]<-rnorm(num.na[i],var.mean,var.sd)

        }else{

          if(num.na[i] == Nrow-1){
            initial.df[na.index,i]<-rep(sorted.df[,i][-na.index],num.na[i])
          }else{
            initial.df[na.index,i]<-sample(sorted.df[,i][-na.index],num.na[i],replace=TRUE)
          }

        }



      }else{


        #numeric: initial impute median
        if(type[i]=="numeric"){
          initial.df[na.index,i]<-median(sorted.df[,i],na.rm = T)
        }else{
          #factor:  initial impute major class
          initial.df[na.index,i]<-names(which.max(table(sorted.df[,i])))
        }
      }
    }


  }







  #### m multiple imputation
  if(is.null(pmm.type)){
    ###no pmm
    imputed.data<-list()


    for(k in 1:m){

      copy=initial.df


      for(i in 1:p){

        na.index=na.newdata[[i]]

        if(length(na.index)>0){
          #only impute columns with missing values




          if(p==2){

            mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
          }else{

            mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
          }


          if(length(na.index)==1){
            mis.data=t(mis.data)
          }



          if(type[i]=="numeric"){

            pred.y=predict(xgb.fit[[k]][[i]],mis.data)
            #update dataset
            copy[,i][na.index]<-pred.y


          }else if(type[i]=="binary"){



            if(length(xgb.fit[[k]][[i]])!=1){

              xgb.pred = predict(xgb.fit,mis.data)
              pred.y=ifelse(xgb.pred>=0.5,1,0)
              pred.y=levels(train.df[Names][,i])[pred.y+1]

              #update dataset
              copy[,i][na.index]<-pred.y

            }else{
              #skip xgboost training, just impute majority class
              copy[,i][na.index]<-xgb.fit[[k]][[i]]
            }





          }else{

            xgb.pred = predict(xgb.fit[[k]][[i]],mis.data)
            pred.y=levels(train.df[Names][,i])[xgb.pred+1]
            #update dataset
            copy[,i][na.index]<-pred.y
          }



        }

      }#end of 1:p
      imputed.data[[k]]<-copy[order(sorted.idx)]



    }

    return(imputed.data)


  }else if(pmm.type==1){
    ####
    #multiple imputation m=5
    imputed.data<-list()

    if(pmm.new==FALSE){
      #extract predicted value of observed data in training set
      yhatobs.list=object$params$yhatobs.list
      #extract original observed data in training data
      yobs.list=object$params$yobs.list
    }else{
      #match to new dataset
      yhatobs.list<-list()
      yobs.list<-list()

      for(i in 1:p){
        na.index=na.newdata[[i]]

        if(length(na.index)>0){
          obs.y=sorted.df[,i][-na.index]

          if(type[i]!="numeric"){
            obs.y=as.integer(obs.y)-1
          }
          yobs.list[[i]]=obs.y

          if(p==2){
            obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])

          }else{
            obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]

          }

          if(length(obs.y)==1){
            obs.data=t(obs.data)
            warning("There is only one observed entry in an numeric variable in the new data.\n PMM match to new data may not be sensible. \n Recommend to set pmm.new = FALSE  ")
          }

          if(type[i]=="numeric"){

            yhatobs=predict(xgb.fit[,1][[1]][[i]],obs.data)
            #update dataset
            yhatobs.list[[i]]=yhatobs

          }else if(type[i]=="binary"){

            t=sort(table(obs.y))
            #t[1] minority class t[2]majority class

            if(!is.na(t[2]) & length(xgb.fit[,1][[1]][[i]])!=1){
              #if the new data have more than one class and
              #the training data have more than one class

              xgb.pred = predict(xgb.fit[,1][[1]][[i]],obs.data)
              yhatobs.list[[i]]=xgb.pred


            }else{
              #if the training data mainly have one class
              warning("Majority class of a binary variable in the new dataset was used as the predicted value for predictive mean matching (type 1).\n Because this variable only contain one class in the new dataset.")
              #skip getting predicted value using the saved xgboost model, just impute majority class
              yhatobs.list[[i]]<-rep(t[1],length(obs.y))
            }



          }else{

            xgb.pred = predict(xgb.fit[,1][[1]][[i]],obs.data,reshape = T)

            if(pmm.link=="logit"){
              xgb.pred<-log(xgb.pred/(1-xgb.pred))
            }

            yhatobs.list[[i]]=xgb.pred

          }
        }

      }
    }#end of match to new data for pmm type 1



    for(k in 1:m){


      copy=initial.df


      for(i in 1:p){
        ########


        ###########

        na.index=na.newdata[[i]]

        if(length(na.index)>0){

          if(p==2){

            mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])
          }else{

            mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])[,-1]
          }

          if(length(na.index)==1){
            mis.data=t(mis.data)
          }


          if(type[i]=="numeric"){

            pred.y=predict(xgb.fit[,2][[k]][[i]],mis.data)


            #update dataset
            copy[,i][na.index]<- pmm(yhatobs = yhatobs.list[[i]],yhatmis = pred.y,yobs=yobs.list[[i]],k=pmm.k)


          }else if(type[i]=="binary"){


            if(length(xgb.fit[,2][[k]][[i]])!=1){

              xgb.pred = predict(xgb.fit[,2][[k]][[i]],mis.data)

              num.result=pmm(yhatobs = yhatobs.list[[i]],yhatmis = xgb.pred,yobs=yobs.list[[i]],k=pmm.k)
              #change to factor
              copy[,i][na.index]<- levels(train.df[Names][,i])[num.result+1]


            }else{
              #skip xgboost training, just impute majority class
              copy[,i][na.index]<-xgb.fit[,2][[k]][[i]]
            }





          }else{

            xgb.pred= predict(xgb.fit[,2][[k]][[i]],mis.data,reshape = T)

            if(pmm.link=="logit"){
              xgb.pred=log(xgb.pred/(1-xgb.pred))
            }

            num.result=pmm.multiclass(donor.pred = yhatobs.list[[i]],target.pred = xgb.pred,donor.obs = yobs.list[[i]],k=pmm.k)
            #change to factor
            copy[,i][na.index]<- levels(train.df[Names][,i])[num.result+1]


          }
        }

      }







      imputed.data[[k]]<-copy[order(sorted.idx)]



    }#end m multiple imputation

    return(imputed.data)

  }else if(pmm.type==2){
    ###########
    imputed.data<-list()

    if(pmm.new==FALSE){
      #extract predicted value of observed data in training set
      yhatobs.list=object$params$yhatobs.list
      #extract original observed data in training data
      yobs.list=object$params$yobs.list
    }else{
      #match to new dataset
      yhatobs.list<-replicate(m,list())
      yobs.list<-list()

      #store observed values in the new dataset
      for(i in 1:p){
        na.index=na.newdata[[i]]

        if(length(na.index)>0){

          obs.y=sorted.df[,i][-na.index]

          if(type[i]!="numeric"){
            obs.y=as.integer(obs.y)-1
          }

          yobs.list[[i]]=obs.y
        }
      }

      #store predicted values of observed entries from new data

    for(k in 1:m){

      for(i in 1:p){

        na.index=na.newdata[[i]]

        if(length(na.index)>0){

          if(p==2){
            obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])

          }else{
            obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]

          }

          if(length(yobs.list[[i]])==1){
            obs.data=t(obs.data)
            warning("There is only one observed entry in an numeric variable in the new data.\n PMM match to new data may not be sensible. \n Recommend to set pmm.new = FALSE  ")
          }


          if(type[i]=="numeric"){

            yhatobs=predict(xgb.fit[[k]][[i]],obs.data)
            #update dataset
            yhatobs.list[[k]][[i]]=yhatobs

          }else if(type[i]=="binary"){

            t=sort(table(yobs.list[[i]]))
            #t[1] minority class t[2]majority class



            if(!is.na(t[2]) & length(xgb.fit[[k]][[i]])!=1){
              #if the new data have more than one class and
              #the training data have more than one class
              xgb.pred = predict(xgb.fit[[k]][[i]],obs.data)
              yhatobs.list[[k]][[i]]=xgb.pred


            }else{
              #if the training data mainly have one class
              warning("Majority class of a binary variable in the new dataset was used as the predicted value
                      for predictive mean matching (type 2) because the variable in the training dataset or new dataset mainly contain one class")
              #skip getting predicted value using the saved xgboost model, just impute majority class
              yhatobs.list[[k]][[i]]<-rep(t[1],length(obs.y))
            }



          }else{

            xgb.pred = predict(xgb.fit[[k]][[i]],obs.data,reshape = T)

            if(pmm.link=="logit"){
              xgb.pred<-log(xgb.pred/(1-xgb.pred))
            }

            yhatobs.list[[k]][[i]]=xgb.pred

          }


        }

      }
    }
    }






    for(k in 1:m){

      copy=initial.df

      for(i in 1:p){

        na.index=na.newdata[[i]]

        if(length(na.index)>0){

          if(p==2){

            mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
          }else{

            mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
          }

          if(length(na.index)==1){
            mis.data=t(mis.data)
          }


          if(type[i]=="numeric"){

            pred.y=predict(xgb.fit[[k]][[i]],mis.data)
            ###use boostrap observed data to fit model
            #use this model to predict all missing values in new data and match with all observed values in new data

            #update dataset
            copy[,i][na.index]<-pmm(yhatobs = yhatobs.list[[k]][[i]],yhatmis = pred.y,yobs=yobs.list[[i]],k=pmm.k)



          }else if(type[i]=="binary"){


            if(length(xgb.fit[[k]][[i]])!=1){


              xgb.pred = predict(xgb.fit[[k]][[i]],mis.data)


              num.result=pmm(yhatobs = yhatobs.list[[k]][[i]],yhatmis = xgb.pred,yobs=yobs.list[[i]],k=pmm.k)
              #change to factor
              copy[,i][na.index]<- levels(train.df[Names][,i])[num.result+1]

            }else{
              #skip xgboost training, just impute majority class
              copy[,i][na.index]<-xgb.fit[[k]][[i]]
            }





          }else{



            xgb.pred = predict(xgb.fit[[k]][[i]],mis.data,reshape = T)



            if(pmm.link=="logit"){
              xgb.pred=log(xgb.pred/(1-xgb.pred))
            }
            num.result=pmm.multiclass(donor.pred = yhatobs.list[[k]][[i]],target.pred = xgb.pred,donor.obs = yobs.list[[i]],k=pmm.k)
            #change to factor
            copy[,i][na.index]<- levels(train.df[Names][,i])[num.result+1]

          }


        }


      }
      imputed.data[[k]]<-copy[order(sorted.idx)]



    }#end of k loops

    return(imputed.data)


    }else if(pmm.type=="auto"){

      imputed.data<-list()

      if(pmm.new==FALSE){
        #extract predicted value of observed data in training set
        yhatobs.list=object$params$yhatobs.list
        #extract original observed data in training data
        yobs.list=object$params$yobs.list
      }else{
        #match to new dataset
        yhatobs.list<-replicate(m,list())
        yobs.list<-list()

        for(i in 1:p){
          na.index=na.newdata[[i]]

          if(length(na.index)>0){
            obs.y=sorted.df[,i][-na.index]

            if(type[i]!="numeric"){
              obs.y=as.integer(obs.y)-1
            }

            yobs.list[[i]]=obs.y
          }
        }


        for(k in 1:m){

          for(i in 1:p){
            na.index=na.newdata[[i]]

            if(length(na.index)>0){

              if(p==2){
                obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])

              }else{
                obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]

              }

              if(length(yobs.list[[i]])==1){
                obs.data=t(obs.data)
              }




              if(type[i]=="numeric"){

                yhatobs=predict(xgb.fit[[k]][[i]],obs.data)
                if(length(yhatobs)==1){
                  warning("There is only one observed entry in an numeric variable in the new data.\n PMM match to new data may not be sensible. \n Recommend to set pmm.new = FALSE .")
                }
                #update dataset
                yhatobs.list[[k]][[i]]=yhatobs

              }#for factor variables don't need to do PMM

            }
          }
        }#end of k loops
      }#end of when pmm.new=TRUE, obtain yobs.list and yhatobs.list


      #impute

      for(k in 1:m){


        copy=initial.df

        for(i in 1:p){

          na.index=na.newdata[[i]]

          if(length(na.index)>0){

            if(p==2){

              mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
            }else{

              mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
            }


            if(length(na.index)==1){
              mis.data=t(mis.data)
            }


            if(type[i]=="numeric"){
              #pmm type 2 for continuous variables


              pred.y=predict(xgb.fit[[k]][[i]],mis.data)


              #update dataset
              copy[,i][na.index]<-pmm(yhatobs = yhatobs.list[[k]][[i]],yhatmis = pred.y,yobs=yobs.list[[i]],k=pmm.k)





            }else if(type[i]=="binary"){


              if(length(xgb.fit[[k]][[i]])!=1){


                xgb.pred = predict(xgb.fit[[k]][[i]],mis.data)
                pred.y=ifelse(xgb.pred>=0.5,1,0)

                #update dataset
                copy[,i][na.index]<-levels(train.df[Names][,i])[pred.y+1]


              }else{
                #skip xgboost training, just impute majority class
                copy[,i][na.index]<-xgb.fit[[k]][[i]]
              }







            }else{




              xgb.pred = predict(xgb.fit[[k]][[i]],mis.data)
              #update dataset
              copy[,i][na.index]<-levels(train.df[Names][,i])[xgb.pred+1]


            }

          }





        }#end of p loops

        imputed.data[[k]]<-copy[order(sorted.idx)]



      }#end of k loops


      return(imputed.data)



    }#end of pmm.type="auto"



  }#end of impute.new function





