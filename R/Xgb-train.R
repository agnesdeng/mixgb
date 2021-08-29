#' Multiple imputation through xgboost R6 class imputer object for training set
#' @docType  class
#' @description Set up an xgboost imputer object with specified hyperparameters and then obtain an imputed object including multiple imputed datasets, saved models and parameters.
#' @format  NULL
#' @import xgboost
#' @export

Mixgb.train <- R6Class("Mixgb.train",
                   cloneable = FALSE,
                    public = list(
                      data=NULL,
                      nrounds=NULL,
                      max_depth=NULL,
                      gamma=NULL,
                      eta=NULL,
                      colsample_bytree=NULL,
                      min_child_weight=NULL,
                      subsample=NULL,
                      print_every_n=NULL,
                      verbose=NULL,
                      nthread=NULL,
                      early_stopping_rounds=NULL,
                      pmm.k=NULL,
                      pmm.type=NULL,
                      pmm.link=NULL,
                      initial.imp=NULL,
                      scale_pos_weight=NULL,
                      tree_method=NULL,
                      gpu_id=NULL,
                      predictor=NULL,

                    #'@description Create a new \code{Mixgb} object. This is used to set up the multiple imputation imputer using xgboost.
                    #'@examples
                    #'MIXGB=Mixgb.train$new(withNA.df)
                    #'MIXGB=Mixgb.train$new(withNA.df,nrounds=50,max_depth=6)
                    #'@param data A data frame with missing values
                    #'@param nrounds max number of boosting iterations. Default: 50
                    #'@param max_depth maximum depth of the tree. Default: 6
                    #'@param gamma Default: 0.1
                    #'@param eta Default: 0.3
                    #'@param nthread Default: 4
                    #'@param early_stopping_rounds Default: 10,
                    #'@param colsample_bytree Default: 1
                    #'@param min_child_weight Default: 1
                    #'@param subsample Default: 1
                    #'@param pmm.k Default: 5
                    #'@param pmm.type Default: "auto" (used to be NULL)
                    #'@param pmm.link Default: "logit"
                    #'@param initial.imp Default: "random"
                    #'@param print_every_n Default: 10L
                    #'@param verbose Default: 0
                    #'@param scale_pos_weight Default:1
                    #'@param tree_method Default: "auto" (can set "gpu_hist" for linux)
                    #'@param gpu_id Device ordinal. Default: 0
                    #'@param predictor The type of predictor algorithm to use. Default: "auto" (other options: "cpu_predictor","gpu_predictor")

                    initialize = function(data,nrounds=50,max_depth=6,gamma=0.1,eta=0.3,nthread=4,early_stopping_rounds=10,colsample_bytree=1,min_child_weight=1,subsample=1,pmm.k=5,pmm.type="auto",pmm.link="logit",scale_pos_weight=1,initial.imp="random",tree_method="auto",gpu_id=0,predictor="auto",print_every_n = 10L,verbose=0) {
                      self$data<-data
                      self$nrounds=nrounds
                      self$max_depth=max_depth
                      self$gamma=gamma
                      self$eta=eta
                      self$colsample_bytree=colsample_bytree
                      self$min_child_weight=min_child_weight
                      self$subsample=subsample
                      self$print_every_n=print_every_n
                      self$verbose=verbose
                      self$nthread=nthread
                      self$early_stopping_rounds=early_stopping_rounds
                      self$pmm.k=pmm.k
                      self$pmm.type=pmm.type
                      self$pmm.link=pmm.link
                      self$initial.imp=initial.imp
                      self$scale_pos_weight=scale_pos_weight
                      self$tree_method=tree_method
                      self$gpu_id=gpu_id
                      self$predictor=predictor

                    },

                    #'@description Use the imputer to impute missing values and obtain multiple imputed datasets, saved training models and some parameters needed for future use.
                    #'@examples
                    #'MIXGB=Mixgb.train$new(withNA.df)
                    #'mixgb.obj=MIXGB$impute(m = 5)
                    #'@param m the number of imputed datasets. Default: 5

                    impute = function(m=5){
                      data=self$data
                      #pmm function match the imputed value wit model observed values, then extra the original value yobs
                      pmm<- function(yhatobs, yhatmis, yobs,k=self$pmm.k){
                        #idx=.Call('_mice_matcher', PACKAGE = 'mice', yhatobs, yhatmis, k)

                        idx=mice::matchindex(d=yhatobs,t=yhatmis,k=k)
                        yobs[idx]
                      }

                      pmm.multiclass<-function(donor.pred,target.pred,donor.obs,k=self$pmm.k){
                        #shuffle donors to break ties
                        donor.size=length(donor.obs)
                        idx=sample(donor.size,replace=F)
                        donor.randompred=donor.pred[idx,]
                        donor.randomobs=donor.obs[idx]
                        #matching
                        match.class=Rfast::knn(xnew=target.pred,y=donor.randomobs,x=donor.randompred,k=k)
                        as.vector(match.class)
                      }

                      #data=withNA.df
                      p=ncol(data)
                      Nrow=nrow(data)


                      ###data preprocessing
                      #1) sort the dataset with increasing NAs
                      sorted.df<-sortNA(data)$sorted.df
                      sorted.idx<-sortNA(data)$sorted.idx
                      type<-feature_type(sorted.df)
                      Names<-colnames(sorted.df)

                      #2)initial imputation

                      initial.df=sorted.df
                      num.na=colSums(is.na(sorted.df))


                      if(all(num.na==0)){
                        stop("No missing values in this data frame.")
                      }

                      if(any(num.na==Nrow)){
                        stop("At least one variable in the data frame has 100% missing values.")

                      }


                      #if pmm.type=1  or pmm.type=2
                      if(any(Nrow-num.na < self$pmm.k) & !is.null(self$pmm.type) & self$pmm.type!="auto"){
                        maxNA=max(num.na)
                        minObs=Nrow-maxNA
                        s1=paste("In this dataset, the minimum number of observed values in a variable is ", minObs, ".",sep="")
                        s2=paste("However, pmm.k=",self$pmm.k,".",sep="")
                        if(minObs == 1){
                          s3=paste("Please set pmm.k = 1 .")
                        }else{
                          s3=paste("Please either set pmm.new = FALSE or set the value of pmm.k less than or equal to ",minObs,".",sep="")
                        }

                        stop(paste(s1,s2,s3,sep="\n"))

                      }

                      #if pmm.type="auto", only numeric variables need to perform PMM
                      if(self$pmm.type=="auto"){
                        idx=which(Nrow-num.na < self$pmm.k & type == "numeric")
                        if(length(idx)>0){
                          maxNA=max(num.na[idx])
                          minObs=Nrow-maxNA
                          s1=paste("In this dataset, the minimum number of observed values in a numeric variable is ", minObs, ".",sep="")
                          s2=paste("When pmm.type = \"auto\", type 2 PMM would apply to numeric variables. However, pmm.k=",self$pmm.k,".",sep="")
                          if(minObs == 1){
                            s3=paste("Please set pmm.k = 1 .")
                          }else{
                            s3=paste("Please either set pmm.new = FALSE or set the value of pmm.k less than or equal to ",minObs,".",sep="")
                          }

                          stop(paste(s1,s2,s3,sep="\n"))
                        }

                      }




                      if(any(num.na>=0.9*Nrow)){
                        warning("Some variables have more than 90% miss entries.")
                      }


                      Obs.index=list()
                      Na.index=list()

                      for(i in 1:p){
                        obs.index=which(!is.na(sorted.df[,i]))
                        na.index=which(is.na(sorted.df[,i]))
                        Obs.index[[i]]=obs.index
                        Na.index[[i]]=na.index

                        if(self$initial.imp=="random"){

                          if(length(obs.index)==1){
                            initial.df[na.index,i]<-rep(sorted.df[,i][obs.index],num.na[i])
                          }else{
                            initial.df[na.index,i]<-sample(sorted.df[,i][obs.index],num.na[i],replace=TRUE)
                          }




                        }else if(self$initial.imp=="rnorm"){

                          if(type[i]=="numeric"){

                            var.mean=mean(sorted.df[,i],na.rm = T)
                            if(length(obs.index)==1){
                              var.sd=0
                            }else{
                              var.sd=sd(sorted.df[,i],na.rm = T)
                            }
                            initial.df[na.index,i]<-rnorm(num.na[i],var.mean,var.sd)

                          }else{
                            #factor variables
                            if(length(obs.index)==1){
                              initial.df[na.index,i]<-rep(sorted.df[,i][obs.index],num.na[i])
                            }else{
                              initial.df[na.index,i]<-sample(sorted.df[,i][obs.index],num.na[i],replace=TRUE)
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

                      #3) imputation

                      params=list()
                      params$initial.imp=self$initial.imp
                      params$pmm.k=self$pmm.k
                      params$pmm.type=self$pmm.type
                      params$pmm.link=self$pmm.link




                      params$m=m
                      params$Names=Names
                      params$type=type
                      params$sorted.idx=sorted.idx
                      params$Obs.index=Obs.index



                      if(m==1){

                        if(is.null(self$pmm.type)){
                          message("Single imputation with PMM is not provided yet. This feature will be added in a later version of mixgb.")
                          #no pmm for single imputation
                          for(i in 1:p){
                            na.index=Na.index[[i]]
                            if(length(na.index)>0){
                              obs.y=sorted.df[,i][-na.index]

                              if(type[i]!="numeric"){
                                obs.y=as.integer(obs.y)-1
                              }
                              if(p==2){
                                obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                                mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])
                              }else{
                                obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                                mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])[,-1]
                              }

                              if(length(na.index)==1){
                                mis.data=t(mis.data)
                              }


                              if(type[i]=="numeric"){
                                obj.type<-"reg:squarederror"
                                xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                pred.y=predict(xgb.fit,mis.data)
                                #update dataset
                                sorted.df[,i][na.index]<-pred.y

                              }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class



                                  if(!is.na(t[2])){
                                    obj.type<-"binary:logistic"
                                    xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                    eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                    min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                    xgb.pred = predict(xgb.fit,mis.data)
                                    pred.y=ifelse(xgb.pred>=0.5,1,0)
                                    pred.y=levels(sorted.df[,i])[pred.y+1]
                                    #update dataset
                                    sorted.df[,i][na.index]<-pred.y
                                  }else{
                                    #skip xgboost training, just impute majority class
                                    sorted.df[,i][na.index]<-names(t[1])

                                  }







                              }else{
                                obj.type= "multi:softmax"
                                N.class=length(levels(sorted.df[,i]))
                                xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                xgb.pred = predict(xgb.fit,mis.data)
                                pred.y=levels(sorted.df[,i])[xgb.pred+1]
                                #update dataset
                                sorted.df[,i][na.index]<-pred.y
                              }
                            }

                          }
                          return(sorted.df[order(sorted.idx)])



                        }else{
                               #single imputation with pmm (if pmm.type is not null)
                          warning("Imputed results are shown without using PMM. Single imputation with PMM is not provided yet.This feature will be added in a later version of mixgb.")
                          self$pmm.type=NULL
                          yhatobs.list<-list()
                          yobs.list<-list()
                          for(i in 1:p){
                            na.index=Na.index[[i]]
                            if(length(na.index)>0){
                              obs.y=sorted.df[,i][-na.index]

                              if(type[i]!="numeric"){
                                obs.y=as.integer(obs.y)-1
                              }
                              if(p==2){
                                obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                                mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])
                              }else{
                                obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                                mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])[,-1]
                              }

                              if(length(na.index)==1){
                                mis.data=t(mis.data)
                              }


                              if(type[i]=="numeric"){
                                obj.type<-"reg:squarederror"
                                xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                pred.y=predict(xgb.fit,mis.data)
                                yhatobs=predict(xgb.fit,obs.data)
                                #update dataset
                                sorted.df[,i][na.index]<- pmm(yhatobs = yhatobs,yhatmis = pred.y,yobs=obs.y,k=self$pmm.k)

                              }else if(type[i]=="binary"){




                                t=sort(table(obs.y))
                                #t[1] minority class t[2]majority class

                                if(!is.na(t[2])){
                                  obj.type<-"binary:logistic"
                                  xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                  eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                  min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                  xgb.pred = predict(xgb.fit,mis.data)
                                  yhatobs=predict(xgb.fit,obs.data)
                                  #update dataset
                                  num.result<- pmm(yhatobs = yhatobs,yhatmis = xgb.pred,yobs=obs.y,k=self$pmm.k)
                                  sorted.df[,i][na.index]<- levels(sorted.df[,i])[num.result+1]

                                }else{
                                  #skip xgboost training, just impute majority class
                                  sorted.df[,i][na.index]<-names(t[1])
                                }




                              }else{
                                obj.type= "multi:softmax"
                                N.class=length(levels(sorted.df[,i]))
                                xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                xgb.pred = predict(xgb.fit,mis.data)
                                pred.y=levels(sorted.df[,i])[xgb.pred+1]
                                #update dataset
                                sorted.df[,i][na.index]<-pred.y
                              }
                            }

                          }
                          return(sorted.df[order(sorted.idx)])


                        }


                      }else{
                        imputed.data<-list()


                        #### m multiple imputation
                       if(is.null(self$pmm.type)){
                       ###no pmm
                         saved.models<-replicate(m,list())


                         for(k in 1:m){
                          #sorted.df :  sorted dataset with increasing %NA , containing NA
                          #initial.df:  sorted.df with initial NA imputed
                          #Boot.data:  bootstrap sample of sorted.df,  containing NAs

                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]
                           copy=initial.df




                           for(i in 1:p){
                             #Boot.initial: bootstrap sample of initial.df,  with NA being imputed with Mean/Median etc
                             Boot.initial=copy[index,]
                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=Na.index[[i]]

                             if(length(Bna.index)==Nrow | length(Bna.index)==Nrow-1){
                               stop("At least one variable in the boostrapped sample has 100% missing values or only one observed entry.\n This implies that there is at least one variable in the original dataset has too many missing entries.\n Imputation procedure aborts.\n Please consider removing variables with too many missing values before imputation.")
                             }

                             if(length(na.index)>0){

                               if(length(Bna.index)==0){

                                 obs.y=Boot.data[,i]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }



                               }else{

                                 obs.y=Boot.data[,i][-Bna.index]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }


                               }




                               if(length(na.index)==1){
                                 mis.data=t(mis.data)
                               }







                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 #update dataset
                                 copy[,i][na.index]<-pred.y
                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit

                               }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){
                                   obj.type<-"binary:logistic"
                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)
                                   pred.y=ifelse(xgb.pred>=0.5,1,0)
                                   pred.y=levels(sorted.df[,i])[pred.y+1]

                                   #update dataset
                                   copy[,i][na.index]<-pred.y
                                   #save model for the k'th imputed dataset, the i'th variable
                                   saved.models[[k]][[i]]<-xgb.fit
                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])

                                   saved.models[[k]][[i]]<-names(t[1])

                                 }





                               }else{
                                 obj.type= "multi:softmax"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,mis.data)
                                 pred.y=levels(sorted.df[,i])[xgb.pred+1]
                                 #update dataset
                                 copy[,i][na.index]<-pred.y
                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit
                               }

                             }else{
                               #If there is no missing value in this variable in the training dataset, still fit and save a model for imputing new dataset
                               obs.y=Boot.data[,i]

                               if(type[i]!="numeric"){
                                 obs.y=as.integer(obs.y)-1
                               }

                               if(p==2){
                                 obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)

                               }else{
                                 obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]

                               }

                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit

                               }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){
                                   obj.type<-"binary:logistic"
                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                   #save model for the k'th imputed dataset, the i'th variable
                                   saved.models[[k]][[i]]<-xgb.fit
                                 }else{
                                   #skip xgboost training, just impute majority class


                                   saved.models[[k]][[i]]<-names(t[1])

                                 }





                               }else{
                                 obj.type= "multi:softmax"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit
                               }

                             }#end of else if there is no missing value in this variable

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]



                         }


                         return(list("imputed.data"=imputed.data,"saved.models"=saved.models,"params"=params))

                       }else if(self$pmm.type==1){

                         saved.models<-replicate(2,replicate(m,list()))
                         #multiple imputation

                         yhatobs.list<-list()
                         yobs.list<-list()

                         for(i in 1:p){
                           na.index=Na.index[[i]]

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



                           }else{
                             obs.y=sorted.df[,i]

                             if(type[i]!="numeric"){
                               obs.y=as.integer(obs.y)-1
                             }
                             yobs.list[[i]]=obs.y

                             if(p==2){
                               obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df)

                             }else{
                               obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df)[,-1]

                             }

                           }

                             if(type[i]=="numeric"){
                               obj.type<-"reg:squarederror"
                               xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                               nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                               min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                               yhatobs=predict(xgb.fit,obs.data)
                               #update dataset
                               yhatobs.list[[i]]=yhatobs
                               #save model (using whole training data) for the k'th imputed dataset, the i'th variable
                               saved.models[,1][[1]][[i]]<-xgb.fit

                             }else if(type[i]=="binary"){

                               t=sort(table(obs.y))
                               #t[1] minority class t[2]majority class

                               if(!is.na(t[2])){
                                 if(self$pmm.link=="logit"){
                                   obj.type<-"binary:logitraw"
                                 }else{
                                   #pmm by "prob"
                                   obj.type<-"binary:logistic"
                                 }

                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,obs.data)
                                 yhatobs.list[[i]]=xgb.pred
                                 #save model (using whole training data) for the k'th imputed dataset, the i'th variable
                                 saved.models[,1][[1]][[i]]<-xgb.fit



                               }else{
                                 #skip xgboost training, just impute majority class
                                 yhatobs.list[[i]]<-rep(names(t[1]),length(obs.y))
                                 #save model (using whole training data) for the k'th imputed dataset, the i'th variable
                                 saved.models[,1][[1]][[i]]<-rep(names(t[1]),length(obs.y))


                               }



                             }else{
                               obj.type= "multi:softprob"
                               N.class=length(levels(sorted.df[,i]))
                               xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                               nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                               min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                               xgb.pred = predict(xgb.fit,obs.data,reshape = T)

                               #save model (using whole training data) for the k'th imputed dataset, the i'th variable
                               saved.models[,1][[1]][[i]]<-xgb.fit

                               if(self$pmm.link=="logit"){
                                 xgb.pred<-log(xgb.pred/(1-xgb.pred))
                               }

                               yhatobs.list[[i]]=xgb.pred

                             }


                         }
                              params$yobs.list=yobs.list
                              params$yhatobs.list=yhatobs.list


                         for(k in 1:m){

                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]
                           Boot.initial=initial.df[index,]
                           copy=initial.df

                           for(i in 1:p){
                             ########


                             ###########
                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=Na.index[[i]]

                             if(length(Bna.index)==Nrow | length(Bna.index)==Nrow-1){
                               stop("At least one variable in the boostrapped sample has 100% missing values or only one observed entry.\n This implies that there is at least one variable in the original dataset has too many missing entries.\n Imputation procedure aborts.\n Please consider removing variables with too many missing values before imputation.")
                             }

                             if(length(na.index)>0){

                               if(length(Bna.index)==0){

                                 obs.y=Boot.data[,i]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }



                               }else{

                                 obs.y=Boot.data[,i][-Bna.index]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }


                               }





                               if(length(na.index)==1){
                                 mis.data=t(mis.data)
                               }






                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 #update dataset
                                 #copy[,i][na.index]<-pred.y
                                 #pred.y=predict(xgb.fit,mis.data)

                                 #update dataset
                                 copy[,i][na.index]<- pmm(yhatobs = yhatobs.list[[i]],yhatmis = pred.y,yobs=yobs.list[[i]],k=self$pmm.k)
                                 #save model (using bootstrapped training data) for the k'th imputed dataset, the i'th variable
                                 saved.models[,2][[k]][[i]]<-xgb.fit

                               }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){
                                   if(self$pmm.link=="logit"){
                                     obj.type<-"binary:logitraw"
                                   }else{
                                     #pmm by "prob"
                                     obj.type<-"binary:logistic"
                                   }

                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)

                                   num.result=pmm(yhatobs = yhatobs.list[[i]],yhatmis = xgb.pred,yobs=yobs.list[[i]],k=self$pmm.k)
                                   #change to factor
                                   copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]
                                   #save model (using bootstrapped training data) for the k'th imputed dataset, the i'th variable
                                   saved.models[,2][[k]][[i]]<-xgb.fit

                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])
                                   saved.models[,2][[k]][[i]]<-names(t[1])
                                 }





                               }else{
                                 obj.type= "multi:softprob"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred= predict(xgb.fit,mis.data,reshape = T)

                                 if(self$pmm.link=="logit"){
                                   xgb.pred=log(xgb.pred/(1-xgb.pred))
                                 }

                                 num.result=pmm.multiclass(donor.pred = yhatobs.list[[i]],target.pred = xgb.pred,donor.obs = yobs.list[[i]],k=self$pmm.k)
                                 #change to factor
                                 copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]
                                 #save model (using bootstrapped training data) for the k'th imputed dataset, the i'th variable
                                 saved.models[,2][[k]][[i]]<-xgb.fit


                               }

                             }else{
                               #If there is no NAs in this variable




                                 obs.y=Boot.data[,i]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)

                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]

                                 }


                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                 #save model (using bootstrapped training data) for the k'th imputed dataset, the i'th variable
                                 saved.models[,2][[k]][[i]]<-xgb.fit

                               }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){
                                   if(self$pmm.link=="logit"){
                                     obj.type<-"binary:logitraw"
                                   }else{
                                     #pmm by "prob"
                                     obj.type<-"binary:logistic"
                                   }

                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)


                                   #save model (using bootstrapped training data) for the k'th imputed dataset, the i'th variable
                                   saved.models[,2][[k]][[i]]<-xgb.fit

                                 }else{
                                   #skip xgboost training, just impute majority class

                                   saved.models[,2][[k]][[i]]<-names(t[1])
                                 }





                               }else{
                                 obj.type= "multi:softprob"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                 #save model (using bootstrapped training data) for the k'th imputed dataset, the i'th variable
                                 saved.models[,2][[k]][[i]]<-xgb.fit


                               }


                             }

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]



                         }#end m multiple imputation

                         return(list("imputed.data"=imputed.data,"saved.models"=saved.models,"params"=params))

                       }else if(self$pmm.type==2){
                         ###########

                          saved.models<-replicate(m,list())
                          yhatobs.list<-replicate(m,list())
                          yobs.list<-list()

                         for(i in 1:p){
                           na.index=Na.index[[i]]

                            if(length(na.index)>0){
                             obs.y=sorted.df[,i][-na.index]

                             if(type[i]!="numeric"){
                               obs.y=as.integer(obs.y)-1
                             }
                             yobs.list[[i]]=obs.y

                            }else{
                              obs.y=sorted.df[,i]

                              if(type[i]!="numeric"){
                                obs.y=as.integer(obs.y)-1
                              }
                              yobs.list[[i]]=obs.y

                            }



                         }


                          params$yobs.list=yobs.list


                         for(k in 1:m){


                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]

                           copy=initial.df

                           for(i in 1:p){
                             Boot.initial=copy[index,]

                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=Na.index[[i]]

                             if(length(Bna.index)==Nrow | length(Bna.index)==Nrow-1){
                               stop("At least one variable in the boostrapped sample has 100% missing values or only one observed entry.\n This implies that there is at least one variable in the original dataset has too many missing entries.\n Imputation procedure aborts.\n Please consider removing variables with too many missing values before imputation.")
                             }


                             if(length(na.index)>0){

                               if(length(Bna.index)==0){

                                 obs.y=Boot.data[,i]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }

                               }else{


                                 obs.y=Boot.data[,i][-Bna.index]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }

                               }

                               if(length(na.index)==1){
                                 mis.data=t(mis.data)
                               }





                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 ###use boostrap observed data to fit model
                                 #use this model to predict all missing whole data and match with all observed whole data
                                  yhatobs=predict(xgb.fit,Obs.data)
                                  yhatobs.list[[k]][[i]]=yhatobs
                                 #update dataset
                                 copy[,i][na.index]<-pmm(yhatobs = yhatobs,yhatmis = pred.y,yobs=yobs.list[[i]],k=self$pmm.k)
                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit


                               }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){
                                   if(self$pmm.link=="logit"){
                                     obj.type<-"binary:logitraw"
                                   }else{
                                     #pmm by "prob"
                                     obj.type<-"binary:logistic"
                                   }
                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)


                                   yhatobs=predict(xgb.fit,Obs.data)
                                   yhatobs.list[[k]][[i]]=yhatobs
                                   num.result=pmm(yhatobs = yhatobs,yhatmis = xgb.pred,yobs=yobs.list[[i]],k=self$pmm.k)
                                   #change to factor
                                   copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]
                                   #save model for the k'th imputed dataset, the i'th variable
                                   saved.models[[k]][[i]]<-xgb.fit

                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])
                                   saved.models[[k]][[i]]<-names(t[1])
                                 }





                               }else{


                                 obj.type= "multi:softprob"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,mis.data,reshape = T)

                                 yhatobs=predict(xgb.fit,Obs.data,reshape = T)


                                 if(self$pmm.link=="logit"){
                                   xgb.pred=log(xgb.pred/(1-xgb.pred))
                                   yhatobs=log(yhatobs/(1-yhatobs))
                                 }

                                 yhatobs.list[[k]][[i]]=yhatobs

                                 num.result=pmm.multiclass(donor.pred = yhatobs,target.pred = xgb.pred,donor.obs = yobs.list[[i]],k=self$pmm.k)
                                 #change to factor
                                 copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]
                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit
                               }

                             }else{
                               #If there is no NAs in this variable


                                 obs.y=Boot.data[,i]

                                 if(type[i]!="numeric"){
                                   obs.y=as.integer(obs.y)-1
                                 }

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df)

                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df)[,-1]
                                 }


                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 ###use boostrap observed data to fit model
                                 #use this model to predict all missing whole data and match with all observed whole data
                                 yhatobs=predict(xgb.fit,Obs.data)
                                 yhatobs.list[[k]][[i]]=yhatobs

                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit


                               }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){
                                   if(self$pmm.link=="logit"){
                                     obj.type<-"binary:logitraw"
                                   }else{
                                     #pmm by "prob"
                                     obj.type<-"binary:logistic"
                                   }
                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)




                                   yhatobs=predict(xgb.fit,Obs.data)
                                   yhatobs.list[[k]][[i]]=yhatobs

                                   #save model for the k'th imputed dataset, the i'th variable
                                   saved.models[[k]][[i]]<-xgb.fit

                                 }else{
                                   #skip xgboost training, just impute majority class

                                   saved.models[[k]][[i]]<-names(t[1])
                                 }





                               }else{


                                 obj.type= "multi:softprob"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)


                                 yhatobs=predict(xgb.fit,Obs.data,reshape = T)


                                 if(self$pmm.link=="logit"){

                                   yhatobs=log(yhatobs/(1-yhatobs))
                                 }

                                 yhatobs.list[[k]][[i]]=yhatobs


                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit
                               }







                             }

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]
                           params$yhatobs.list=yhatobs.list



                         }

                          return(list("imputed.data"=imputed.data,"saved.models"=saved.models,"params"=params))


                       }else if(self$pmm.type=="auto"){

                         saved.models<-replicate(m,list())
                         yhatobs.list<-replicate(m,list())
                         yobs.list<-list()


                         for(i in 1:p){

                           if(type[i]=="numeric"){

                             na.index=Na.index[[i]]

                             if(length(na.index)>0){
                               obs.y=sorted.df[,i][-na.index]
                               yobs.list[[i]]=obs.y
                             }else{
                               obs.y=sorted.df[,i]
                               yobs.list[[i]]=obs.y
                             }


                           }

                         }

                         params$yobs.list=yobs.list





                         for(k in 1:m){


                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]

                           copy=initial.df

                           for(i in 1:p){
                             Boot.initial=copy[index,]

                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=Na.index[[i]]

                             if(length(Bna.index)==Nrow | length(Bna.index)==Nrow-1){
                               stop("At least one variable in the boostrapped sample has 100% missing values or only one observed entry.\n This implies that there is at least one variable in the original dataset has too many missing entries.\n Imputation procedure aborts.\n Please consider removing variables with too many missing values before imputation.")
                             }


                             if(length(na.index)>0){

                               if(length(Bna.index)==0){

                                 obs.y=Boot.data[,i]
                               }else{
                                 obs.y=Boot.data[,i][-Bna.index]
                               }


                               if(type[i]!="numeric"){
                                 obs.y=as.integer(obs.y)-1
                               }



                               if(type[i]=="numeric"){
                                 #pmm type 2 for continuous variables

                                 if(length(Bna.index)==0){
                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                     Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                     Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                   }
                                 }else{
                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                     Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                     Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                   }
                                 }



                                 if(length(na.index)==1){
                                   mis.data=t(mis.data)
                                 }




                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 ###use boostrap observed data to fit model
                                 #use this model to predict all missing whole data and match with all observed whole data
                                 yhatobs=predict(xgb.fit,Obs.data)
                                 yhatobs.list[[k]][[i]]=yhatobs
                                 #update dataset
                                 copy[,i][na.index]<-pmm(yhatobs = yhatobs,yhatmis = pred.y,yobs=yobs.list[[i]],k=self$pmm.k)
                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit


                               }else if(type[i]=="binary"){

                                 if(length(Bna.index)==0){
                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                   }


                                 }else{
                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                   }

                                 }


                                 if(length(na.index)==1){
                                   mis.data=t(mis.data)
                                 }

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){

                                   obj.type<-"binary:logistic"

                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)
                                   pred.y=ifelse(xgb.pred>=0.5,1,0)

                                   #update dataset
                                   copy[,i][na.index]<-levels(sorted.df[,i])[pred.y+1]
                                   #save model for the k'th imputed dataset, the i'th variable
                                   saved.models[[k]][[i]]<-xgb.fit

                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])
                                   saved.models[[k]][[i]]<-names(t[1])
                                 }







                               }else{

                                 if(length(Bna.index)==0){
                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                   }
                                 }else{
                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                     mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                   }

                                 }


                                 if(length(na.index)==1){
                                   mis.data=t(mis.data)
                                 }


                                 obj.type= "multi:softmax"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,mis.data)
                                 #update dataset
                                 copy[,i][na.index]<-levels(sorted.df[,i])[xgb.pred+1]
                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit

                               }

                             }else{
                               #If there is no NAs in this variable



                                 obs.y=Boot.data[,i]



                               if(type[i]!="numeric"){
                                 obs.y=as.integer(obs.y)-1
                               }



                               if(type[i]=="numeric"){
                                 #pmm type 2 for continuous variables


                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)
                                     Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df)

                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]
                                     Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df)[,-1]

                                   }


                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                 ###use boostrap observed data to fit model
                                 #get the predicted values of observed data
                                 yhatobs=predict(xgb.fit,Obs.data)
                                 #original observed values
                                 yhatobs.list[[k]][[i]]=yhatobs

                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit


                               }else if(type[i]=="binary"){


                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)

                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]

                                   }




                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){

                                   obj.type<-"binary:logistic"

                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   eval_metric ="logloss",nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)


                                   #save model for the k'th imputed dataset, the i'th variable
                                   saved.models[[k]][[i]]<-xgb.fit

                                 }else{
                                   #skip xgboost training, just impute majority class

                                   saved.models[[k]][[i]]<-names(t[1])
                                 }







                               }else{


                                   if(p==2){
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)

                                   }else{
                                     obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial)[,-1]

                                   }




                                 obj.type= "multi:softmax"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,tree_method=self$tree_method, gpu_id=self$gpu_id, predictor=self$predictor,verbose = self$verbose, print_every_n = self$print_every_n)

                                 #save model for the k'th imputed dataset, the i'th variable
                                 saved.models[[k]][[i]]<-xgb.fit

                               }

                             }

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]

                           params$yhatobs.list=yhatobs.list

                         }

                         return(list("imputed.data"=imputed.data,"saved.models"=saved.models,"params"=params))


                       }
                        #end of else pmm not null else if pmm.type==1 else pmm.type==2 else "auto"



                      }#end of if single else multiple

                      }#end of impute function

                  )
)



