#' Multiple imputation through xgboost R6 class imputer object
#' @docType  class
#' @format  An [R6Class] mixgb imputer object
#' @import xgboost
# @return [Mixgb]
#' @export

Mixgb <- R6Class("Mixgb",
                   cloneable = FALSE,

                    public = list(
                      #'@field data A data frame with missing values
                      #'@field nrounds max number of boosting iterations. Default: 50
                      #'@field max_depth maximum depth of the tree. Default: 6
                      #'@field gamma Default: 0.1
                      #'@field eta Default: 0.3
                      #'@field nthread Default: 4
                      #'@field early_stopping_rounds Default: 10,
                      #'@field colsample_bytree Default: 1
                      #'@field min_child_weight Default: 1
                      #'@field subsample Default: 1
                      #'@field pmm.k Default: 5
                      #'@field pmm.type Default: "auto" (used to be NULL). "auto": pmm.type2 for continuous, no pmm for categorical
                      #'@field pmm.link match on predictive mean of "logit" or "prob".Default: "logit"
                      #'@field scale_pos_weight Default: 1
                      #'@field initial.imp Default: "random"
                      #'@field print_every_n Default: 10L
                      #'@field verbose Default: 1

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
                    #'@description Create a new \code{Mixgb} object. This is used to set up the multiple imputation imputer using xgboost.
                    #'@examples
                    #'MIXGB=Mixgb$new(withNA.df)
                    #'MIXGB=Mixgb$new(withNA.df,nrounds=50,max_depth=6)
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
                    #'@param verbose Default: 1
                    #'@param scale_pos_weight Default:1



                    initialize = function(data,nrounds=50,max_depth=6,gamma=0.1,eta=0.3,nthread=4,early_stopping_rounds=10,colsample_bytree=1,min_child_weight=1,subsample=1,pmm.k=5,pmm.type="auto",pmm.link="logit",scale_pos_weight=1,initial.imp="random",print_every_n = 10L,verbose=0) {
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

                    },

                    #'@description Use the imputer to impute missing values and obtain multiple datasets
                    #'@examples
                    #'MIXGB=Mixgb$new(withNA.df)
                    #'imputation.list=MIXGB$impute(m = 5)
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

                      for(i in 1:p){

                        if(self$initial.imp=="random"){

                            index=which(!is.na(sorted.df[,i]))
                            initial.df[is.na(sorted.df[,i]),i]<-sample(sorted.df[,i][index],num.na[i],replace=TRUE)


                        }else if(self$initial.imp=="rnorm"){

                          if(type[i]=="numeric"){
                            var.mean=mean(sorted.df[,i],na.rm = T)
                            var.sd=sd(sorted.df[,i],na.rm = T)
                            index=which(is.na(sorted.df[,i]))
                            initial.df[index,i]<-rnorm(num.na[i],var.mean,var.sd)
                          }else{
                            index=which(!is.na(sorted.df[,i]))
                            initial.df[is.na(sorted.df[,i]),i]<-sample(sorted.df[,i][index],num.na[i],replace=TRUE)
                          }



                          }else{


                          #numeric: initial impute median
                          if(type[i]=="numeric"){
                            initial.df[is.na(sorted.df[,i]),i]<-median(sorted.df[,i],na.rm = T)
                          }else{
                            #factor:  initial impute major class
                            initial.df[is.na(sorted.df[,i]),i]<-names(which.max(table(initial.df[,i])))
                          }
                        }


                      }

                      #3) imputation  i=2

                      if(m==1){

                        if(is.null(self$pmm.type)){
                          #no pmm for single imputation
                          for(i in 1:p){
                            na.index=which(is.na(sorted.df[,i]))
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

                              if(type[i]=="numeric"){
                                obj.type<-"reg:squarederror"
                                xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                pred.y=predict(xgb.fit,mis.data)
                                #update dataset
                                sorted.df[,i][na.index]<-pred.y

                              }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class



                                  if(!is.na(t[2])){
                                    obj.type<-"binary:logistic"
                                    xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                    nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                    min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,verbose = self$verbose, print_every_n = self$print_every_n)

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
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
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
                          yhatobs.list<-list()
                          yobs.list<-list()
                          for(i in 1:p){
                            na.index=which(is.na(sorted.df[,i]))
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

                              if(type[i]=="numeric"){
                                obj.type<-"reg:squarederror"
                                xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
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
                                                  nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                  min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,verbose = self$verbose, print_every_n = self$print_every_n)

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
                                                min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
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

                        #### m multiple imputation
                       if(is.null(self$pmm.type)){
                       ###no pmm
                         imputed.data<-list()


                         for(k in 1:m){
                          #sorted.df :  sorted dataset with increasing %NA , containing NA
                          #initial.df:  sorted.df with initial NA imputed
                          #Boot.data:  bootstrap sample of sorted.df,  containing NAs
                           #Boot.initial: bootstrap sample of initial.df,  with NA being imputed with Mean/Median etc
                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]
                           #Boot.initial
                           copy=initial.df




                           for(i in 1:p){
                             Boot.initial=copy[index,]
                             #####
                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=which(is.na(sorted.df[,i]))

                             if(length(Bna.index)>0){

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

                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 #update dataset
                                 copy[,i][na.index]<-pred.y


                               }else if(type[i]=="binary"){

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){
                                   obj.type<-"binary:logistic"
                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)
                                   pred.y=ifelse(xgb.pred>=0.5,1,0)
                                   pred.y=levels(sorted.df[,i])[pred.y+1]

                                   #update dataset
                                   copy[,i][na.index]<-pred.y

                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])
                                 }





                               }else{
                                 obj.type= "multi:softmax"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,mis.data)
                                 pred.y=levels(sorted.df[,i])[xgb.pred+1]
                                 #update dataset
                                 copy[,i][na.index]<-pred.y
                               }

                             }

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]



                         }

                         return(imputed.data)


                       }else if(self$pmm.type==1){
                         ####
                         #multiple imputation m=5
                         imputed.data<-list()
                         yhatobs.list<-list()
                         yobs.list<-list()

                         for(i in 1:p){
                           na.index=which(is.na(sorted.df[,i]))
                           if(length(na.index)>0){
                             obs.y=sorted.df[,i][-na.index]

                             if(type[i]!="numeric"){
                               obs.y=as.integer(obs.y)-1
                             }
                             yobs.list[[i]]=obs.y

                             if(p==2){
                               obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                               mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])
                             }else{
                               obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                               mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])[,-1]
                             }

                             if(type[i]=="numeric"){
                               obj.type<-"reg:squarederror"
                               xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                               nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                               min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)

                               yhatobs=predict(xgb.fit,obs.data)
                               #update dataset
                               yhatobs.list[[i]]=yhatobs

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
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,obs.data)
                                 yhatobs.list[[i]]=xgb.pred


                               }else{
                                 #skip xgboost training, just impute majority class
                                 yhatobs.list[[i]]<-rep(names(t[1]),length(obs.y))
                               }



                             }else{
                               obj.type= "multi:softprob"
                               N.class=length(levels(sorted.df[,i]))
                               xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                               nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                               min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                               xgb.pred = predict(xgb.fit,obs.data,reshape = T)

                               if(self$pmm.link=="logit"){
                                 xgb.pred<-log(xgb.pred/(1-xgb.pred))
                               }

                               yhatobs.list[[i]]=xgb.pred

                             }
                           }

                         }




                         for(k in 1:m){

                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]
                           Boot.initial=initial.df[index,]
                           copy=sorted.df

                           for(i in 1:p){
                             ########


                             ###########
                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=which(is.na(sorted.df[,i]))

                             if(length(Bna.index)>0){

                               obs.y=Boot.data[,i][-Bna.index]

                               if(type[i]!="numeric"){
                                 obs.y=as.integer(obs.y)-1
                               }

                               if(p==2){
                                 obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                 mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])
                               }else{
                                 obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                 mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[na.index,])[,-1]
                               }

                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 #update dataset
                                 #copy[,i][na.index]<-pred.y
                                 #pred.y=predict(xgb.fit,mis.data)

                                 #update dataset
                                 copy[,i][na.index]<- pmm(yhatobs = yhatobs.list[[i]],yhatmis = pred.y,yobs=yobs.list[[i]],k=self$pmm.k)


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
                                                   nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)

                                   num.result=pmm(yhatobs = yhatobs.list[[i]],yhatmis = xgb.pred,yobs=yobs.list[[i]],k=self$pmm.k)
                                   #change to factor
                                   copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]


                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])
                                 }





                               }else{
                                 obj.type= "multi:softprob"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred= predict(xgb.fit,mis.data,reshape = T)

                                 if(self$pmm.link=="logit"){
                                   xgb.pred=log(xgb.pred/(1-xgb.pred))
                                 }

                                 num.result=pmm.multiclass(donor.pred = yhatobs.list[[i]],target.pred = xgb.pred,donor.obs = yobs.list[[i]],k=self$pmm.k)
                                 #change to factor
                                 copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]


                               }

                             }

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]



                         }#end m multiple imputation

                         return(imputed.data)

                       }else if(self$pmm.type==2){
                         ###########
                         imputed.data<-list()
                          yobs.list<-list()

                         for(i in 1:p){
                           na.index=which(is.na(sorted.df[,i]))

                            if(length(na.index)>0){
                             obs.y=sorted.df[,i][-na.index]

                             if(type[i]!="numeric"){
                               obs.y=as.integer(obs.y)-1
                             }
                             yobs.list[[i]]=obs.y
                           }
                         }


                         for(k in 1:m){


                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]

                           copy=initial.df

                           for(i in 1:p){
                             Boot.initial=copy[index,]

                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=which(is.na(sorted.df[,i]))

                             if(length(Bna.index)>0){

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

                               if(type[i]=="numeric"){
                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 ###use boostrap observed data to fit model
                                 #use this model to predict all missing whole data and match with all observed whole data
                                  yhatobs=predict(xgb.fit,Obs.data)
                                 #update dataset
                                 copy[,i][na.index]<-pmm(yhatobs = yhatobs,yhatmis = pred.y,yobs=yobs.list[[i]],k=self$pmm.k)



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
                                                   nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)


                                   yhatObs=predict(xgb.fit,Obs.data)

                                   num.result=pmm(yhatobs = yhatObs,yhatmis = xgb.pred,yobs=yobs.list[[i]],k=self$pmm.k)
                                   #change to factor
                                   copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]

                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])
                                 }





                               }else{


                                 obj.type= "multi:softprob"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,mis.data,reshape = T)

                                 yhatObs=predict(xgb.fit,Obs.data,reshape = T)

                                 if(self$pmm.link=="logit"){
                                   xgb.pred=log(xgb.pred/(1-xgb.pred))
                                   yhatObs=log(yhatObs/(1-yhatObs))
                                 }
                                 num.result=pmm.multiclass(donor.pred = yhatObs,target.pred = xgb.pred,donor.obs = yobs.list[[i]],k=self$pmm.k)
                                 #change to factor
                                 copy[,i][na.index]<- levels(sorted.df[,i])[num.result+1]

                               }

                             }

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]



                         }

                         return(imputed.data)


                       }else if(self$pmm.type=="auto"){
                         imputed.data<-list()
                         yobs.list<-list()


                         for(i in 1:p){
                           if(type[i]=="numeric"){
                             na.index=which(is.na(sorted.df[,i]))
                             if(length(na.index)>0){
                               obs.y=sorted.df[,i][-na.index]
                               yobs.list[[i]]=obs.y
                               }
                           }

                         }






                         for(k in 1:m){


                           index=sample(Nrow,Nrow,replace=TRUE)
                           Boot.data=sorted.df[index,]

                           copy=initial.df

                           for(i in 1:p){
                             Boot.initial=copy[index,]

                             Bna.index=which(is.na(Boot.data[,i]))
                             na.index=which(is.na(sorted.df[,i]))

                             if(length(Bna.index)>0){

                               obs.y=Boot.data[,i][-Bna.index]

                               if(type[i]!="numeric"){
                                 obs.y=as.integer(obs.y)-1
                               }



                               if(type[i]=="numeric"){
                                 #pmm type 2 for continuous variables
                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                   Obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=initial.df[-na.index,])[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }

                                 obj.type<-"reg:squarederror"
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 pred.y=predict(xgb.fit,mis.data)
                                 ###use boostrap observed data to fit model
                                 #use this model to predict all missing whole data and match with all observed whole data
                                 yhatobs=predict(xgb.fit,Obs.data)
                                 #update dataset
                                 copy[,i][na.index]<-pmm(yhatobs = yhatobs,yhatmis = pred.y,yobs=yobs.list[[i]],k=self$pmm.k)



                               }else if(type[i]=="binary"){

                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }

                                 t=sort(table(obs.y))
                                 #t[1] minority class t[2]majority class

                                 if(!is.na(t[2])){

                                   obj.type<-"binary:logistic"

                                   xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                   nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                   min_child_weight=self$min_child_weight,subsample=self$subsample,scale_pos_weight=self$scale_pos_weight,verbose = self$verbose, print_every_n = self$print_every_n)

                                   xgb.pred = predict(xgb.fit,mis.data)
                                   pred.y=ifelse(xgb.pred>=0.5,1,0)

                                   #update dataset
                                   copy[,i][na.index]<-levels(sorted.df[,i])[pred.y+1]


                                 }else{
                                   #skip xgboost training, just impute majority class
                                   copy[,i][na.index]<-names(t[1])
                                 }







                               }else{
                                 if(p==2){
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])
                                 }else{
                                   obs.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=Boot.initial[-Bna.index,])[,-1]
                                   mis.data=sparse.model.matrix(as.formula(paste(Names[i],"~.",sep="")),data=copy[na.index,])[,-1]
                                 }



                                 obj.type= "multi:softmax"
                                 N.class=length(levels(sorted.df[,i]))
                                 xgb.fit=xgboost(data=obs.data,label = obs.y,objective = obj.type, num_class=N.class,missing = NA, weight = NULL,nthread=self$nthread,early_stopping_rounds=self$early_stopping_rounds,
                                                 nrounds=self$nrounds, max_depth=self$max_depth,gamma=self$gamma,eta=self$eta,colsample_bytree=self$colsample_bytree,
                                                 min_child_weight=self$min_child_weight,subsample=self$subsample,verbose = self$verbose, print_every_n = self$print_every_n)
                                 xgb.pred = predict(xgb.fit,mis.data)
                                 #update dataset
                                 copy[,i][na.index]<-levels(sorted.df[,i])[xgb.pred+1]


                               }

                             }

                           }
                           imputed.data[[k]]<-copy[order(sorted.idx)]



                         }

                         return(imputed.data)


                       }
                        #end of else pmm not null else if pmm.type==1 else pmm.type==2 else "auto"



                      }#end of if single else multiple

                      }#end of impute function

                  )
)



