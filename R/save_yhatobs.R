# A function use to obtain yhatobs using the whole dataset (use for pmm.type=1)
save_yhatobs <- function(Obs.m, matrix.method, cbind.types, all.idx,
                         yobs.list, maxit, pmm.link, sorted.dt, missing.vars, extra.vars = NULL, extra.types = NULL, sorted.names, Na.idx, missing.types, Ncol,
                         xgb.params=list(),
                         nrounds , early_stopping_rounds , print_every_n , verbose,
                         ...) {

  nthread <- xgb.params$nthread
  # param xgb.params NULL if XGBmodels was fed in
  # return a list of yhatobs values for specified variables
  # check whether xgb.params contains sample related hyperparameters, need to coerce to 1 as we want to obtain yhatobs using the whole dataset

  sample.params <- grepl("sample", names(xgb.params))
  if (any(sample.params == TRUE)) {
    xgb.params[sample.params] <- 1
  }

  save.p <- length(missing.vars) + length(extra.vars)
  yhatobs.list <- vector("list", save.p)
  names(yhatobs.list) <- c(missing.vars, extra.vars)

  # general case : save all variables with NAs
  for (i in seq_len(maxit)) {
    for (var in missing.vars) {
      #features <- setdiff(sorted.names, var)
      #form <- reformulate(termlabels = features, response = var)

      na.idx <- Na.idx[[var]]
      obs.y <- yobs.list[[var]]

      Mis.vars<-missing.vars[missing.vars != var]

      if (length(obs.y) == 1) {
        warning(paste("PMM to this dataset may not be sensible as there is only one observed entry in the variable ", var, sep = ""))
      }


      if(matrix.method=="as.matrix"){

        Mis.m<-as.matrix(sorted.dt[,Mis.vars,with = FALSE])


      }else{

        Mis.list <- lapply(Mis.vars, function(feature){

          if(cbind.types[feature] %in% c("numeric","integer")){
            as.matrix(sorted.dt[[feature]])
          } else if(cbind.types[feature] == "ordered"){
            Matrix::t(fac2Sparse(sorted.dt[[feature]], factorPatt12=c(T,F), contrasts.arg = "contr.poly")[[1]])
          } else {
            Matrix::t(fac2sparse(sorted.dt[[feature]]))[, -1, drop = FALSE]
          }
        })


        if(matrix.method=="cpp.combo"){
          Mis.m<-cbind_combo(Mis.list )
        }else if(matrix.method=="cpp.factor"){
          Mis.m<-cbind_sparse_matrix(Mis.list )
        }


      }

      All.m<-cbind2(Mis.m,Obs.m)

      obs.data<-All.m[-na.idx, , drop = FALSE]
      mis.data<-All.m[na.idx, , drop = FALSE]



      if (missing.types[var] == "numeric") {

        dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
        dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)

        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }

        obj.type <- "reg:squarederror"
        xgb.fit <- xgb.train(
          data = dobs, objective = obj.type, watchlist = watchlist,
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )

        if (i != maxit) {
          yhatmis <- predict(xgb.fit, dmis)
          #sorted.dt[[var]][na.idx] <- yhatmis
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          # last iteration
          yhatobs.list[[var]] <- predict(xgb.fit, dobs)
        }
      }else if (missing.types[var] == "integer") {

        dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
        dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)

        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }

        obj.type <- "reg:squarederror"
        xgb.fit <- xgb.train(
          data = dobs, objective = obj.type, watchlist = watchlist,
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )

        if (i != maxit) {
          yhatmis <- predict(xgb.fit, dmis)
          #sorted.dt[[var]][na.idx] <- yhatmis
          # round to integer
          sorted.dt[na.idx, (var) := round(yhatmis)]
        } else {
          # last iteration
          #yhatobs is not rounded as yhatmis is not rounded when pmm is used
          yhatobs.list[[var]] <- predict(xgb.fit, dobs)
        }
      } else if (missing.types[var] == "binary") {
        obs.y <- as.integer(obs.y) - 1
        bin.t <- sort(table(obs.y))

        dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
        dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)

        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }

        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class

        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class
          if (i != maxit) {
            yhatmis <- levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1]
            sorted.dt[na.idx, (var) := yhatmis]
          } else {
            yhatobs.list[[var]] <- rep(levels(sorted.dt[[var]])[as.integer(sorted.names(bin.t[1])) + 1], length(obs.y))
          }

          msg <- paste("The binary variable", var, "in the data only have single class. Imputation models can't be built.")
          stop(msg)

        } else {
          # general case
          if (pmm.link == "logit") {
            # pmm by "logit" value
            obj.type <- "binary:logitraw"
          } else {
            # pmm by "prob" value
            obj.type <- "binary:logistic"
          }
          xgb.fit <- xgb.train(
            data = dobs, objective = obj.type, watchlist = watchlist,
            eval_metric = "logloss",
            params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
            print_every_n = print_every_n, verbose = verbose, ...
          )

          if (i != maxit) {
            yhatmis <- predict(xgb.fit, dmis)
            # if pmm.link="logit", these would be logit values, otherwise would be probability values
            if (pmm.link == "logit") {
              # if prediction is logit
              yhatmis  <- exp(yhatmis) / (1 + exp(yhatmis))
            }
            yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
            yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
            sorted.dt[na.idx, (var) := yhatmis]

          } else {
            #last iteration
            yhatobs.list[[var]] <- predict(xgb.fit, dobs)
          }
        }
      } else if (missing.types[var] == "logical") {
        dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
        dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }

        bin.t <- sort(table(obs.y))
        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class
        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class


          if (i != maxit) {
            yhatmis <- as.logical(names(bin.t[1]))
            sorted.dt[na.idx, (var) := yhatmis]

          } else {
            yhatobs.list[[var]] <- rep(as.logical(names(bin.t[1])), length(obs.y))
          }

          msg <- paste("The logical variable", var, "in the data only have single class. Imputation models can't be built.")
          stop(msg)

        } else {
          # general case
          if (pmm.link == "logit") {
            # pmm by "logit" value
            obj.type <- "binary:logitraw"
          } else {
            # pmm by "prob" value
            obj.type <- "binary:logistic"
          }
          xgb.fit <- xgb.train(
            data = dobs, objective = obj.type, watchlist = watchlist,
            eval_metric = "logloss",
            params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
            print_every_n = print_every_n, verbose = verbose, ...
          )

          if (i != maxit) {
            yhatmis <- predict(xgb.fit, dmis)
            # if pmm.link="logit", these would be logit values, otherwise would be probability values
            if (pmm.link == "logit") {
              # if prediction is logit
              yhatmis<- exp(yhatmis) / (1 + exp(yhatmis))
            }
            yhatmis <- ifelse(yhatmis >= 0.5, T, F)
            sorted.dt[na.idx, (var) := yhatmis]
          } else {
            #last iteration
            yhatobs.list[[var]] <- predict(xgb.fit, dobs)
          }
        }
      } else {
        # multiclass
        obs.y <- as.integer(obs.y) - 1
        yobs.list[[var]] <- obs.y

        dobs <-xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
        dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)

        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }

        if (i != maxit) {
          # predict return class
          obj.type <- "multi:softmax"
        } else {
          # predict return prob
          obj.type <- "multi:softprob"
        }

        N.class <- length(levels(sorted.dt[[var]]))
        xgb.fit <- xgb.train(
          data = dobs, num_class = N.class,
          objective = obj.type, watchlist = watchlist,
          eval_metric = "mlogloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )
        if (i != maxit) {
          # only update datset, prediction return predicted class
          yhatmis <- predict(xgb.fit, mis.data)
          sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]


          yhatmis <- predict(xgb.fit, dmis)
          yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          # prediction returns probability for matching: probability matrix for each class
          yhatobs.list[[var]] <- predict(xgb.fit, dobs, reshape = TRUE)
        }
      }
    } # end of for each missing variable
  }

  # for extra variables -----------------------------------------------------


  if (!is.null(extra.vars)) {
    # if there are extra.vars, use the sorted.dt after the last iteration to obtain models
    if(matrix.method=="as.matrix"){

      All.m<-as.matrix(sorted.dt)

    }else{

      All.list <- lapply(sorted.names, function(feature){

        if(cbind.types[feature] %in% c("numeric","integer")){
          as.matrix(sorted.dt[[feature]])
        } else if(cbind.types[feature] == "ordered"){
          Matrix::t(fac2Sparse(sorted.dt[[feature]], factorPatt12=c(T,F), contrasts.arg = "contr.poly")[[1]])
        } else {
          Matrix::t(fac2sparse(sorted.dt[[feature]]))[, -1, drop = FALSE]
        }
      })


      if(matrix.method=="cpp.combo"){
        All.m<-cbind_combo(All.list )
      }else if(matrix.method=="cpp.factor"){
        All.m<-cbind_sparse_matrix(All.list )
      }

      Ncol.list<-lapply(All.list,ncol)
      end.idx<-cumsum(Ncol.list)
      start.idx<-c(1,(end.idx+1)[-length(end.idx)])
    }



    for (var in extra.vars) {


      obs.y <- yobs.list[[var]]

      if(matrix.method=="as.matrix"){
        var.idx<-all.idx[var]
      }else{
        v<-all.idx[var]
        var.idx<-start.idx[v]:end.idx[v]
      }

      obs.data<-All.m[, -var.idx , drop = FALSE]


      if (extra.types[var] == "numeric" | extra.types[var] == "integer") {

        dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)

        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }

        obj.type <- "reg:squarederror"
        xgb.fit <- xgb.train(
          data = dobs, objective = obj.type, watchlist = watchlist,
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )
        yhatobs.list[[var]] <- predict(xgb.fit, dobs)

      } else if (extra.types[var] == "binary") {
        obs.y <- as.integer(obs.y) - 1
        bin.t <- sort(table(obs.y))
        dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }
        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class
        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class
          yhatobs.list[[var]] <- rep(levels(sorted.dt[[var]])[as.integer(sorted.names(bin.t[1])) + 1], length(obs.y))
        } else {
          # general case
          if (pmm.link == "logit") {
            # pmm by "logit" value
            obj.type <- "binary:logitraw"
          } else {
            # pmm by "prob" value
            obj.type <- "binary:logistic"
          }
          xgb.fit <- xgb.train(
            data = dobs, objective = obj.type, watchlist = watchlist,
            eval_metric = "logloss",
            params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
            print_every_n = print_every_n, verbose = verbose, ...
          )
          # if pmm.link="logit", these would be logit values, otherwise would be probability values
          yhatobs.list[[var]] <- predict(xgb.fit, dobs)
        }
      } else if (extra.types[var] == "logical") {
        dobs <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)

        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }


        bin.t <- sort(table(obs.y))
        # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
        # when bin.t only has one value: bin.t[1] the only existent class
        if (is.na(bin.t[2])) {
          # this binary variable only have one class being observed (e.g., observed values are all "0"s)
          # skip xgboost training, just impute the only existent class
          yhatobs.list[[var]] <- rep(as.logical(names(bin.t[1])), length(obs.y))
        } else {
          # general case
          if (pmm.link == "logit") {
            # pmm by "logit" value
            obj.type <- "binary:logitraw"
          } else {
            # pmm by "prob" value
            obj.type <- "binary:logistic"
          }
          xgb.fit <- xgb.train(
            data = dobs, objective = obj.type, watchlist = watchlist,
            eval_metric = "logloss",
            params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
            print_every_n = print_every_n, verbose = verbose, ...
          )
          # if pmm.link="logit", these would be logit values, otherwise would be probability values
          yhatobs.list[[var]] <- predict(xgb.fit, dobs)
        }
      } else {
        # multiclass
        obs.y <- as.integer(obs.y) - 1
        yobs.list[[var]] <- obs.y

        dobs <-xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
        if (is.null(early_stopping_rounds)) {
          watchlist <- list(train = dobs)
        } else {
          watchlist <- list(train = dobs)
          # to be done, have eval
          # watchlist <- list(train = dobs,eval=dmis)
        }
          #save probability as yhatobs to do pmm matching

          obj.type <- "multi:softprob"


        N.class <- length(levels(sorted.dt[[var]]))
        xgb.fit <- xgb.train(
          data = dobs, num_class = N.class,
          objective = obj.type, watchlist = watchlist,
          eval_metric = "mlogloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )
        # prediction returns probability for matching: probability matrix for each class
        yhatobs.list[[var]] <- predict(xgb.fit, dobs, reshape = TRUE)
      }
    } # end of for each extra variable
  }


  return(yhatobs.list)
}
