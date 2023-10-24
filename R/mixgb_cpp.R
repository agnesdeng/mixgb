# Multiple imputation using xgboost (without bootstrap)

mixgb_cpp <- function(matrix.method, cbind.types,  pmm.type, pmm.link, pmm.k, yobs.list, yhatobs.list = NULL, sorted.dt, missing.vars, sorted.names, Na.idx, missing.types, Ncol,
                        xgb.params = list(),
                        nrounds = 100, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0,
                        ...) {

  # sorted.dt <- copy(sorted.dt)
  nthread <- xgb.params$nthread
  # param yhatobs.list if it is pmm.type 1, must feed in the yhatobs.list
  for (var in missing.vars) {

    features <- setdiff(sorted.names, var)
    form <- reformulate(termlabels = features, response = var)

    na.idx <- Na.idx[[var]]
    obs.y <- yobs.list[[var]]

   if(matrix.method=="as.matrix"){

     obs.data <- as.matrix(sorted.dt[-na.idx, features, with = FALSE])
     mis.data <- as.matrix(sorted.dt[na.idx, features, with = FALSE])

   }else{
     all.list<-vector("list",length(features))
     names(all.list)<-features

     if(length(features) == 1){
       all.list <- list(
         if(cbind.types[features] %in% c("numeric","integer")){
           sorted.dt[[features]]
         } else if(cbind.types[features] == "ordered"){
           t(fac2Sparse(sorted.dt[[features]], factorPatt12=c(T,F), contrasts.arg = "contr.poly")[[1]])
         } else {
           t(fac2sparse(sorted.dt[[features]]))[, -1, drop = FALSE]
         }
       )
     } else {

       all.list <- lapply(features, function(feature){

         if(cbind.types[feature] %in% c("numeric","integer")){
           sorted.dt[[feature]]
         } else if(cbind.types[feature] == "ordered"){
           Matrix::t(fac2Sparse(sorted.dt[[feature]], factorPatt12=c(T,F), contrasts.arg = "contr.poly")[[1]])
         } else {
           Matrix::t(fac2sparse(sorted.dt[[feature]]))[, -1, drop = FALSE]
         }
       })
     }



     if(matrix.method=="cpp.combo"){
       all.m<-cbind_combo(all.list)
     }else if(matrix.method=="cpp.factor"){
       all.m<-cbind_sparse_matrix(all.list)
     }



     #mis.data0
     obs.data<-all.m[-na.idx, , drop = FALSE]
     mis.data<-all.m[na.idx, , drop = FALSE]

   }









# start -------------------------------------------------------------------







    # numeric or integer ---------------------------------------------------------------------------
    if (missing.types[var] == "numeric" | missing.types[var] == "integer") {
      dtrain <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dtest <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dtrain)
      } else {
        watchlist <- list(train = dtrain)
        # to be done, have eval
        # watchlist <- list(train = dtrain,eval=dtest)
      }


      obj.type <- "reg:squarederror"
      xgb.fit <- xgb.train(
        data = dtrain, objective = obj.type, watchlist = watchlist,
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
        print_every_n = print_every_n, verbose = verbose, ...
      )

      yhatmis <- predict(xgb.fit, dtest)
      if (!is.null(pmm.type)) {
        if (pmm.type != 1) {
          # for pmm.type=0 or 2 or auto (type 2 for numeric or integer)
          yhatobs <- predict(xgb.fit, obs.data)
        } else {
          # for pmm.type=1
          yhatobs <- yhatobs.list[[var]]
        }
        yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = obs.y, k = pmm.k)
      }
      # update dataset

      sorted.dt[na.idx, (var) := yhatmis]
    } else if (missing.types[var] == "binary") {
      # binary ---------------------------------------------------------------------------
      obs.y <- as.integer(obs.y) - 1
      bin.t <- sort(table(obs.y))


      dtrain <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dtest <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dtrain)
      } else {
        watchlist <- list(train = dtrain)
        # to be done, have eval
        # watchlist <- list(train = dtrain,eval=dtest)
      }

      # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
      # when bin.t only has one value: bin.t[1] the only existent class
      if (is.na(bin.t[2])) {
        # this binary variable only has a single class being observed (e.g., observed values are all "0"s)
        # skip xgboost training, just impute the only existent class
        yhatmis <- levels(sorted.dt[[var]])[as.integer(names(bin.t[1])) + 1]
        sorted.dt[na.idx, (var) := yhatmis]
        msg <- paste("The binary variable", var, "in the data only have single class. Imputation models can't be built.")
        stop(msg)
      } else {
        if (!is.null(pmm) & pmm.link == "logit") {
          # pmm by "logit" value
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" and for no pmm
          obj.type <- "binary:logistic"
        }
        xgb.fit <- xgb.train(
          data = dtrain, objective = obj.type, watchlist = watchlist,
          eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )

        yhatmis <- predict(xgb.fit, dtest)

        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
          yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatobs <- yhatobs.list[[var]]
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, obs.data)
          }


          yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          sorted.dt[na.idx, (var) := yhatmis]
        }
      }
    } else if (missing.types[var] == "logical") {
      dtrain <- xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dtest <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dtrain)
      } else {
        watchlist <- list(train = dtrain)
        # to be done, have eval
        # watchlist <- list(train = dtrain,eval=dtest)
      }

      bin.t <- sort(table(obs.y))
      # when bin.t has two values: bin.t[1] minority class & bin.t[2] majority class
      # when bin.t only has one value: bin.t[1] the only existent class
      if (is.na(bin.t[2])) {
        # this binary variable only has a single class being observed (e.g., observed values are all "0"s)
        # skip xgboost training, just impute the only existent class
        yhatmis <- as.logical(names(bin.t[1]))
        sorted.dt[na.idx, (var) := yhatmis]
        msg <- paste("The logical variable", var, "in the data only have single class. Imputation models can't be built.")
        stop(msg)
      } else {
        if (!is.null(pmm) & pmm.link == "logit") {
          # pmm by "logit" value
          obj.type <- "binary:logitraw"
        } else {
          # pmm by "prob" and for no pmm
          obj.type <- "binary:logistic"
        }

        xgb.fit <- xgb.train(
          data = dtrain, objective = obj.type, watchlist = watchlist,
          eval_metric = "logloss",
          params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
          print_every_n = print_every_n, verbose = verbose, ...
        )
        yhatmis <- predict(xgb.fit, dtest)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, T, F)
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatobs <- yhatobs.list[[var]]
          } else {
            # for pmm.type=0 or 2
            yhatobs <- predict(xgb.fit, obs.data)
          }

          yhatmis <- pmm(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          sorted.dt[na.idx, (var) := yhatmis]
        }
      }
    } else {
      # multiclass ---------------------------------------------------------------------------
      obs.y <- as.integer(obs.y) - 1

      dtrain <-xgb.DMatrix(data = obs.data, label = obs.y, nthread = nthread)
      dtest <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(early_stopping_rounds)) {
        watchlist <- list(train = dtrain)
      } else {
        watchlist <- list(train = dtrain)
        # to be done, have eval
        # watchlist <- list(train = dtrain,eval=dtest)
      }

      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        obj.type <- "multi:softmax"
      } else {
        # use probability to do matching
        obj.type <- "multi:softprob"
      }
      N.class <- length(levels(sorted.dt[[var]]))


      xgb.fit <- xgb.train(
        data = dtrain, num_class = N.class,
        objective = obj.type, watchlist = watchlist,
        eval_metric = "mlogloss",
        params = xgb.params, nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
        print_every_n = print_every_n, verbose = verbose, ...
      )
      # yhatmis <- predict(xgb.fit, dtest)

      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        # use softmax, predict returns class
        # for pmm.type=NULL or "auto"
        yhatmis <- predict(xgb.fit, dtest)
        yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
        sorted.dt[na.idx, (var) := yhatmis]
      } else {
        # predict returns probability matrix for each class
        # yhatmis <- predict(xgb.fit, mis.data, reshape = TRUE)
        # hasn't tested yet
        yhatmis <- predict(xgb.fit, dtest, reshape = TRUE)
        if (pmm.type == 1) {
          # for pmm.type=1
          yhatobs <- yhatobs.list[[var]]
        } else {
          # for pmm.type=0 or 2
          # probability matrix for each class
          # yhatobs <- predict(xgb.fit, obs.data, reshape = TRUE)
          # hasn't tested yet
          yhatobs <- predict(xgb.fit, dtrain, reshape = TRUE)
        }
        yhatmis <- pmm.multiclass(yhatobs = yhatobs, yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        yhatmis <- levels(sorted.dt[[var]])[yhatmis]
        sorted.dt[na.idx, (var) := yhatmis]
      }
    }
    # for each var
  }
  # end of for each missing variable
  sorted.dt
}
