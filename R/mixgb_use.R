# Multiple imputation using xgboost (through saved models in local dir)
mixgb_use <- function(nthread,Obs.m, matrix.method, cbind.types,
                           m.set, xgb.models, save.vars, save.p, extra.vars = NULL,
                           extra.types = NULL,
                           pmm.type, pmm.link, pmm.k, yobs.list, yhatobs.list = NULL,
                           sorted.dt, missing.vars, new.missing.vars, sorted.names, new.Na.idx, new.missing.types, Ncol) {
  # param m.set the ith imputation
  # param yhatobs.list if it is pmm.type 1, must feed in the yhatobs.list
  # param yobs.list  observed values in the original training data
  # param yhatobs.list predicted observed values in the original training data,
  # param sorted.dt sorted new data after initial imputation (with the same order as the original data)
  # param missing.vars names of variables with missing values in new data
  # param missing.types types of variable with missing values in new data
  # param sorted.names all names of variables in sorted order
  # param Ncol number of columns in new data

  for (var in new.missing.vars) {

    #features <- setdiff(sorted.names, var)
    #form <- reformulate(termlabels = features, response = var)

    na.idx <- new.Na.idx[[var]]

    #originally missing variables in the original training dataset
    Mis.vars<-missing.vars[missing.vars != var]


    if(matrix.method=="as.matrix"){

      Mis.m<-as.matrix(sorted.dt[,Mis.vars,with = FALSE])


    }else{

      Mis.list <- lapply(Mis.vars, function(feature){

        if(cbind.types[feature] %in% c("numeric","integer")){
          as.matrix(sorted.dt[[feature]])
        } else if(cbind.types[feature] == "ordered"){
          Matrix::t(fac2Sparse(sorted.dt[[feature]], drop.unused.levels = FALSE, factorPatt12=c(T,F), contrasts.arg = "contr.poly")[[1]])
        } else {
          Matrix::t(fac2sparse(sorted.dt[[feature]], drop.unused.levels = FALSE))[, -1, drop = FALSE]
        }
      })


      if(matrix.method=="cpp.combo"){
        Mis.m<-cbind_combo(Mis.list )
      }else if(matrix.method=="cpp.factor"){
        Mis.m<-cbind_sparse_matrix(Mis.list )
      }


    }

    All.m<-cbind2(Mis.m,Obs.m)

    mis.data<-All.m[na.idx, , drop = FALSE]


    # numeric or integer ---------------------------------------------------------------------------
    if (new.missing.types[var] == "numeric") {
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      yhatmis <- predict(xgb.models[[var]], dmis)

      if (!is.null(pmm.type)) {
        if (isTRUE(pmm.type == 1)) {
          yhatmis <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        } else {
          yhatmis <- pmm(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
      }
      # update dataset
      #sorted.dt[[var]][na.idx] <- yhatmis
      sorted.dt[na.idx, (var) := yhatmis]
    }else if (new.missing.types[var] == "integer") {
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      yhatmis <- predict(xgb.models[[var]], dmis)

      if (!is.null(pmm.type)) {
        if (isTRUE(pmm.type == 1)) {
          yhatmis <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        } else {
          yhatmis <- pmm(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
      }
      # update dataset
      #sorted.dt[[var]][na.idx] <- yhatmis
      sorted.dt[na.idx, (var) := round(yhatmis)]

    } else if (new.missing.types[var] == "binary") {
      # binary ---------------------------------------------------------------------------
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (length(xgb.models[[var]]) != 1) {
        # load model
        yhatmis <- predict(xgb.models[[var]], dmis)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
          yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatmis <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          } else {
            # for pmm.type=0 or 2
            yhatmis <- pmm(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          }
          sorted.dt[na.idx, (var) := yhatmis]
        }
      } else {
        # load majority class
        #sorted.dt[[var]][na.idx] <- xgb.models[[var]]
        yhatmis<- xgb.models[[var]]
        sorted.dt[na.idx, (var) := yhatmis]
        msg <- paste("Imputation for variable", var, "use the only existent class in the bootstrap sample. May not be reliable.")
        warning(msg)
      }
    } else if (new.missing.types[var] == "logical") {
      # logical ---------------------------------------------------------------------------
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (length(xgb.models[[var]]) != 1) {
        yhatmis <- predict(xgb.models[[var]], dmis)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, T, F)
          sorted.dt[na.idx, (var) := yhatmis]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            yhatmis <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          } else {
            # for pmm.type=0 or 2
            yhatmis <- pmm(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          }
          sorted.dt[na.idx, (var) := yhatmis]
        }
      } else {
        yhatmis <- xgb.models[[var]]
        sorted.dt[na.idx, (var) := yhatmis]
        msg <- paste("Imputation for variable", var, "use the only existent class in the bootstrap sample. May not be reliable.")
        warning(msg)
      }
    } else {
      # multiclass ---------------------------------------------------------------------------
      dmis <- xgb.DMatrix(data = mis.data, nthread = nthread)
      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        # use softmax, predict returns class
        # for pmm.type=NULL or "auto"
        yhatmis <- predict(xgb.models[[var]], dmis)
        yhatmis <- levels(sorted.dt[[var]])[yhatmis + 1]
        sorted.dt[na.idx, (var) := yhatmis]

      } else {
        # predict returns probability matrix for each class
        yhatmis <- predict(xgb.models[[var]], dmis, reshape = TRUE)
        if (pmm.type == 1) {
          # for pmm.type=1
          yhatmis <- pmm.multiclass(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        } else {
          # for pmm.type=0 or 2
          yhatmis <- pmm.multiclass(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
        yhatmis <- levels(sorted.dt[[var]])[yhatmis]
        sorted.dt[na.idx, (var) := yhatmis]
      }
    }
  } # end of for each missing variable

  return(sorted.dt)
}
