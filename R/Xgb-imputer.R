#' Multiple imputation through xgboost R6 class imputer object
#' @docType  class
#' @description Set up an xgboost imputer object with specified hyperparameters and then obtain multiple imputed datasets
#' @format  NULL
#' @import xgboost data.table
#' @export

Mixgb <- R6Class("Mixgb",
  cloneable = FALSE,
  public = list(
    data = NULL,
    xgb.params = NULL,
    nrounds = NULL,
    print_every_n = NULL,
    verbose = NULL,
    early_stopping_rounds = NULL,
    pmm.k = NULL,
    pmm.type = NULL,
    pmm.link = NULL,
    initial.num = NULL,
    initial.fac = NULL,
    initial.obj = NULL,
    bootstrap = NULL,


    #' @description Create a new \code{Mixgb} object. This is used to set up the multiple imputation imputer using xgboost.
    #' @examples
    #' MIXGB=Mixgb$new(withNA.dt)
    #' MIXGB=Mixgb$new(withNA.dt,nrounds=50,max_depth=6)
    #' @param data A data.frame or data.table with missing values
    #' @param xgb.params A list of Hyperparameters values for xgboost
    #' @param nrounds Max number of boosting iterations. Default: 50
    #' @param early_stopping_rounds Default: 10,
    #' @param print_every_n Default: 10L
    #' @param verbose Default: 0
    #' @param pmm.k Default: 5
    #' @param pmm.type Default: "auto". (can be 0,1,2 or "auto")
    #' @param pmm.link Default: "prob". (can be "prob" or "logit")
    #' @param initial.num initial imputation method for numeric type data ("normal","mean","median","mode","sample"). Default: "normal"
    #' @param initial.fac initial imputation method for factor type data ("mode","sample"). Default: "mode"
    #' @param initial.obj An object including data structure and a pre-fill dataset after initial imputation
    #' @param bootstrap Whether use bootstrapping to obtain randomness for multiple imputation. Default: TRUE. If FALSE, user need to specify sample related hyperparameters of xgboost to obtain different imputations, otherwise m imputations would be identical.



    initialize = function(data, xgb.params = list(max_depth = 6, gamma = 0.1, eta = 0.3, min_child_weight = 1, subsample = 1, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode, tree_method = "auto", gpu_id = 0, predictor = "auto"),
                          nrounds = 50, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0,
                          pmm.k = 5, pmm.type = "auto", pmm.link = "prob",
                          initial.num = "normal", initial.fac = "mode", bootstrap = TRUE) {
      self$bootstrap <- bootstrap
      self$initial.num <- initial.num
      self$initial.fac <- initial.fac
      # initial imputation
      initial.obj <- initial_imp(data, initial.num = initial.num, initial.fac = initial.fac, bootstrap = bootstrap)
      self$initial.obj <- initial.obj

      sorted.naSums <- initial.obj$sorted.naSums
      sorted.types <- initial.obj$sorted.types
      Nrow <- initial.obj$Nrow
      # Ncol<-initial.obj$Ncol

      # checking
      check_pmm(pmm.type = pmm.type, bootstrap = bootstrap, xgb.params = xgb.params, Nrow = Nrow, sorted.naSums = sorted.naSums, sorted.types = sorted.types, pmm.k = pmm.k)

      self$xgb.params <- xgb.params
      self$nrounds <- nrounds
      self$early_stopping_rounds <- early_stopping_rounds
      self$print_every_n <- print_every_n
      self$verbose <- verbose

      self$pmm.k <- pmm.k
      self$pmm.type <- pmm.type
      self$pmm.link <- pmm.link
    },

    #' @description Use the imputer to impute missing values and obtain multiple datasets
    #' @examples
    #' MIXGB=Mixgb$new(withNA.dt)
    #' imputation.list=MIXGB$impute(m = 5)
    #' @param m the number of imputed datasets. Default: 5
    #' @param maxit number of imputation iterations. Default: 1
    #' @param save.models whether or not to save models to impute new data later on. Default: FALSE.
    #' @param save.vars the names or indices of variables in the original dataset that users want to save models for later imputation. By default, save.vars=NULL,response models for columns with missing values will be saved. If want to save all models, please specify save.vars=colnames(data). `save.vars` must contains all columns with missing values.
    #' @param ... extra arguments passed to xgboost
    #'
    impute = function(m = 5, maxit = 1, save.models = FALSE, save.vars = NULL, ...) {
      imputed.data <- vector("list", m)
      # initial.obj<-MIXGB$initial.obj
      initial.obj <- self$initial.obj



      origin.names <- initial.obj$origin.names
      sorted.types <- initial.obj$sorted.types
      sorted.names <- initial.obj$sorted.names
      sorted.naSums <- initial.obj$sorted.naSums
      ###
      sorted.idx <- initial.obj$sorted.idx

      missing.idx <- initial.obj$missing.idx
      missing.vars <- initial.obj$missing.vars
      missing.types <- initial.obj$missing.types
      Obs.idx <- initial.obj$Obs.idx
      Na.idx <- initial.obj$Na.idx
      Ncol <- initial.obj$Ncol
      Nrow <- initial.obj$Nrow
      mp <- initial.obj$mp

      pmm.type <- self$pmm.type
      pmm.link <- self$pmm.link
      pmm.k <- self$pmm.k
      bootstrap <- self$bootstrap

      # validate save.vars :
      # keep.vars for future development: skip saving some NAs columns if you know that future data don't have NAs in this columns (not recommend to use it though)
      if (save.models == TRUE) {
        extra.vars <- save_vars(save.vars = save.vars, origin.names = origin.names, missing.vars = missing.vars)
        extra.types <- sorted.types[extra.vars]
        save.vars <- c(missing.vars, extra.vars)
        save.p <- length(save.vars)
        # for each variable in save.vars, save its observed values for PMM
        yobs.list <- vector("list", save.p)
        names(yobs.list) <- save.vars
        # variables with NAs
        for (var in missing.vars) {
          na.idx <- Na.idx[[var]]
          # obs.y
          yobs.list[[var]] <- initial.obj$sorted.dt[[var]][-na.idx]
        }
        # variables fully observed
        for (var in extra.vars) {
          yobs.list[[var]] <- initial.obj$sorted.dt[[var]]
        }
      } else {
        # save.models=FALSE
        extra.vars <- NULL
        extra.types <- NULL
        yobs.list <- vector("list", mp)
        names(yobs.list) <- missing.vars
        for (var in missing.vars) {
          na.idx <- Na.idx[[var]]
          # obs.y
          yobs.list[[var]] <- initial.obj$sorted.dt[[var]][-na.idx]
        }
      }


      yhatobs.list <- NULL
      if (isTRUE(pmm.type == 1)) {
        yhatobs.list <- save_yhatobs(
          yobs.list = yobs.list, maxit = maxit, pmm.link = pmm.link, sorted.dt = initial.obj$sorted.dt, missing.vars = missing.vars, extra.vars = extra.vars, extra.types = extra.types, sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
          xgb.params = self$xgb.params,
          nrounds = self$nrounds, early_stopping_rounds = self$early_stopping_rounds, print_every_n = self$print_every_n, verbose = self$verbose, ...
        )
      }

      # yhatobs.list<-save_yhatobs(yobs.list=yobs.list,maxit=maxit,pmm.link=pmm.link, sorted.dt=initial.obj$sorted.dt, missing.vars=missing.vars, extra.vars=extra.vars,extra.types=extra.types,sorted.names=sorted.names,Na.idx=Na.idx,missing.types=missing.types,Ncol=Ncol,
      # xgb.params = list(max_depth = 6, gamma = 0.1, eta = 0.3, colsample_bytree = 1, min_child_weight = 1, subsample = 1, tree_method = "auto", gpu_id = 0, predictor = "auto", scale_pos_weight = 1),
      # nrounds = 50, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0)

      # ............................................................................................................
      if (save.models == FALSE) {
        # Default: don't save any models


        if (bootstrap == FALSE) {
          # bootstrap=FALSE--------------------------------------------------------------
          for (i in seq_len(m)) {
            cat("Imputing dataset", i, "with mixgb without bootstrap\n")
            # feed in the initial imputed dataset
            sorted.dt <- initial.obj$sorted.dt
            for (j in seq_len(maxit)) {
              sorted.dt <- mixgb(
                pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list,
                sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
                Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
                xgb.params = self$xgb.params,
                nrounds = self$nrounds, early_stopping_rounds = self$early_stopping_rounds, print_every_n = self$print_every_n, verbose = self$verbose, ...
              )
            }
            imputed.data[[i]] <- sorted.dt[, ..origin.names]
          }
        } else {
          # bootstrap=TRUE--------------------------------------------------------------
          for (i in seq_len(m)) {
            cat("Imputing dataset", i, "with mixgb with bootstrap\n")
            # feed in the initial imputed dataset
            sorted.dt <- initial.obj$sorted.dt
            boot.result <- boot(Nrow = Nrow, sorted.dt = sorted.dt, sortedNA.dt = initial.obj$sortedNA.dt, missing.vars = missing.vars, mp = mp)
            for (j in seq_len(maxit)) {
              sorted.dt <- mixgb_boot(
                BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
                yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars,
                sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
                xgb.params = self$xgb.params,
                nrounds = self$nrounds, early_stopping_rounds = self$early_stopping_rounds, print_every_n = self$print_every_n, verbose = self$verbose, ...
              )
            }
            imputed.data[[i]] <- sorted.dt[, ..origin.names]
          }
        } # end of if(bootstrap)


        return(imputed.data)
      } else {
        # save.models=TRUE, save models for imputing new data..........................................................................................
        # For save.models.............................................................................................
        ## Change indices into "names" and validate the argument save.vars,
        # save.vars=c(3,4,10,14)
        # save.vars=c(2,4)

        # var="age"
        # XGB.modles[[i]][var]    return a list $age   with response model for age
        # XGB.modles[[i]][[var]]    response model for age

        # ..........................................................................................................
        # save params of this imputer for impute.new().....................................................
        params <- list()
        # initial imputation methods
        params$initial.num <- self$initial.num
        params$initial.fac <- self$initial.fac
        # PMM settings
        params$pmm.k <- self$pmm.k
        params$pmm.type <- self$pmm.type
        params$pmm.link <- self$pmm.link
        # number of imputations
        params$m <- m
        # number of iterations
        params$maxit <- maxit
        params$sorted.names <- sorted.names
        params$sorted.types <- sorted.types
        params$sorted.naSums <- sorted.naSums

        params$save.vars <- save.vars
        params$missing.vars <- missing.vars
        params$extra.vars <- extra.vars


        ##
        params$Na.idx <- Na.idx
        params$sorted.idx <- sorted.idx
        params$Obs.idx <- Obs.idx
        params$bootstrap <- self$bootstrap
        params$yobs.list <- yobs.list



        # pre-allocation for saved models: for each of m, save Nmodels
        XGB.models <- vector("list", m)

        #--------------------------------------------------------
        if (bootstrap == FALSE) {
          # bootstrap=FALSE--------------------------------------------------------------
          for (i in seq_len(m)) {
            cat("Imputing dataset", i, "with mixgb without bootstrap\n")
            # feed in the initial imputed dataset
            sorted.dt <- initial.obj$sorted.dt
            if (maxit > 1) {
              for (j in seq_len(maxit - 1)) {
                # for j=1:(maxit-1)  only update the imputed dataset
                sorted.dt <- mixgb(
                  pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars,
                  sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
                  xgb.params = self$xgb.params,
                  nrounds = self$nrounds, early_stopping_rounds = self$early_stopping_rounds, print_every_n = self$print_every_n, verbose = self$verbose, ...
                )
              }
            }
            # the last iteration, update the imputed dataset, also save models
            saved.obj <- mixgb_save(
              save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
              yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
              Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
              xgb.params = self$xgb.params,
              nrounds = self$nrounds, early_stopping_rounds = self$early_stopping_rounds, print_every_n = self$print_every_n, verbose = self$verbose, ...
            )
            # if pmm.type=NULL
            imputed.data[[i]] <- saved.obj$sorted.dt[, ..origin.names]
            XGB.models[[i]] <- saved.obj$xgb.models
            # if pmm.type=0,2,auto
            if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2) | isTRUE(pmm.type == "auto")) {
              yhatobs.list[[i]] <- saved.obj$yhatobs.list
            }
            # if pmm.type=1  (only use one set of yhatobs.list across all m, it's saved ahead)
          }
        } else {
          # bootstrap=TRUE--------------------------------------------------------------
          for (i in seq_len(m)) {
            cat("Imputing dataset", i, "with mixgb with bootstrap\n")
            # feed in the initial imputed dataset
            sorted.dt <- initial.obj$sorted.dt
            boot.result <- boot(Nrow = Nrow, sorted.dt = sorted.dt, sortedNA.dt = initial.obj$sortedNA.dt, missing.vars = missing.vars, mp = mp)
            if (maxit > 1) {
              for (j in seq_len(maxit - 1)) {
                sorted.dt <- mixgb_boot(
                  BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
                  yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars,
                  sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
                  xgb.params = self$xgb.params,
                  nrounds = self$nrounds, early_stopping_rounds = self$early_stopping_rounds, print_every_n = self$print_every_n, verbose = self$verbose, ...
                )
              }
            }
            saved.obj <- mixgb_bootsave(
              BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
              yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
              Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
              xgb.params = self$xgb.params,
              nrounds = self$nrounds, early_stopping_rounds = self$early_stopping_rounds, print_every_n = self$print_every_n, verbose = self$verbose, ...
            )

            # if pmm.type=NULL
            imputed.data[[i]] <- saved.obj$sorted.dt[, ..origin.names]
            XGB.models[[i]] <- saved.obj$xgb.models
            # if pmm.type=0,2,auto
            if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2) | isTRUE(pmm.type == "auto")) {
              yhatobs.list[[i]] <- saved.obj$yhatobs.list
            }
            # if pmm.type=1  (only use one set of yhatobs.list across all m, it's saved ahead)
          }
        } # end of if(bootstrap)



        #---------------------------------------------------------

        # pmm.type=NULL, yhatobs.list=NULL
        # pmm.type=1, yhatobs.list only one list with different vars
        # pmm.type=0,2,"auto" yhatlobs.list has m list with different vars

        params$yhatobs.list <- yhatobs.list
        params$yobs.list <- yobs.list

        return(list("imputed.data" = imputed.data, "XGB.models" = XGB.models, "params" = params))
      }
    } # end of impute function
  )
)
