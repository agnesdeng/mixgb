#' Multiple imputation through XGBoost
#' @description Obtain multiply imputed datasets using XGBoost, with an option to save models for imputing new data later on. Users can choose different settings regarding bootstrapping and predictive mean matching as well as XGBoost hyperparameters.
#' @param data A data.frame or data.table with missing values
#' @param m The number of imputed datasets. Default: 5
#' @param maxit The number of imputation iterations. Default: 1
#' @param ordinalAsInteger Whether to convert ordinal factors to integers. The default setting \code{ordinalAsInteger = TRUE} can speed up the imputation process.
#' @param bootstrap Whether to use bootstrapping for multiple imputation. By default, \code{bootstrap = TRUE}. If \code{FALSE}, users are recommended to specify sampling-related hyperparameters of XGBoost to obtain imputations with adequate variability.
#' @param pmm.type The types of predictive mean matching (PMM). Possible values:
#' \itemize{
#'  \item \code{NULL}: Imputations without PMM;
#'  \item \code{0}: Imputations with PMM type 0;
#'  \item \code{1}: Imputations with PMM type 1;
#'  \item \code{2}: Imputations with PMM type 2;
#'  \item \code{"auto"} (Default): Imputations with PMM type 2 for numeric/integer variables; imputations without PMM for categorical variables.
#' }
#' @param pmm.k The number of donors for predictive mean matching. Default: 5
#' @param pmm.link The link for predictive mean matching binary variables
#' \itemize{
#'  \item \code{"prob"} (Default): use probabilities;
#'  \item \code{"logit"}: use logit values.
#' }
#' @param initial.num Initial imputation method for numeric type data:
#' \itemize{
#'  \item \code{"normal"} (Default);
#'  \item \code{"mean"};
#'  \item \code{"median"};
#'  \item \code{"mode"};
#'  \item \code{"sample"}.
#' }
#' @param initial.int Initial imputation method for integer type data:
#' \itemize{
#'  \item \code{"mode"} (Default);
#'  \item \code{"sample"}.
#' }
#' @param initial.fac Initial imputation method for factor type data:
#' \itemize{
#'  \item \code{"mode"} (Default);
#'  \item \code{"sample"}.
#' }
#' @param save.models Whether to save models for imputing new data later on. Default: \code{FALSE}
#' @param save.vars Response models for variables specified in \code{save.vars} will be saved for imputing new data. Can be a vector of names or indices. By default, \code{save.vars = NULL}, response models for variables with missing values will be saved. To save all models, please specify \code{save.vars = colnames(data)}.
#' @param verbose Verbose setting for mixgb. If \code{TRUE}, will print out the progress of imputation. Default: \code{FALSE}.
#' @param xgb.params A list of XGBoost parameters. For more details, please check \href{https://xgboost.readthedocs.io/en/stable/parameter.html}{XGBoost documentation on parameters}.
#' @param nrounds The maximum number of boosting iterations for XGBoost. Default: 50
#' @param early_stopping_rounds An integer value \code{k}. XGBoost training will stop if the validation performance hasn't improved for \code{k} rounds. Default: 10.
#' @param print_every_n Print XGBoost evaluation information at every nth iteration if \code{xgboost_verbose > 0}.
#' @param xgboost_verbose Verbose setting for XGBoost training: 0 (silent), 1 (print information) and 2 (print additional information). Default: 0
#' @param ... Extra arguments to pass to XGBoost
#' @return If \code{save.models = FALSE}, will return a list of \code{m} imputed datasets. If \code{save.models = TRUE}, will return an object with imputed datasets, saved models and parameters.
#' @export
#' @examples
#' # obtain m multiply datasets without saving models
#' mixgb.data <- mixgb(data = nhanes3, m = 2)
#'
#' # obtain m multiply imputed datasets and save models for imputing new data later on
#' mixgb.obj <- mixgb(data = nhanes3, m = 2, save.models = TRUE)
mixgb <- function(data, m = 5, maxit = 1, ordinalAsInteger = TRUE, bootstrap = TRUE,
                  pmm.type = "auto", pmm.k = 5, pmm.link = "prob",
                  initial.num = "normal", initial.int = "mode", initial.fac = "mode",
                  save.models = FALSE, save.vars = NULL, verbose = F,
                  xgb.params = list(max_depth = 6, gamma = 0, eta = 0.3, min_child_weight = 1, subsample = 1, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1, tree_method = "auto", gpu_id = 0, predictor = "auto"),
                  nrounds = 50, early_stopping_rounds = 1, print_every_n = 10L, xgboost_verbose = 0, ...) {
  if (!(is.data.frame(data) || is.matrix(data))) {
    stop("Data need to be a data frame or a matrix.")
  }

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }


  if (ordinalAsInteger == TRUE) {
    if (is.null(pmm.type)) {
      ordinalAsInteger <- FALSE
      warning(" `ordinalAsInteger` will be coerced to FALSE when `pmm.type = NULL`")
    }
    ord.fac <- names(Filter(is.ordered, data))
    # ord.fac<- colnames(data)[sapply(data,is.ordered)]
    ## data[,c(ord.fac) := lapply(.SD, as.integer), .SDcols = ord.fac]
    if(length(ord.fac)>0){
      data[, c(ord.fac) := lapply(.SD, fac2int), .SDcols = ord.fac]
    }
  }


  # initial imputation
  # initial.obj: An object including some basic data information and a pre-fill dataset after initial imputation
  initial.obj <- initial_imp(data, initial.num = initial.num, initial.int = initial.int, initial.fac = initial.fac, bootstrap = bootstrap)
  sorted.naSums <- initial.obj$sorted.naSums
  sorted.types <- initial.obj$sorted.types
  Nrow <- initial.obj$Nrow


  # checking
  check_pmm(pmm.type = pmm.type, bootstrap = bootstrap, xgb.params = xgb.params, Nrow = Nrow, sorted.naSums = sorted.naSums, sorted.types = sorted.types, pmm.k = pmm.k)

  imputed.data <- vector("list", m)

  origin.names <- initial.obj$origin.names
  sorted.types <- initial.obj$sorted.types
  sorted.names <- initial.obj$sorted.names
  sorted.naSums <- initial.obj$sorted.naSums
  ##
  sorted.idx <- initial.obj$sorted.idx

  missing.idx <- initial.obj$missing.idx
  missing.vars <- initial.obj$missing.vars
  missing.types <- initial.obj$missing.types
  Obs.idx <- initial.obj$Obs.idx
  Na.idx <- initial.obj$Na.idx
  Ncol <- initial.obj$Ncol
  mp <- initial.obj$mp

  # validate save.vars :
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
      xgb.params = xgb.params,
      nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
    )
  }

  # ............................................................................................................
  if (save.models == FALSE) {
    # Default: don't save any models

    if (bootstrap == FALSE) {
      # bootstrap=FALSE--------------------------------------------------------------
      if(verbose){
        cat("mixgb without bootstrap:", "imputing set")
      }

      for (i in seq_len(m)) {
        if(verbose){
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- initial.obj$sorted.dt
        for (j in seq_len(maxit)) {
          sorted.dt <- mixgb_null(
            pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list,
            sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
            Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
            xgb.params = xgb.params,
            nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
          )
        }
        imputed.data[[i]] <- sorted.dt[, origin.names, with = FALSE]
      }
    } else {
      # bootstrap=TRUE--------------------------------------------------------------
      if(verbose){
        cat("mixgb with bootstrap:", "imputing set")
      }

      for (i in seq_len(m)) {
        if(verbose){
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- initial.obj$sorted.dt
        boot.result <- boot(Nrow = Nrow, sorted.dt = sorted.dt, sortedNA.dt = initial.obj$sortedNA.dt, missing.vars = missing.vars, mp = mp)
        for (j in seq_len(maxit)) {
          sorted.dt <- mixgb_boot(
            BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
            yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars,
            sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
            xgb.params = xgb.params,
            nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
          )
        }
        imputed.data[[i]] <- sorted.dt[, origin.names, with = FALSE]
      }
    } # end of if(bootstrap)

    if(verbose){
      cat("\n")
    }

    return(imputed.data)
    # mixgb.obj <- list("imputed.data" = imputed.data, "XGB.models" =NULL, "params" = NULL)
    # class(mixgb.obj)<-"mixgbObj"
    # return(mixgb.obj)
  } else {
    # save.models=TRUE, save models for imputing new data..........................................................................................

    # save params of this imputer for impute.new().....................................................
    params <- list()
    params$initial.num <- initial.num
    params$initial.fac <- initial.fac
    params$pmm.k <- pmm.k
    params$pmm.type <- pmm.type
    params$pmm.link <- pmm.link
    params$m <- m
    params$maxit <- maxit
    params$sorted.names <- sorted.names
    params$sorted.types <- sorted.types
    params$sorted.naSums <- sorted.naSums
    params$save.vars <- save.vars
    params$missing.vars <- missing.vars
    params$extra.vars <- extra.vars
    params$Na.idx <- Na.idx
    params$sorted.idx <- sorted.idx
    params$Obs.idx <- Obs.idx
    params$bootstrap <- bootstrap
    params$yobs.list <- yobs.list
    params$ordinalAsInteger <- ordinalAsInteger


    # pre-allocation for saved models: for each of m, save Nmodels
    XGB.models <- vector("list", m)

    #--------------------------------------------------------
    if (bootstrap == FALSE) {
      # bootstrap=FALSE--------------------------------------------------------------
      if(verbose){
        cat("mixgb without bootstrap:", "saving models and imputing set")
      }

      for (i in seq_len(m)) {
        if(verbose){
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- initial.obj$sorted.dt
        if (maxit > 1) {
          for (j in seq_len(maxit - 1)) {
            # for j=1:(maxit-1)  only update the imputed dataset
            sorted.dt <- mixgb_null(
              pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars,
              sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
              xgb.params = xgb.params,
              nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
            )
          }
        }
        # the last iteration, update the imputed dataset, also save models
        saved.obj <- mixgb_save(
          save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
          yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
          Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
          xgb.params = xgb.params,
          nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
        )
        # if pmm.type=NULL
        imputed.data[[i]] <- saved.obj$sorted.dt[, origin.names, with = FALSE]
        XGB.models[[i]] <- saved.obj$xgb.models
        # if pmm.type=0,2,auto
        if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2) | isTRUE(pmm.type == "auto")) {
          yhatobs.list[[i]] <- saved.obj$yhatobs.list
        }
        # if pmm.type=1  (only use one set of yhatobs.list across all m, it's saved ahead)
      }
    } else {
      # bootstrap=TRUE--------------------------------------------------------------
      if(verbose){
        cat("mixgb with bootstrap:", "saving models and imputing set")
      }

      for (i in seq_len(m)) {
        if(verbose){
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- initial.obj$sorted.dt
        boot.result <- boot(Nrow = Nrow, sorted.dt = sorted.dt, sortedNA.dt = initial.obj$sortedNA.dt, missing.vars = missing.vars, mp = mp)
        if (maxit > 1) {
          for (j in seq_len(maxit - 1)) {
            sorted.dt <- mixgb_boot(
              BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
              yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars,
              sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
              xgb.params = xgb.params,
              nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
            )
          }
        }
        saved.obj <- mixgb_bootsave(
          BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
          yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
          Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
          xgb.params = xgb.params,
          nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
        )

        # if pmm.type=NULL
        imputed.data[[i]] <- saved.obj$sorted.dt[, origin.names, with = FALSE]
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

    if(verbose){
      cat("\n")
    }
    mixgb.obj <- list("imputed.data" = imputed.data, "XGB.models" = XGB.models, "params" = params)
    class(mixgb.obj) <- "mixgbObj"
    return(mixgb.obj)
  }
} # end of impute function
