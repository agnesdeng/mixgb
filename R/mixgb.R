#'  Multiple imputation through XGBoost
#' @description This function is used to generate multiply-imputed datasets using XGBoost, subsampling and predictive mean matching (PMM).
#' @param data A data.frame or data.table with missing values
#' @param m The number of imputed datasets. Default: 5
#' @param maxit The number of imputation iterations. Default: 1
#' @param ordinalAsInteger Whether to convert ordinal factors to integers. By default, \code{ordinalAsInteger = FALSE}. Setting \code{ordinalAsInteger = TRUE} may speed up the imputation process for large datasets.
#' @param pmm.type The type of predictive mean matching (PMM). Possible values:
#' \itemize{
#'  \item \code{NULL} (default): Imputations without PMM;
#'  \item \code{0}: Imputations with PMM type 0;
#'  \item \code{1}: Imputations with PMM type 1;
#'  \item \code{2}: Imputations with PMM type 2;
#'  \item \code{"auto"}: Imputations with PMM type 2 for numeric/integer variables; imputations without PMM for categorical variables.
#' }
#' @param pmm.k The number of donors for predictive mean matching. Default: 5
#' @param pmm.link The link for predictive mean matching in binary variables
#' \itemize{
#'  \item \code{"prob"} (default): use probabilities;
#'  \item \code{"logit"}: use logit values.
#' }
#' @param initial.num Initial imputation method for numeric type data:
#' \itemize{
#'  \item \code{"normal"} (default);
#'  \item \code{"mean"};
#'  \item \code{"median"};
#'  \item \code{"mode"};
#'  \item \code{"sample"}.
#' }
#' @param initial.int Initial imputation method for integer type data:
#' \itemize{
#'  \item \code{"mode"} (default);
#'  \item \code{"sample"}.
#' }
#' @param initial.fac Initial imputation method for factor type data:
#' \itemize{
#'  \item \code{"mode"} (default);
#'  \item \code{"sample"}.
#' }
#' @param save.models Whether to save imputation models for imputing new data later on. Default: \code{FALSE}
#' @param save.vars For the purpose of imputing new data, the imputation models for response variables specified in \code{save.vars} will be saved. The values in \code{save.vars} can be a vector of names or indices. By default, only the imputation models for variables with missing values in the original data will be saved (\code{save.vars = NULL}). To save imputation models for all variables, users can specify \code{save.vars = colnames(data)}.
#' @param save.models.folder Users can specify a directory to save all imputation models. Models will be saved in JSON format by internally calling \code{xgb.save()}, which is recommended by XGBoost.
#' @param verbose Verbose setting for mixgb. If \code{TRUE}, will print out the progress of imputation. Default: \code{FALSE}.
#' @param xgb.params A list of XGBoost parameters. For more details, please check \href{https://xgboost.readthedocs.io/en/stable/parameter.html}{XGBoost documentation on parameters}.
#' @param nrounds The maximum number of boosting iterations for XGBoost. Default: 100
#' @param early_stopping_rounds An integer value \code{k}. XGBoost training will stop if the validation performance has not improved for \code{k} rounds. Default: 10.
#' @param print_every_n Print XGBoost evaluation information at every nth iteration if \code{xgboost_verbose > 0}.
#' @param xgboost_verbose Verbose setting for XGBoost training: 0 (silent), 1 (print information) and 2 (print additional information). Default: 0
#' @param ... Extra arguments to be passed to XGBoost
#' @return If \code{save.models = FALSE}, this function will return a list of \code{m} imputed datasets. If \code{save.models = TRUE}, it will return an object with imputed datasets, saved models and parameters.
#' @export
#' @examples
#' # obtain m multiply datasets without saving models
#' params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
#' mixgb.data <- mixgb(data = nhanes3, m = 2, xgb.params = params, nrounds = 10)
#'
#' # obtain m multiply imputed datasets and save models for imputing new data later on
#' mixgb.obj <- mixgb(data = nhanes3, m = 2, xgb.params = params, nrounds = 10,
#'                    save.models = TRUE, save.models.folder = tempdir())
mixgb<- function(data, m = 5, maxit = 1, ordinalAsInteger = FALSE,
                     pmm.type = NULL, pmm.k = 5, pmm.link = "prob",
                     initial.num = "normal", initial.int = "mode", initial.fac = "mode",
                     save.models = FALSE, save.vars = NULL, save.models.folder = NULL,
                     verbose = F,
                     xgb.params = list(),
                     nrounds = 100, early_stopping_rounds = NULL, print_every_n = 10L, xgboost_verbose = 0, ...) {

  if (!(is.data.frame(data) || is.matrix(data))) {
    stop("Data need to be a data frame or a matrix.")
  }

  if (!is.data.table(data)) {
    data <- as.data.table(data)

  }

  # check whether to use xgb.save()
  if (!is.null(save.models.folder)) {
    if (!dir.exists(save.models.folder)) {
      dir.create(file.path(save.models.folder), recursive = TRUE)
    }
    save.models <- TRUE
    XGB.save <- TRUE
  } else if(save.models) {
    warnings("Models are saved in the environment. Suggest specifing the directory in save.models.folder")
    XGB.save<-FALSE
  }

  # Get the current version of XGBoost as a 'package_version' object
  xgboost.version <- packageVersion("xgboost")

  # Define the minimum required version as a 'package_version' object
  required.version <- package_version("2.0.0")

  # Use relational operators to compare version objects
  if (xgboost.version >= required.version) {
    # for XGBoost 2.0.0 or newer
    xgb.params <- do.call("default_params", xgb.params)
  } else {
    # Code for older versions of XGBoost
    xgb.params <- do.call("default_params_cran", xgb.params)
  }





  if (ordinalAsInteger == TRUE) {
    if (is.null(pmm.type)) {
      ordinalAsInteger <- FALSE
      warning(" `ordinalAsInteger` will be coerced to FALSE when `pmm.type = NULL`")
    }
    ord.fac <- names(Filter(is.ordered, data))

    if (length(ord.fac) > 0) {
      data[, c(ord.fac) := lapply(.SD, fac2int), .SDcols = ord.fac]
    }
  }


  # initial imputation

  Ncol <- ncol(data)
  if (Ncol < 2) {
    stop("Data need to have a least two columns.")
  }
  Nrow <- nrow(data)


  ## Data preprocessing
  # 1) sort the dataset with increasing NAs
  origin.names <- copy(colnames(data))

  # Calculate the number of NAs per column directly within the data.table framework
  na_counts <- data[, lapply(.SD, function(col) sum(is.na(col)))]

  # Order columns by their NA counts
  na_vector <- unlist(na_counts, use.names = TRUE)
  sorted.idx <- order(na_vector)
  # Get sorted column names
  sorted.names <- names(na_counts)[sorted.idx]

  all.idx<-1:Ncol
  names(all.idx)<-sorted.names


  # data.table
  # sorted.data <- data[, ..sorted.names]
  # sorted.data <- data[, sorted.names, with = FALSE]
  setcolorder(data, sorted.names)

  sorted.types <- feature_type2(data)
  cbind.types<-cbind_type(data)

  if(all(cbind.types=="factor")){
    matrix.method<-"cpp.factor"
  }else if(all(cbind.types %in% c("numeric","integer"))){
    matrix.method<-"as.matrix"
  }else{
    matrix.method<-"cpp.combo"
  }


  # 2)initial imputation & data validation


  sorted.naSums <- na_counts[, sorted.names, with = FALSE]
  missing.idx <- which(sorted.naSums != 0)
  missing.vars <- sorted.names[missing.idx]

  names(missing.idx)<-missing.vars

  obs.vars<-sorted.names[-missing.idx]

  missing.types <- sorted.types[missing.idx]
  missing.method <- ifelse(missing.types == "numeric", initial.num,
                           ifelse(missing.types == "integer", initial.int, initial.fac)
  )


  if (all(sorted.naSums == 0)) {
    stop("No missing values in this data frame.")
  }

  if (any(sorted.naSums == Nrow)) {
    stop("At least one variable in the data frame has 100% missing values.")
  }



  if (any(sorted.naSums >= 0.9 * Nrow)) {
    warning("Some variables have more than 90% miss entries.")
  }

  mp <- length(missing.vars)
  Obs.idx <- vector("list", mp)
  names(Obs.idx) <- missing.vars
  Na.idx <- vector("list", mp)
  names(Na.idx) <- missing.vars



  for (var in missing.vars) {
    na.idx <- which(is.na(data[[var]]))
    Na.idx[[var]] <- na.idx

    # Na.idx[[var]] <- data[, .I[is.na(get(var))], .SDcols = var]

    if (missing.method[[var]] == "normal") {
      # only works for numeric
      var.mean <- mean(data[[var]], na.rm = TRUE)
      var.sd <- sd(data[[var]], na.rm = TRUE)
      set(data, i = na.idx, j = var, value = stats::rnorm(n = length(na.idx), mean = var.mean, sd = var.sd))
    } else if (missing.method[[var]] == "mean") {
      # only works for numeric
      set(data, i = na.idx, j = var, value = mean(data[[var]], na.rm = TRUE))
    } else if (missing.method[[var]] == "median") {
      # only works for numeric
      set(data, i = na.idx, j = var, value = median(data[[var]], na.rm = TRUE))
    } else if (missing.method[[var]] == "mode") {
      # work for both numeric (only recommend for integer type) and factor
      unique.values <- unique(na.omit(data[[var]]))
      tab <- tabulate(match(data[[var]], unique.values))
      var.mode <- unique.values[tab == max(tab)]
      if (length(var.mode) != 1) {
        # if mode is not unique, impute with randomly sampled modes
        var.mode <- sample(var.mode, size = length(na.idx), replace = TRUE)
      }

      set(data, i = na.idx, j = var, value = var.mode)
    } else if (missing.method[[var]] == "sample") {
      # work for both numeric (only recommend for integer type) and factor
      set(data, i = na.idx, j = var, value = sample(data[[var]][!is.na(data[[var]])], size = length(na.idx), replace = TRUE))
    } else {
      stop("Please specify an acceptable initial imputation method.")
    }

    # To do: include initial imputation using models
    # To do: if bootstrap, need a copy of the sorted data before initial imputation, will need more memory usage. Do it later
    # if (bootstrap) {
    # originally :  result$sortedNA.dt <- sort.result$sorted.dt
    # }
  }



  check_pmm(pmm.type = pmm.type, xgb.params = xgb.params, Nrow = Nrow, sorted.naSums = sorted.naSums, sorted.types = sorted.types, pmm.k = pmm.k)

  imputed.data <- vector("list", m)


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
      yobs.list[[var]] <- data[[var]][-na.idx]
    }
    # variables fully observed
    for (var in extra.vars) {
      yobs.list[[var]] <- data[[var]]
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
      yobs.list[[var]] <- data[[var]][-na.idx]
    }
  }


  if(length(obs.vars)==0){
    Obs.m<-NULL
  }else{
    if(matrix.method=="as.matrix"){

      Obs.m<-as.matrix(data[,!missing.vars,with = FALSE])


    }else{

      Obs.list <- lapply(obs.vars, function(feature){

        if(cbind.types[feature] %in% c("numeric","integer")){
          as.matrix(data[[feature]])
        } else if(cbind.types[feature] == "ordered"){
          Matrix::t(fac2Sparse(data[[feature]], factorPatt12=c(T,F), contrasts.arg = "contr.poly")[[1]])
        } else {
          Matrix::t(fac2sparse(data[[feature]]))[, -1, drop = FALSE]
        }
      })


      if(matrix.method=="cpp.combo"){
        Obs.m<-cbind_combo(Obs.list )
      }else if(matrix.method=="cpp.factor"){
        Obs.m<-cbind_sparse_matrix(Obs.list )
      }


    }
  }



  yhatobs.list <- NULL
  if (isTRUE(pmm.type == 1)) {
    sorted.dt <- copy(data)
    yhatobs.list <- save_yhatobs(Obs.m=Obs.m, matrix.method=matrix.method, cbind.types=cbind.types, all.idx=all.idx,
      yobs.list = yobs.list, maxit = maxit, pmm.link = pmm.link, sorted.dt = sorted.dt, missing.vars = missing.vars, extra.vars = extra.vars, extra.types = extra.types, sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
      xgb.params = xgb.params,
      nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
    )
  }



  # ............................................................................................................
  if (save.models == FALSE) {
    # Default: don't save any models


      if (verbose) {
        cat("mixgb with subsampling:", "imputing set")
      }

      for (i in seq_len(m)) {
        if (verbose) {
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- copy(data)
        # sorted.dt <- data

        for (j in seq_len(maxit)) {

          sorted.dt <- mixgb_null(Obs.m=Obs.m, matrix.method=matrix.method, cbind.types=cbind.types,
            pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list,
            sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
            Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
            xgb.params = xgb.params,
            nrounds = nrounds, early_stopping_rounds = early_stopping_rounds,
            print_every_n = print_every_n, verbose = xgboost_verbose
            ,...
          )

        }

        imputed.data[[i]] <- sorted.dt[, origin.names, with = FALSE]
      }


    if (verbose) {
      cat("\n")
    }

    return(imputed.data)

  } else {
    # save.models=TRUE, save models for imputing new data..........................................................................................
    # save params of this imputer for impute.new().....................................................
    params <- list()
    params$initial.num <- initial.num
    params$initial.int<-initial.int
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
    params$obs.vars<-obs.vars
    params$extra.vars <- extra.vars
    params$Na.idx <- Na.idx
    params$sorted.idx <- sorted.idx
    params$Obs.idx <- Obs.idx
    params$yobs.list <- yobs.list
    params$ordinalAsInteger <- ordinalAsInteger
    params$nthread<-xgb.params$nthread
    params$matrix.method<-matrix.method
    params$cbind.types<-cbind.types


    # pre-allocation for saved models: for each of m, save Nmodels
    if (isTRUE(save.models)) {
      # save the model dir
      XGB.models <- vector("list", m)
    }


      if (verbose) {
        cat("mixgb with subsampling:", "saving models and imputing set")
      }

      for (i in seq_len(m)) {
        if (verbose) {
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- copy(data)
        if (maxit > 1) {
          for (j in seq_len(maxit - 1)) {
            # for j=1:(maxit-1)  only update the imputed dataset
            sorted.dt <- mixgb_null(Obs.m=Obs.m, matrix.method=matrix.method, cbind.types=cbind.types,
                                    pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list,
                                    sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
                                    Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
                                    xgb.params = xgb.params,
                                    nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose,...
            )

          }
        }
        # the last iteration, update the imputed dataset, also save models
        if (isTRUE(XGB.save)) {
          saved.obj <- mixgb_xgb_save(Obs.m=Obs.m, matrix.method=matrix.method, cbind.types=cbind.types, all.idx,
            save.models.folder = save.models.folder, i = i,
            save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
            yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
            Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
            xgb.params = xgb.params,
            nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
          )
        }else{
          saved.obj <- mixgb_save(Obs.m=Obs.m, matrix.method=matrix.method, cbind.types=cbind.types, all.idx,
                                      save.models.folder = save.models.folder, i = i,
                                      save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
                                      yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
                                      Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
                                      xgb.params = xgb.params,
                                      nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
          )
        }

        # if pmm.type=NULL
        imputed.data[[i]] <- saved.obj$sorted.dt[, origin.names, with = FALSE]
        XGB.models[[i]] <- saved.obj$xgb.models
        # if pmm.type=0,2,auto
        if (isTRUE(pmm.type == 0) | isTRUE(pmm.type == 2) | isTRUE(pmm.type == "auto")) {
          yhatobs.list[[i]] <- saved.obj$yhatobs.list
        }
        # if pmm.type=1  (only use one set of yhatobs.list across all m, it's saved ahead)
      }




    #---------------------------------------------------------

    # pmm.type=NULL, yhatobs.list=NULL
    # pmm.type=1, yhatobs.list only one list with different vars
    # pmm.type=0,2,"auto" yhatlobs.list has m list with different vars

    params$yhatobs.list <- yhatobs.list
    params$yobs.list <- yobs.list

    if (verbose) {
      cat("\n")
    }
    mixgb.obj <- list("imputed.data" = imputed.data, "XGB.models" = XGB.models, "params" = params, "XGB.save" = XGB.save)
    class(mixgb.obj) <- "mixgbObj"
    return(mixgb.obj)
  }
} # end of impute function




#' Auxiliary function for validating xgb.params
#' @description Auxiliary function for setting up the default XGBoost-related hyperparameters for mixgb and checking the \code{xgb.params} argument in \code{mixgb()}. For more details on XGBoost hyperparameters, please refer to \href{https://xgboost.readthedocs.io/en/stable/parameter.html}{XGBoost documentation on parameters}.
#' @param device Can be either \code{"cpu"} or \code{"cuda"}. For ther options please refer to \href{https://xgboost.readthedocs.io/en/stable/parameter.html#general-parameters}{XGBoost documentation on parameters}.
#' @param tree_method Options: \code{"auto"}, \code{"exact"}, \code{"approx"}, and \code{"hist"}. Default: \code{"hist"}.
#' @param eta Step size shrinkage. Default: 0.3.
#' @param gamma Minimum loss reduction required to make a further partition on a leaf node of the tree. Default: 0
#' @param max_depth Maximum depth of a tree. Default: 3.
#' @param min_child_weight Minimum sum of instance weight needed in a child. Default: 1.
#' @param max_delta_step Maximum delta step. Default: 0.
#' @param subsample Subsampling ratio of the data. Default: 0.7.
#' @param sampling_method The method used to sample the data. Default: \code{"uniform"}.
#' @param colsample_bytree Subsampling ratio of columns when constructing each tree. Default: 1.
#' @param colsample_bylevel Subsampling ratio of columns for each level. Default: 1.
#' @param colsample_bynode Subsampling ratio of columns for each node. Default: 1.
#' @param lambda L2 regularization term on weights. Default: 1.
#' @param alpha L1 regularization term on weights. Default: 0.
#' @param max_leaves Maximum number of nodes to be added (Not used when \code{tree_method = "exact"}). Default: 0.
#' @param max_bin Maximum number of discrete bins to bucket continuous features (Only used when \code{tree_method} is either \code{"hist"}, \code{"approx"} or \code{"gpu_hist"}). Default: 256.
#' @param num_parallel_tree The number of parallel trees used for boosted random forests. Default: 1.
#' @param nthread The number of CPU threads to be used. Default: -1 (all available threads).
#' @return A list of hyperparameters.
#' @export
#' @examples
#' default_params()
#'
#' xgb.params <- list(device = "cuda", subsample = 0.9, nthread = 2)
#' default_params(device = xgb.params$device,
#'                subsample = xgb.params$subsample,
#'                nthread = xgb.params$nthread)
#'
#' xgb.params <- do.call("default_params", xgb.params)
#' xgb.params
default_params <- function(device = "cpu", tree_method = "hist", eta = 0.3, gamma = 0, max_depth = 3, min_child_weight = 1, max_delta_step = 0,
                           subsample = 0.7, sampling_method = "uniform", colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1, lambda = 1, alpha = 0,
                           max_leaves = 0, max_bin = 256, num_parallel_tree = 1, nthread = -1) {
  list(
    device = device, tree_method = tree_method, eta = eta, gamma = gamma, max_depth = max_depth, min_child_weight = min_child_weight,
    subsample = subsample, sampling_method = sampling_method, colsample_bytree = colsample_bytree, colsample_bylevel = colsample_bylevel,
    colsample_bynode = colsample_bynode, lambda = lambda, alpha = alpha,
    max_leaves = max_leaves, max_bin = max_bin, num_parallel_tree = num_parallel_tree, nthread = nthread
  )
}


is_dir <- function(dir) {
  return(file.info(dir)$isdir)
}



#' Auxiliary function for validating xgb.params compatible with XGBoost CRAN version
#' @description Auxiliary function for setting up the default XGBoost-related hyperparameters for mixgb and checking the \code{xgb.params} argument in \code{mixgb()}. For more details on XGBoost hyperparameters, please refer to \href{https://xgboost.readthedocs.io/en/stable/parameter.html}{XGBoost documentation on parameters}.
#' @param tree_method Options: \code{"auto"}, \code{"exact"}, \code{"approx"}, and \code{"hist"}. Default: \code{"hist"}.
#' @param eta Step size shrinkage. Default: 0.3.
#' @param gamma Minimum loss reduction required to make a further partition on a leaf node of the tree. Default: 0
#' @param max_depth Maximum depth of a tree. Default: 3.
#' @param min_child_weight Minimum sum of instance weight needed in a child. Default: 1.
#' @param max_delta_step Maximum delta step. Default: 0.
#' @param subsample Subsampling ratio of the data. Default: 0.7.
#' @param sampling_method The method used to sample the data. Default: \code{"uniform"}.
#' @param colsample_bytree Subsampling ratio of columns when constructing each tree. Default: 1.
#' @param colsample_bylevel Subsampling ratio of columns for each level. Default: 1.
#' @param colsample_bynode Subsampling ratio of columns for each node. Default: 1.
#' @param lambda L2 regularization term on weights. Default: 1.
#' @param alpha L1 regularization term on weights. Default: 0.
#' @param max_leaves Maximum number of nodes to be added (Not used when \code{tree_method = "exact"}). Default: 0.
#' @param max_bin Maximum number of discrete bins to bucket continuous features (Only used when \code{tree_method} is either \code{"hist"}, \code{"approx"} or \code{"gpu_hist"}). Default: 256.
#' @param predictor Default: \code{"auto"}
#' @param num_parallel_tree The number of parallel trees used for boosted random forests. Default: 1.
#' @param gpu_id Which GPU device should be used. Default: 0.
#' @param nthread The number of CPU threads to be used. Default: -1 (all available threads).
#' @return A list of hyperparameters.
#' @export
#' @examples
#' default_params_cran()
#'
#' xgb.params <- list(subsample = 0.9, gpu_id = 1)
#' default_params_cran(subsample = xgb.params$subsample, gpu_id = xgb.params$gpu_id)
#'
#' xgb.params <- do.call("default_params_cran", xgb.params)
#' xgb.params
default_params_cran <- function(eta = 0.3, gamma = 0, max_depth = 3, min_child_weight = 1, max_delta_step, subsample = 0.7, sampling_method = "uniform",
                                colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode = 1, lambda = 1, alpha = 0, tree_method = "auto", max_leaves = 0, max_bin = 256,
                                predictor = "auto", num_parallel_tree = 1, gpu_id = 0, nthread = -1) {
  list(eta = eta, gamma = gamma, max_depth = max_depth, min_child_weight = min_child_weight, subsample = subsample, sampling_method = sampling_method, colsample_bytree = colsample_bytree, colsample_bylevel = colsample_bylevel, colsample_bynode = colsample_bynode, lambda = lambda, alpha = alpha, tree_method = tree_method, max_leaves = max_leaves, max_bin = max_bin, predictor = predictor, num_parallel_tree = num_parallel_tree, gpu_id = gpu_id, nthread = nthread)
}


