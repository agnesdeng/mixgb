#' Multiple imputation through XGBoost Version 3 for big numeric data (In development)
#' @description This function is used to generate multiply imputed datasets using XGBoost, subsampling and predictive mean matching (PMM).
#' @param data A data.frame or data.table with missing values
#' @param m The number of imputed datasets. Default: 5
#' @param maxit The number of imputation iterations. Default: 1
#' @param sparse Whether to use \code{sparse.model.matrix}. Default: FALSE. Setting this to TRUE for numeric data will be slow when the data is not sparse.
#' @param ordinalAsInteger Whether to convert ordinal factors to integers. By default, \code{ordinalAsInteger = FALSE}. Setting \code{ordinalAsInteger = TRUE} may speed up the imputation process for large datasets.
#' @param bootstrap Whether to use bootstrapping for multiple imputation. By default, \code{bootstrap = FALSE}. Setting \code{bootstrap = TRUE} can improve imputation variability if sampling-related hyperparameters of XGBoost are set to 1.
#' @param pmm.type The type of predictive mean matching (PMM). Possible values:
#' \itemize{
#'  \item \code{NULL}: Imputations without PMM;
#'  \item \code{0}: Imputations with PMM type 0;
#'  \item \code{1}: Imputations with PMM type 1;
#'  \item \code{2}: Imputations with PMM type 2;
#'  \item \code{"auto"} (Default): Imputations with PMM type 2 for numeric/integer variables; imputations without PMM for categorical variables.
#' }
#' @param pmm.k The number of donors for predictive mean matching. Default: 5
#' @param pmm.link The link for predictive mean matching in binary variables
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
#' @param save.models Whether to save imputation models for imputing new data later on. Default: \code{FALSE}
#' @param save.vars For the purpose of imputing new data, the imputation models for response variables specified in \code{save.vars} will be saved. The values in \code{save.vars} can be a vector of names or indices. By default, only the imputation models for variables with missing values in the original data will be saved (\code{save.vars = NULL}). To save imputation models for all variables, users can specify it with \code{save.vars = colnames(data)}.
#' @param save.models.folder Users can specify a folder directory to save all imputation models. Models will be saved as JSON format by internally calling \code{xgb.save()}, which is recommended by XGBoost.
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
#' mixgb.obj <- mixgb(data = nhanes3, m = 2, xgb.params = params, nrounds = 10, save.models = TRUE)
mixgb_v3 <- function(data, m = 5, maxit = 1, sparse = FALSE, ordinalAsInteger = FALSE, bootstrap = FALSE,
                     pmm.type = "auto", pmm.k = 5, pmm.link = "prob",
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
  } else {
    XGB.save <- FALSE
  }


  xgb.params <- do.call("default_params", xgb.params)



  if (ordinalAsInteger) {
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



  # data.table
  # sorted.data <- data[, ..sorted.names]
  # sorted.data <- data[, sorted.names, with = FALSE]
  setcolorder(data, sorted.names)

  # setcolorder(data,sorted.names)
  # will change data

  # sorted.dt=data!
  # sorted.idx
  # sorted.names
  sorted.types <- feature_type2(data)


  # 2)initial imputation & data validation

  # sorted.naSums <- data[, lapply(.SD, function(x) sum(is.na(x)))]

  sorted.naSums <- na_counts[, sorted.names, with = FALSE]
  missing.idx <- which(sorted.naSums != 0)
  missing.vars <- sorted.names[missing.idx]
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

  # stop("test memory and time")

  # initial.obj <- initial_imp2(data, initial.num = initial.num, initial.int = initial.int, initial.fac = initial.fac, bootstrap = bootstrap)
  # sorted.naSums <- initial.obj$sorted.naSums
  # sorted.types <- initial.obj$sorted.types
  # Nrow <- initial.obj$Nrow

  # rm(data)
  # gc(full = TRUE)
  # checking


  check_pmm(pmm.type = pmm.type, bootstrap = bootstrap, xgb.params = xgb.params, Nrow = Nrow, sorted.naSums = sorted.naSums, sorted.types = sorted.types, pmm.k = pmm.k)

  imputed.data <- vector("list", m)
  miss.data <- vector("list", m)

  # origin.names <- initial.obj$origin.names
  # sorted.types <- initial.obj$sorted.types
  # sorted.names <- initial.obj$sorted.names
  # sorted.naSums <- initial.obj$sorted.naSums
  # sorted.idx <- initial.obj$sorted.idx

  # missing.idx <- initial.obj$missing.idx
  # missing.vars <- initial.obj$missing.vars
  # missing.types <- initial.obj$missing.types
  # Obs.idx <- initial.obj$Obs.idx
  #  Na.idx <- initial.obj$Na.idx
  #  Ncol <- initial.obj$Ncol
  # mp <- initial.obj$mp

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


  yhatobs.list <- NULL
  if (isTRUE(pmm.type == 1)) {
    yhatobs.list <- save_yhatobs(
      yobs.list = yobs.list, maxit = maxit, pmm.link = pmm.link, sorted.dt = data, missing.vars = missing.vars, extra.vars = extra.vars, extra.types = extra.types, sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
      xgb.params = xgb.params,
      nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
    )
  }

  # ............................................................................................................
  if (save.models == FALSE) {
    # Default: don't save any models

    if (bootstrap == FALSE) {
      # bootstrap=FALSE--------------------------------------------------------------
      if (verbose) {
        cat("mixgb without bootstrap:", "imputing set")
      }

      for (i in seq_len(m)) {
        if (verbose) {
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- copy(data)
        # sorted.dt <- data
        if (maxit > 1) {
          for (j in seq_len(maxit - 1)) {
            sorted.dt <- mixgb_null2(
              sparse = sparse,
              pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list,
              sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
              Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
              xgb.params = xgb.params,
              nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
            )
          }
        }
        # if maxit =1 or at the last iteration: only save the imputations of missing values

        miss.data[[i]] <- mixgb_miss(
          sparse = sparse, mp = mp,
          pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list,
          sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
          Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
          xgb.params = xgb.params,
          nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
        )




        # imputed.data[[i]] <- sorted.dt[, origin.names, with = FALSE]
      }
    } else {
      # bootstrap=TRUE--------------------------------------------------------------
      if (verbose) {
        cat("mixgb with bootstrap:", "imputing set")
      }

      for (i in seq_len(m)) {
        if (verbose) {
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- data
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

    if (verbose) {
      cat("\n")
    }

    # return(imputed.data)
    # return(miss.data)

    # plug in
    setcolorder(data, origin.names)

    for (i in seq_len(m)) {
      for (var in missing.vars) {
        na.idx <- Na.idx[[var]]
        data[na.idx, (var) := miss.data[[i]][[var]]]
      }
      imputed.data[[i]] <- copy(data)
    }

    imputed.data


    # imputed.data[[i]] <- sorted.dt[, origin.names, with = FALSE]
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
    if (isTRUE(XGB.save)) {
      # save the model dir
      XGB.models <- vector("list", m)
    } else {
      # save the model
      XGB.models <- vector("list", m)
    }


    #--------------------------------------------------------
    if (bootstrap == FALSE) {
      # bootstrap=FALSE--------------------------------------------------------------
      if (verbose) {
        cat("mixgb without bootstrap:", "saving models and imputing set")
      }

      for (i in seq_len(m)) {
        if (verbose) {
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- data
        if (maxit > 1) {
          for (j in seq_len(maxit - 1)) {
            # for j=1:(maxit-1)  only update the imputed dataset
            sorted.dt <- mixgb_null2(
              sparse = sparse,
              pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k, yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars,
              sorted.names = sorted.names, Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
              xgb.params = xgb.params,
              nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
            )
          }
        }
        # the last iteration, update the imputed dataset, also save models
        if (isTRUE(XGB.save)) {
          saved.obj <- mixgb_xgb_save(
            save.models.folder = save.models.folder, i = i,
            save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
            yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
            Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
            xgb.params = xgb.params,
            nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
          )
        } else {
          saved.obj <- mixgb_save(
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
    } else {
      # bootstrap=TRUE--------------------------------------------------------------
      if (verbose) {
        cat("mixgb with bootstrap:", "saving models and imputing set")
      }

      for (i in seq_len(m)) {
        if (verbose) {
          cat(" --", i)
        }
        # feed in the initial imputed dataset
        sorted.dt <- data
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
        if (isTRUE(XGB.save)) {
          saved.obj <- mixgb_boot_xgb_save(
            save.models.folder = save.models.folder, i = i,
            BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
            yobs.list = yobs.list, yhatobs.list = yhatobs.list, sorted.dt = sorted.dt, missing.vars = missing.vars, sorted.names = sorted.names,
            Na.idx = Na.idx, missing.types = missing.types, Ncol = Ncol,
            xgb.params = xgb.params,
            nrounds = nrounds, early_stopping_rounds = early_stopping_rounds, print_every_n = print_every_n, verbose = xgboost_verbose, ...
          )
        } else {
          saved.obj <- mixgb_bootsave(
            BNa.idx = boot.result$BNa.idx, boot.dt = boot.result$boot.dt, save.vars = save.vars, save.p = save.p, extra.vars = extra.vars, extra.types = extra.types, pmm.type = pmm.type, pmm.link = pmm.link, pmm.k = pmm.k,
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
    } # end of if(bootstrap)



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

