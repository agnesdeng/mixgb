# Checking for pmm.type and bootstrap constraints

check_pmm <- function(pmm.type, bootstrap, xgb.params, Nrow, sorted.naSums, sorted.types, pmm.k) {
  # without bootstrapping: at least one of the sample related hyperparameters in xgboost must be less than 1
  if (!is.null(pmm.type)) {
    ################
    if ((pmm.type == 1 | pmm.type == 2) & bootstrap == FALSE) {
      sample.params <- grepl("sample", names(xgb.params))
      if (all(sample.params == FALSE)) {
        # no sample params specified by users, default are all 1
        warning("For multiple imputation through XGBoost with PMM type 1 or type 2(without bootstrap), recommend to set at least of one of the following hyperparameters less than 1:\n subsample\n colsample_bytree\n colsample_bylevel\n colsample_bynode")
      } else {
        if (all(xgb.params[sample.params] == 1)) {
          warning("For multiple imputation through XGBoost with PMM type 1 or type 2 (without bootstrap), recommend to set at least one of the following hyperparameters less than 1:\n subsample\n colsample_bytree\n colsample_bylevel\n colsample_bynode")
        }
      }
    }

    #################
    if (pmm.type == 0 & bootstrap == TRUE) {
      stop("PMM type0 requires using the whole dataset. Bootstrap or sample hyperparameters<1 are not allowed.")
    }

    if (pmm.type == 0 & bootstrap == FALSE) {
      sample.params <- grepl("sample", names(xgb.params))
      if (any(sample.params == TRUE)) {
        if (any(xgb.params[sample.params] != 1)) {
          stop("PMM type0 requires using the whole dataset. Bootstrap or sample hyperparameters<1 are not allowed.")
        }
      }
    }
    ###########
    if (!pmm.type %in% c(0, 1, 2, "auto")) {
      stop("The specified pmm.type is incorrect. It must be one of the following types: NULL,1,2,\"auto\".")
    }
    ########
    if (pmm.type == 0) {
      if (bootstrap == TRUE) {
        stop("PMM type0 need to use the whole dataset. It does not allow bootstrap.")
      } else {
        # bootstrap=FALSE, but need to check other sample related params to be 1
        sample.params <- grepl("sample", names(xgb.params))
        if (any(sample.params == TRUE)) {
          if (any(xgb.params[sample.params] != 1)) {
            stop("For multiple imputation through XGBoost with PMM type 0 (without bootstrap), all the following hyperparameters must be 1:\n subsample\n colsample_bytree\n colsample_bylevel\n colsample_bynode")
          }
        }
      }
    }

    # .......................
    if (any(Nrow - sorted.naSums < pmm.k) && pmm.type != "auto") {
      maxNA <- max(sorted.naSums)
      minObs <- Nrow - maxNA
      s1 <- paste("In this dataset, the minimum number of observed values in a variable is ", minObs, ".", sep = "")
      s2 <- paste("However, pmm.k=", pmm.k, ".", sep = "")
      if (minObs == 1) {
        s3 <- paste("Please set pmm.k = 1 .")
      } else {
        s3 <- paste("Please set the value of pmm.k less than or equal to ", minObs, ".", sep = "")
      }
      stop(paste(s1, s2, s3, sep = "\n"))
    }
    ### if pmm.type="auto", only numeric variables need to perform PMM
    if (pmm.type == "auto") {
      idx <- which(Nrow - sorted.naSums < pmm.k & sorted.types == "numeric")
      if (length(idx) > 0) {
        maxNA <- max(sorted.naSums[idx])
        minObs <- Nrow - maxNA
        s1 <- paste("In this dataset, the minimum number of observed values in a numeric variable is ", minObs, ".", sep = "")
        s2 <- paste("When pmm.type = \"auto\", type 2 PMM would apply to numeric variables. However, pmm.k=", pmm.k, ".", sep = "")
        if (minObs == 1) {
          s3 <- paste("Please set pmm.k = 1 .")
        } else {
          s3 <- paste("Please set the value of pmm.k less than or equal to ", minObs, ".", sep = "")
        }
        stop(paste(s1, s2, s3, sep = "\n"))
      }
    }
  }
}
