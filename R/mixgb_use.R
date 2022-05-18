# Multiple imputation using xgboost (through saved models)
mixgb_use <- function(m.set, xgb.models, save.vars, save.p, extra.vars = NULL, extra.types = NULL, pmm.type, pmm.link, pmm.k, yobs.list, yhatobs.list = NULL,
                      sorted.dt, missing.vars, sorted.names, Na.idx, missing.types, Ncol) {

  # param m.set the ith imputation
  # param yhatobs.list if it is pmm.type 1, must feed in the yhatobs.list
  # param yobs.list  observed values in the original training data
  # param yhatobs.list predicted observed values in the original training data,
  # param sorted.dt sorted new data after initial imputation (with the same order as the original data)
  # param missing.vars names of variables with missing values in new data
  # param missing.types types of variable with missing values in new data
  # param sorted.names all names of variables in sorted order
  # param Ncol number of columns in new data

  for (var in missing.vars) {
    features <- setdiff(sorted.names, var)
    form <- reformulate(termlabels = features, response = var)

    na.idx <- Na.idx[[var]]

    if (Ncol == 2) {
      mis.data <- sparse.model.matrix(form, data = sorted.dt[na.idx, ])
    } else {
      mis.data <- sparse.model.matrix(form, data = sorted.dt[na.idx, ])[, -1, drop = FALSE]
    }
    # numeric or integer ---------------------------------------------------------------------------
    if (missing.types[var] == "numeric" | missing.types[var] == "integer") {
      yhatmis <- predict(xgb.models[[var]], mis.data)

      if (!is.null(pmm.type)) {
        if (isTRUE(pmm.type == 1)) {
          yhatmis <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        } else {
          yhatmis <- pmm(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
      }
      # update dataset
      sorted.dt[[var]][na.idx] <- yhatmis
    } else if (missing.types[var] == "binary") {
      # binary ---------------------------------------------------------------------------
      if (length(xgb.models[[var]]) == 1) {
        sorted.dt[[var]][na.idx] <- xgb.models[[var]]
        msg <- paste("Imputation for variable", var, "use the only existent class in the bootstrap sample. May not be reliable.")
        warning(msg)
      } else {
        yhatmis <- predict(xgb.models[[var]], mis.data)
        if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
          # for pmm.type=NULL or "auto"
          yhatmis <- ifelse(yhatmis >= 0.5, 1, 0)
          sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
        } else {
          if (pmm.type == 1) {
            # for pmm.type=1
            sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          } else {
            # for pmm.type=0 or 2
            sorted.dt[[var]][na.idx] <- pmm(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
          }
        }
      }
    } else {
      # multiclass ---------------------------------------------------------------------------

      if (is.null(pmm.type) | isTRUE(pmm.type == "auto")) {
        # use softmax, predict returns class
        # for pmm.type=NULL or "auto"
        yhatmis <- predict(xgb.models[[var]], mis.data)
        sorted.dt[[var]][na.idx] <- levels(sorted.dt[[var]])[yhatmis + 1]
      } else {
        # predict returns probability matrix for each class
        yhatmis <- predict(xgb.models[[var]], mis.data, reshape = TRUE)
        if (pmm.type == 1) {
          # for pmm.type=1
          sorted.dt[[var]][na.idx] <- pmm.multiclass(yhatobs = yhatobs.list[[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        } else {
          # for pmm.type=0 or 2
          sorted.dt[[var]][na.idx] <- pmm.multiclass(yhatobs = yhatobs.list[[m.set]][[var]], yhatmis = yhatmis, yobs = yobs.list[[var]], k = pmm.k)
        }
      }
    }
  } # end of for each missing variable

  return(sorted.dt)
}
