# Multiple imputation using xgboost (without bootstrap) updated version
mixgb_null <- function(Obs.m, matrix.method, cbind.types, pmm.type, pmm.link, pmm.k, yobs.list, yhatobs.list = NULL, sorted.dt, missing.vars, sorted.names, Na.idx, missing.types, Ncol,
                       xgb.params = list(),
                       nrounds, early_stopping_rounds, print_every_n, verbose,
                       ...) {
  nthread <- xgb.params$nthread


  for (var in missing.vars) {
    na.idx <- Na.idx[[var]]
    obs.y <- yobs.list[[var]]

    # Mis.vars: missing variables except the current imputed variable (as response)
    if (length(missing.vars) != 1) {
      Mis.vars <- missing.vars[missing.vars != var]

      if (matrix.method == "as.matrix") {
        Mis.m <- as.matrix(sorted.dt[, Mis.vars, with = FALSE])
      } else {
        Mis.list <- lapply(Mis.vars, function(feature) {
          if (cbind.types[feature] %in% c("numeric", "integer")) {
            as.matrix(sorted.dt[[feature]])
          } else if (cbind.types[feature] == "ordered") {
            Matrix::t(fac2Sparse(sorted.dt[[feature]], drop.unused.levels = FALSE, factorPatt12 = c(T, F), contrasts.arg = "contr.poly")[[1]])
          } else {
            Matrix::t(fac2sparse(sorted.dt[[feature]], drop.unused.levels = FALSE))[, -1, drop = FALSE]
          }
        })


        if (matrix.method == "cpp.combo") {
          Mis.m <- cbind_combo(Mis.list)
        } else if (matrix.method == "cpp.factor") {
          Mis.m <- cbind_sparse_matrix(Mis.list)
        }
      }

      All.m <- cbind2(Mis.m, Obs.m)
    } else {
      All.m <- Obs.m
    }


    obs.data <- All.m[-na.idx, , drop = FALSE]
    mis.data <- All.m[na.idx, , drop = FALSE]

    if (missing.types[var] == "numeric") {
      yhatmis <- impute_numeric(
        save = FALSE, obs.data, mis.data, obs.y, xgb.params,
        nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
        pmm.type, pmm.k,
        yhatobs = yhatobs.list[[var]],
        round_nopmm = FALSE
      )
    } else if (missing.types[var] == "integer") {
      yhatmis <- impute_numeric(
        save = FALSE, obs.data, mis.data, obs.y, xgb.params,
        nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
        pmm.type, pmm.k,
        yhatobs = yhatobs.list[[var]],
        round_nopmm = TRUE
      )
    } else if (missing.types[var] == "binary") {
      yhatmis <- impute_binary(var,
        save = FALSE, obs.data, mis.data, obs.y, xgb.params,
        nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
        pmm.type, pmm.k, pmm.link,
        yhatobs = yhatobs.list[[var]],
        original_levels = levels(sorted.dt[[var]])
      )
    } else if (missing.types[var] == "logical") {
      yhatmis <- impute_binary(var,
        save = FALSE, obs.data, mis.data, obs.y, xgb.params,
        nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
        pmm.type, pmm.k, pmm.link,
        yhatobs = yhatobs.list[[var]],
        original_levels = levels(sorted.dt[[var]])
      )
    } else if (missing.types[var] == "multiclass") {
      yhatmis <- impute_multiclass(
        save = FALSE, obs.data, mis.data, obs.y, xgb.params,
        nrounds, nthread, early_stopping_rounds, print_every_n, verbose,
        pmm.type, pmm.k,
        yhatobs = yhatobs.list[[var]],
        original_levels = levels(sorted.dt[[var]])
      )
    }

    sorted.dt[na.idx, (var) := yhatmis]
  } # end of for each missing variable
  sorted.dt
}
