# Checking for pmm.type
check_pmm <- function(pmm.type, xgb.params, Nrow, sorted.naSums, sorted.types, pmm.k) {
  # at least one of the sampling-related hyperparameters in xgboost must be less than 1
  if (!is.null(pmm.type)) {
    if (!pmm.type %in% c(0, 1, 2, "auto")) {
      stop("The specified pmm.type is incorrect. It must be one of the following types: NULL,1,2,\"auto\".")
    }


    ## pmm.type=1 or pmm.type=2:  requires subsample<1
    if ((pmm.type == 1 | pmm.type == 2)) {
      sample.params <- grepl("sample", names(xgb.params))
      if (all(sample.params == FALSE)) {
        # no sample params specified by users, default are all 1
        warning("For multiple imputation through XGBoost with PMM type 1 or type 2, recommend to set at least of one of the following hyperparameters less than 1:\n subsample\n colsample_bytree\n colsample_bylevel\n colsample_bynode")
      } else {
        if (all(xgb.params[sample.params] == 1)) {
          warning("For multiple imputation through XGBoost with PMM type 1 or type 2, recommend to set at least one of the following hyperparameters less than 1:\n subsample\n colsample_bytree\n colsample_bylevel\n colsample_bynode")
        }
      }
    }



    ## pmm.type=0 does not allow subsampling
    if (pmm.type == 0) {
      sample.params <- grepl("sample", names(xgb.params))
      if (any(sample.params == TRUE)) {
        if (any(xgb.params[sample.params] != 1)) {
          stop("PMM type0 requires using the whole dataset. Sampling-related hyperparameters < 1 are not allowed. All the following hyperparameters must be 1:\n subsample\n colsample_bytree\n colsample_bylevel\n colsample_bynode.")
        }
      }
    }

    ## if pmm.type=0,1 or 2, all variables need to perform PMM
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
    ## if pmm.type="auto", only numeric variables need to perform PMM
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


check_data_format <- function(data) {
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame, tibble or data.table")
  }

  # Ensure data.table is handled safely
  if (inherits(data, "data.table")) {
    data <- data.table::copy(data)
  }
  data
}


# check character type variables --------------------------------------------
check_character <- function(data, verbose) {
  char_idx <- which(sapply(data, is.character))

  if (length(char_idx) >= 1) {
    char_cols <- colnames(data)[char_idx]

    msg <- "Character variable(s) detected"
    question <- "Do you want to convert them to factor type?"
    proceed <- .user_action(msg = msg, question = question, cols = char_cols)

    if (proceed) {
      if (isTRUE(verbose)) {
        cli::cli_alert_info("Converting character columns to factor: {.var {char_cols}}")
      }


      if (inherits(data, "data.table")) {
        for (col in char_cols) {
          data[, (col) := as.factor(get(col))]
        }
      } else {
        data[char_cols] <- lapply(data[char_cols], as.factor)
      }
    } else {
      stop("User chose not to proceed. Please check the data")
    }
  }
  data
}


# check NaN ---------------------------------------------------------------
check_NaN <- function(data, verbose) {
  nan_num <- do.call(cbind, lapply(data, is.nan))
  if (any(nan_num)) {
    col_idx <- unique(which(nan_num, arr.ind = TRUE)[, 2])
    nan_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Numeric {.emph NaN} values detected in {.var {nan_col}}.",
        " They have been converted to {.emph NA}."
      ))
    }
    data[nan_num] <- NA
  }

  nan_chr <- data == "NaN"
  nan_chr[is.na(nan_chr)] <- FALSE

  if (any(nan_chr)) {
    col_idx <- unique(which(nan_chr, arr.ind = TRUE)[, 2])
    nan_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Character {.emph \"NaN\"} values detected in {.var {nan_col}}.",
        " They have been converted to {.emph NA}."
      ))
    }
    data[nan_chr] <- NA
    # drop "NaN" level
    data <- droplevels(data)
  }
  data
}


# check Inf and -Inf ------------------------------------------------------
check_inf <- function(data, verbose) {
  inf_num <- do.call(cbind, lapply(data, is.infinite))

  if (any(inf_num)) {
    col_idx <- unique(which(inf_num, arr.ind = TRUE)[, 2])
    inf_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Numeric {.emph Inf/-Inf} values detected in {.var {inf_col}}.",
        "They have been converted to {.emph NA}."
      ))
    }
    data[inf_num] <- NA
  }

  inf_chr <- (data == "Inf" | data == "-Inf")
  inf_chr[is.na(inf_chr)] <- FALSE

  if (any(inf_chr)) {
    col_idx <- unique(which(inf_chr, arr.ind = TRUE)[, 2])
    inf_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Character / factor {.emph \"Inf\" / \"-Inf\"} values detected in {.var {inf_col}}",
        "They have been converted to {.emph NA}."
      ))
    }
    data[inf_chr] <- NA
    # drop "Inf"/"-Inf" level
    data <- droplevels(data)
  }
  data
}


# check empty cells "" ----------------------------------------------------
check_empty <- function(data, verbose) {
  empty_chr <- data == ""
  empty_chr[is.na(empty_chr)] <- FALSE

  if (any(empty_chr)) {
    col_idx <- unique(which(empty_chr, arr.ind = TRUE)[, 2])
    empty_col <- colnames(data)[col_idx]
    if (isTRUE(verbose)) {
      cli::cli_alert_warning(c(
        "Empty string {.emph \"\"} values detected in {.var {empty_col}}.",
        " They have been converted to {.emph NA}."
      ))
    }
    # replace empty strings with NA
    data[empty_chr] <- NA

    # drop empty string level from factors
    data <- droplevels(data)
  }
  data
}


# check single level factor -----------------------------------------------
check_single_level <- function(data, verbose) {
  # nlevels only work for factor
  # idx <- which(sapply(data, nlevels) == 1)
  n_levels <- vapply(data, function(x) length(unique(x)), numeric(1))
  idx <- which(n_levels == 1)

  if (length(idx) > 0) {
    single_cols <- colnames(data)[idx]

    msg <- "Variable(s) with only one level detected"
    question <- "Do you want to remove them from the data and proceed?"
    proceed <- .user_action(msg = msg, question = question, cols = single_cols)

    if (proceed) {
      if (isTRUE(verbose)) {
        cli::cli_alert_warning("Removing variables with only one level: {.var {single_cols}}")
      }

      if (inherits(data, "data.table")) {
        data[, (single_cols) := NULL]
      } else {
        data <- data[, -idx, drop = FALSE]
      }
    } else {
      cli::cli_abort("User chose not to proceed. Please inspect variables with only one level: {.var {single_cols}}")
    }
  }
  data
}

# check ID-like columns: factors with too many levels -------------------------------------
check_ID <- function(data, verbose, max_levels = 0.5 * nrow(data)) {
  num_levels <- sapply(data, nlevels)
  num_row <- nrow(data)

  idx <- which(num_levels > max_levels)
  if (length(idx) > 0) {
    ID_cols <- colnames(data)[idx]

    msg <- paste("Factor variable(s) with more than", max_levels, "levels detected")
    question <- "Do you want to remove them from the data and proceed?"
    proceed <- .user_action(msg = msg, question = question, cols = ID_cols)

    if (proceed) {
      if (isTRUE(verbose)) {
        cli::cli_alert_warning("Removing ID-like variables with too many levels: {.var {ID_cols}}")
      }
    } else {
      cli::cli_abort("User chose not to proceed. Please inspect ID-like variables with too many levels: {.var {ID_cols}}")
    }
  }
  data
}

# helper: ask for user input
.user_action <- function(msg, question = NULL, cols) {
  if (!interactive()) {
    return(FALSE)
  }
  cat("\n", msg, "in: ", paste(cli::col_blue(cols), collapse = ", "), "\n")

  if (is.null(question)) {
    question <- "Do you want to proceed with the fix?"
  }

  choice <- utils::menu(
    choices = c("Yes, please.", "Stop, I'd like to take a closer look at these flagged columns first."),
    title = paste("\n>", question)
  )

  # Return TRUE if they picked option 1, FALSE otherwise
  return(choice == 1)
}
