#' Data cleaning
#' @description  The function `check_data()` serves the purpose of performing a preliminary check and fix some evident issues. However, the function cannot resolve all data quality-related problems.
#' @param data A data frame.
#' @param verbose Verbose setting. If \code{TRUE}, will print out warnings when data issues are found. Default: \code{TRUE}.
#' @param max_levels An integer specifying the maximum number of levels allowed for factor variables. Default: 50.
#' @param stringsAsFactors A logical value indicating whether all character vectors in the dataset should be converted to factors. Default: \code{TRUE}.
#' @return A preliminary cleaned dataset
#' @export
#' @examples
#' nhanes3[4, 4] <- NaN
#' nhanes3[5, 5] <- Inf
#' nhanes3[6, 6] <- -Inf
#'
#' cleandata <- check_data(data = nhanes3)
check_data <- function(data, verbose = TRUE, max_levels = 50, stringsAsFactors = TRUE) {



  if (verbose) {
    cli::cli_h1("Sanity Check")
  }

  # check if data is data.frame, tibble or data.table -----------------------
  if (!is.data.frame(data)) {
    stop("data must be a data.frame, tibble or data.table.")
  }

  Names <- colnames(data)

  # check character type variables --------------------------------------------
  char_idx <- which(sapply(data, is.character))

  if (length(char_idx) >= 1) {
    char_cols <- names(data)[char_idx]

    if (interactive()) {
      cat("Character variable(s) detected in:", paste(char_cols, collapse = ", "), "\n")
      proceed<-askYesNo("Do you want to convert them to factor type?")
    } else {
      proceed <- FALSE # default for non-interactive sessions
    }

    if (proceed) {
      cli::cli_alert_info("Converting character columns to factor: {.var {char_cols}}")

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

  # check NaN ---------------------------------------------------------------
  nan_num <- do.call(cbind, lapply(data, is.nan))
  if (any(nan_num)) {
    col_idx <- unique(which(nan_num, arr.ind = TRUE)[, 2])
    nan_col <- Names[col_idx]
    if (verbose) {
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
    nan_col <- Names[col_idx]

    cli::cli_alert_warning(c(
      "Character {.emph \"NaN\"} values detected in {.var {nan_col}}.",
      " They have been converted to {.emph NA}."
    ))

    data[nan_chr] <- NA
    # drop "NaN" level
    data <- droplevels(data)
  }

  # check Inf and -Inf ------------------------------------------------------
  inf_num <- do.call(cbind, lapply(data, is.infinite))

  if (any(inf_num)) {
    col_idx <- unique(which(inf_num, arr.ind = TRUE)[, 2])
    inf_col <- names(data)[col_idx]

    cli::cli_alert_warning(c(
      "Numeric {.emph Inf/-Inf} values detected in {.var {inf_col}}.",
      "They have been converted to {.emph NA}."
    ))

    data[inf_num] <- NA
  }


  inf_chr <- (data == "Inf" | data == "-Inf")
  inf_chr[is.na(inf_chr)] <- FALSE

  if (any(inf_chr)) {
    col_idx <- unique(which(inf_chr, arr.ind = TRUE)[, 2])
    inf_col <- names(data)[col_idx]

    cli::cli_alert_warning(c(
      "Character / factor {.emph \"Inf\" / \"-Inf\"} values detected in {.var {inf_col}}",
      "They have been converted to {.emph NA}."
    ))

    data[inf_chr] <- NA
    # drop "Inf"/"-Inf" level
    data <- droplevels(data)
  }

  # check empty cells "" ----------------------------------------------------
  empty_chr <- data == ""
  empty_chr[is.na(empty_chr)] <- FALSE

  if (any(empty_chr)) {
    col_idx <- unique(which(empty_chr, arr.ind = TRUE)[, 2])
    empty_col <- Names[col_idx]

    cli::cli_alert_warning(c(
      "Empty string {.emph \"\"} values detected in {.var {empty_col}}.",
      " They have been converted to {.emph NA}."
    ))

    # replace empty strings with NA
    data[empty_chr] <- NA

    # drop empty string level from factors
    data <- droplevels(data)
  }



  # check single level factor -----------------------------------------------
  idx <- which(sapply(data, nlevels) == 1)

  if (length(idx) >= 1) {
    single_col <- names(data)[idx]

    if (interactive()) {

      cat("Factor variable(s) with only one level detected in:", paste(single_col, collapse = ", "), "\n")
      proceed<-askYesNo("Do you want to remove them from the data and proceed?")


    } else {
      proceed <- FALSE # default for non-interactive sessions
    }

    if (proceed) {
      cli::cli_alert_warning("Removing columns with only one level: {.var {single_col}}")
      if (inherits(data, "data.table")) {
        data[, (single_col) := NULL]
      } else {
        data <- data[, -idx, drop = FALSE]
      }
    } else {
      stop("User chose not to proceed. Please check the data")
    }
  }


  # check factors with too many levels -------------------------------------
  num_levels <- sapply(data, nlevels)
  num_row <- nrow(data)

  idx <- which(num_levels > max_levels)
  if (length(idx) > 0) {
    toomany_col <- names(data)[idx]


    if (interactive()) {
      cat("Factor variable(s) with more than", max_levels, "levels detected in:",
          paste(toomany_col, collapse = ", "), "\n")
      proceed<-askYesNo("Do you want to keep them and proceed?")
    } else {
      proceed <- FALSE # default for non-interactive sessions
    }

    if (proceed) {
      cli::cli_alert_warning("User chose to keep columns with too many levels: {.var {toomany_col}}")
    } else {
      cli::cli_abort("User chose not to proceed. Please check variables with too many levels: {.var {toomany_col}}")
    }
  }

  data
}
