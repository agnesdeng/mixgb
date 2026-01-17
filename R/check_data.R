#' Sanity check for input data before imputation
#' @description  The function `check_data()` serves the purpose of performing a preliminary check and fix some evident issues. However, the function cannot resolve all data quality-related problems.
#' @param data A data frame or data table.
#' @param max_levels An integer specifying the maximum number of levels allowed for a factor variable. This is used to detect potential ID columns that are often non-informative for imputation. Default: 50\% of the number of rows, rounded to the nearest integer.
#' @param verbose Verbose setting. If \code{TRUE}, will print out warnings when data issues are found. Default: \code{TRUE}.
#' @return A preliminary checked dataset
#' @export
#' @examples
#' bad_data <- data.frame(Amount = c(Inf, 10, 201.5), Type = factor(c("NaN", "B", "A")))
#' checked_data <- check_data(data = bad_data, verbose = TRUE)
check_data <- function(data, max_levels = round(0.5 * nrow(data)), verbose = TRUE) {
  if (isTRUE(verbose)) {
    cli::cli_h1("Sanity checks begins")
  }

  data <- data |>
    check_data_format() |>
    check_character(verbose) |>
    check_NaN(verbose) |>
    check_inf(verbose) |>
    check_empty(verbose) |>
    check_single_level(verbose) |>
    check_ID(verbose, max_levels)


  if (isTRUE(verbose)) {
    cli::cli_alert_success("All sanity checks completed.")
  }

  invisible(data)
}
