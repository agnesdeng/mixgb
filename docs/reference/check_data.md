# Sanity check for input data before imputation

The function \`check_data()\` serves the purpose of performing a
preliminary check and fix some evident issues. However, the function
cannot resolve all data quality-related problems.

## Usage

``` r
check_data(data, max_levels = round(0.5 * nrow(data)), verbose = TRUE)
```

## Arguments

- data:

  A data frame or data table.

- max_levels:

  An integer specifying the maximum number of levels allowed for a
  factor variable. This is used to detect potential ID columns that are
  often non-informative for imputation. Default: 50% of the number of
  rows, rounded to the nearest integer.

- verbose:

  Verbose setting. If `TRUE`, will print out warnings when data issues
  are found. Default: `TRUE`.

## Value

A preliminary checked dataset

## Examples

``` r
bad_data <- data.frame(Amount = c(Inf, 10, 201.5), Type = factor(c("NaN", "B", "A")))
checked_data <- check_data(data = bad_data, verbose = TRUE)
#> 
#> ── Sanity checks begins ────────────────────────────────────────────────────────
#> ! Character "NaN" values detected in `Type`. They have been converted to NA.
#> ! Numeric Inf/-Inf values detected in `Amount`.They have been converted to NA.
#> ✔ All sanity checks completed.
```
