# Data cleaning

The function \`check_data()\` serves the purpose of performing a
preliminary check and fix some evident issues. However, the function
cannot resolve all data quality-related problems.

## Usage

``` r
check_data(data, verbose = TRUE, max_levels = 50, stringsAsFactors = TRUE)
```

## Arguments

- data:

  A data frame.

- verbose:

  Verbose setting. If `TRUE`, will print out warnings when data issues
  are found. Default: `TRUE`.

- max_levels:

  An integer specifying the maximum number of levels allowed for factor
  variables. Default: 50.

- stringsAsFactors:

  A logical value indicating whether all character vectors in the
  dataset should be converted to factors. Default: `TRUE`.

## Value

A preliminary cleaned dataset

## Examples

``` r
nhanes3[4, 4] <- NaN
nhanes3[5, 5] <- Inf
nhanes3[6, 6] <- -Inf

cleandata <- check_data(data = nhanes3)
#> 
#> ── Sanity Check ────────────────────────────────────────────────────────────────
#> ! Numeric NaN values detected in `head_circumference_cm`. They have been converted to NA.
#> ! Numeric Inf/-Inf values detected in `recumbent_length_cm` and `weight_kg`.They have been converted to NA.
```
