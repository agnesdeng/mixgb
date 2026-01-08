# Show multiply imputed values for a single variable

Show m sets of imputed values for a specified variable.

## Usage

``` r
show_var(imputation.list, var.name, original.data, true.values = NULL)
```

## Arguments

- imputation.list:

  A list of `m` imputed datasets returned by the `mixgb` imputer.

- var.name:

  The name of a variable of interest.

- original.data:

  The original data with missing data.

- true.values:

  A vector of the true values (if known) of the missing values. In
  general, this is unknown.

## Value

A data.table with `m` columns, each column represents the imputed values
of all missing entries in the specified variable. If `true.values` is
provided, the last column will be the true values of the missing values.

## Examples

``` r
#obtain m multiply datasets
library(mixgb)
mixgb.data <- mixgb(data = nhanes3, m = 3)

imputed.BMPHEAD <- show_var(imputation.list = mixgb.data, var.name = "BMPHEAD",
  original.data = nhanes3)
```
