# Show multiply imputed values for a single variable

Show m sets of imputed values for a specified variable.

## Usage

``` r
show_var(data, imp_list, x, true_values = NULL)
```

## Arguments

- data:

  The original data with missing data.

- imp_list:

  A list of `m` imputed datasets returned by the `mixgb` imputer.

- x:

  The name of a variable of interest.

- true_values:

  A vector of the true values (if known) of the missing values. In
  general, this is unknown.

## Value

A data.table with `m` columns, each column represents the imputed values
of all missing entries in the specified variable. If `true_values` is
provided, the last column will be the true values of the missing values.

## Examples

``` r
# obtain m multiply datasets
library(mixgb)
imp_list <- mixgb(data = nhanes3, m = 3)

imp_head <- show_var(
  imp_list = imp_list, x = "head_circumference_cm",
  data = nhanes3
)
```
