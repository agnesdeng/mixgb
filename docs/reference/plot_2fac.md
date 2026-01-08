# Bar plots for two imputed factor variables

Plot observed values with m sets of imputed values for two specified
numeric variables using ggplot2.

## Usage

``` r
plot_2fac(
  imputation.list,
  var.fac1,
  var.fac2,
  original.data,
  true.data = NULL,
  color.pal = NULL
)
```

## Arguments

- imputation.list:

  A list of `m` imputed datasets returned by the `mixgb` imputer

- var.fac1:

  A factor variable

- var.fac2:

  A factor variable

- original.data:

  The original data with missing data

- true.data:

  The true data without missing values. In general, this is unknown.
  Only use for simulation studies.

- color.pal:

  A vector of hex color codes for the observed and m sets of imputed
  values panels. The vector should be of length `m+1`. Default: NULL
  (use "gray40" for the observed panel, use ggplot2 default colors for
  other panels.)

## Value

Scatter plots for two numeric/integer variable

## Examples

``` r
# \donttest{
#create some extra missing values in factor variables "HSSEX" and "DMARETHN"
nhanes3_NA<-createNA(nhanes3, var.names = c("HSSEX","DMARETHN"), p = 0.1)

# obtain m multiply datasets
library(mixgb)
imputed.data <- mixgb(data = nhanes3_NA, m = 2)

# plot the multiply imputed values for variables "HSSEX" versus "DMARETHN"
plot_2fac(
  imputation.list = imputed.data, var.fac1 = "DMARETHN", var.fac2 = "HSSEX",
  original.data = nhanes3_NA
)

# }
```
