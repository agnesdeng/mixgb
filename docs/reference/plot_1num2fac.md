# Box plots with overlaying data points for a numeric variable vs a factor condition on another factor

Plot observed values versus m sets of imputed values for one specified
numeric variable and two factors using ggplot2.

## Usage

``` r
plot_1num2fac(
  imputation.list,
  var.fac,
  var.num,
  con.fac,
  original.data,
  true.data = NULL,
  color.pal = NULL,
  shape = FALSE,
  point.size = 1.5
)
```

## Arguments

- imputation.list:

  A list of `m` imputed datasets returned by the `mixgb` imputer

- var.fac:

  A factor variable on the x-axis

- var.num:

  A numeric variable on the y-axis

- con.fac:

  A conditional factor

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

- shape:

  Whether to plot shapes for different types of missing values. By
  default, this is set to FALSE to speed up plotting. We only recommend
  using \`shape = TRUE\` for small datasets.

- point.size:

  The size of point. Default: 1.5

## Value

Boxplots with overlaying data points

## Examples

``` r
# \donttest{
# create some extra missing values in factor variables "HSSEX" and "DMARETHN"
nhanes3_NA <- createNA(nhanes3, var.names = c("HSSEX", "DMARETHN"), p = 0.1)
# obtain m multiply datasets
library(mixgb)
imputed.data <- mixgb(data = nhanes3_NA, m = 5)

# plot the multiply imputed values for variables "BMPRECUM" versus "HSSEX"
# conditional on "DMARETHN"
plot_1num2fac(
  imputation.list = imputed.data, var.fac = "HSSEX", var.num = "BMPRECUM",
  con.fac = "DMARETHN", original.data = nhanes3_NA
)

# }
```
