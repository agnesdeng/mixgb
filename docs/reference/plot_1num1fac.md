# Box plots with points for one numeric variable vs one factor (or integer) variable.

Plot observed values versus m sets of imputed values for one numeric
variable vs one factor (or integer) variable using ggplot2.

## Usage

``` r
plot_1num1fac(
  imputation.list,
  var.num,
  var.fac,
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

- var.num:

  A numeric variable

- var.fac:

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

- shape:

  Whether to plot shapes for different types of missing values. By
  default, this is set to FALSE to speed up plotting. We only recommend
  using \`shape = TRUE\` for small datasets.

- point.size:

  The size of point. Default: 1.5

## Value

Box plot with jittered data points for a numeric/integer variable; Bar
plot for a categorical variable.

## Examples

``` r
# obtain m multiply datasets
library(mixgb)
imputed.data <- mixgb(data = nhanes3, m = 2)

# plot the multiply imputed values for variables "BMPHEAD" versus "HSSEX"
plot_1num1fac(
  imputation.list = imputed.data, var.num = "BMPHEAD", var.fac = "HSSEX",
  original.data = nhanes3
)
```
