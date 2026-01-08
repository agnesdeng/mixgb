# Inspection plot of multiply imputed values for three variables

High-level function to plot observed values with m sets of imputed
values for three variables using ggplot2.

## Usage

``` r
plot3D(
  imputation.list,
  var.x,
  var.y,
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

- var.x:

  A numeric variable on the x-axis

- var.y:

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

## Examples

``` r
# obtain m multiply datasets
library(mixgb)
imputed.data <- mixgb(data = nhanes3, m = 2)

# plot the multiply imputed values for variables "BMPRECUM" versus "BMPHEAD"
# conditional on "HSSEX"
plot3D(
  imputation.list = imputed.data, var.x = "BMPHEAD", var.y = "BMPRECUM",
  con.fac = "HSSEX", original.data = nhanes3
)
#> Error in plot3D(imputation.list = imputed.data, var.x = "BMPHEAD", var.y = "BMPRECUM",     con.fac = "HSSEX", original.data = nhanes3): could not find function "plot3D"
```
