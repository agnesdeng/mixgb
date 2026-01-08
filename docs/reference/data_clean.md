# Data cleaning

The function \`data_clean()\` serves the purpose of performing a
preliminary check and fix some evident issues. However, the function
cannot resolve all data quality-related problems.

## Usage

``` r
data_clean(rawdata, levels.tol = 0.2)
```

## Arguments

- rawdata:

  A data frame.

- levels.tol:

  Tolerant proportion of the number of levels to the number of
  observations in a multiclass variable. Default: 0.2

## Value

A preliminary cleaned dataset

## Examples

``` r
rawdata <- nhanes3

rawdata[4, 4] <- NaN
rawdata[5, 5] <- Inf
rawdata[6, 6] <- -Inf

cleandata <- data_clean(rawdata = rawdata)
#> Warning: There exists at least one entry coded as NaN in the following numeric variable(s): BMPHEAD.
#> It is now coverted to NA instead.
#> Warning: There exists at least one entry coded as Inf or -Inf in the following variable(s): BMPRECUM;BMPWT.
#> It is now coverted to NA instead.
```
