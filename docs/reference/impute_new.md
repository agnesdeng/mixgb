# Impute new data with a saved `mixgb` imputer object

Impute new data with a saved `mixgb` imputer object

## Usage

``` r
impute_new(
  object,
  newdata,
  initial.newdata = FALSE,
  pmm.k = NULL,
  m = NULL,
  verbose = FALSE
)
```

## Arguments

- object:

  A saved imputer object created by `mixgb(..., save.models = TRUE)`

- newdata:

  A data.frame or data.table. New data with missing values.

- initial.newdata:

  Whether to use the information from the new data to initially impute
  the missing values of the new data. By default, this is set to
  `FALSE`, the original data passed to [`mixgb()`](mixgb.md) will be
  used for initial imputation.

- pmm.k:

  The number of donors for predictive mean matching. If `NULL` (the
  default), the `pmm.k` value in the saved imputer object will be used.

- m:

  The number of imputed datasets. If `NULL` (the default), the `m` value
  in the saved imputer object will be used.

- verbose:

  Verbose setting for mixgb. If `TRUE`, will print out the progress of
  imputation. Default: `FALSE`.

## Value

A list of `m` imputed datasets for new data.

## Examples

``` r
set.seed(2022)
n <- nrow(nhanes3)
idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
train.data <- nhanes3[idx, ]
test.data <- nhanes3[-idx, ]

params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
mixgb.obj <- mixgb(data = train.data, m = 2, xgb.params = params, nrounds = 10,
                   save.models = TRUE, save.models.folder = tempdir())

# obtain m imputed datasets for train.data
train.imputed <- mixgb.obj$imputed.data
train.imputed
#> [[1]]
#>      HSAGEIR  HSSEX DMARETHN BMPHEAD BMPRECUM BMPWT
#>        <int> <fctr>   <fctr>   <num>    <num> <num>
#>   1:      10      2        3    45.7     70.2  8.15
#>   2:       3      1        4    42.0     63.5  7.50
#>   3:       3      2        1    43.2     63.1  6.75
#>   4:       9      2        2    44.4     73.5  8.30
#>   5:       9      1        1    45.3     73.8  9.10
#>  ---                                               
#> 346:       4      1        2    44.8     66.7  8.60
#> 347:       2      1        1    41.4     60.0  6.75
#> 348:       5      2        2    40.4     63.2  7.35
#> 349:       5      1        2    44.0     69.1  8.75
#> 350:      11      1        3    47.1     77.8 10.35
#> 
#> [[2]]
#>      HSAGEIR  HSSEX DMARETHN BMPHEAD BMPRECUM BMPWT
#>        <int> <fctr>   <fctr>   <num>    <num> <num>
#>   1:      10      2        3    45.7     70.2  8.15
#>   2:       3      1        4    42.0     63.5  7.50
#>   3:       3      2        1    43.2     63.1  6.75
#>   4:       9      2        2    44.4     73.5  8.30
#>   5:       9      1        1    45.3     73.8  9.10
#>  ---                                               
#> 346:       4      1        2    44.8     66.7  8.60
#> 347:       2      1        1    41.4     60.0  6.75
#> 348:       5      2        2    40.4     63.2  7.35
#> 349:       5      1        2    44.0     69.1  8.75
#> 350:      11      1        3    47.1     77.8 10.35
#> 

# use the saved imputer to impute new data
test.imputed <- impute_new(object = mixgb.obj, newdata = test.data)
test.imputed
#> [[1]]
#>      HSAGEIR  HSSEX DMARETHN BMPHEAD BMPRECUM BMPWT
#>        <int> <fctr>   <fctr>   <num>    <num> <num>
#>   1:       3      2        2    42.6     67.1  8.70
#>   2:       8      1        2    47.4     78.2 10.75
#>   3:       9      1        2    45.1     73.0  8.40
#>   4:       9      1        1    44.3     73.3  8.70
#>   5:       3      1        1    43.4     65.1  7.35
#>  ---                                               
#> 146:       2      1        3    35.7     49.5  3.80
#> 147:       7      1        1    45.5     70.7 10.40
#> 148:       6      2        4    46.7     66.9  8.10
#> 149:      11      2        4    46.4     73.5  9.15
#> 150:       9      2        2    45.9     71.0  8.55
#> 
#> [[2]]
#>      HSAGEIR  HSSEX DMARETHN BMPHEAD BMPRECUM BMPWT
#>        <int> <fctr>   <fctr>   <num>    <num> <num>
#>   1:       3      2        2    42.6     67.1  8.70
#>   2:       8      1        2    47.4     78.2 10.75
#>   3:       9      1        2    45.1     73.0  8.40
#>   4:       9      1        1    44.3     73.3  8.70
#>   5:       3      1        1    43.4     65.1  7.35
#>  ---                                               
#> 146:       2      1        3    35.7     49.5  3.80
#> 147:       7      1        1    45.5     70.7 10.40
#> 148:       6      2        4    46.7     66.9  8.10
#> 149:      11      2        4    46.4     73.5  9.15
#> 150:       9      2        2    45.9     71.0  8.55
#> 
```
