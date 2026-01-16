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
#>      age_months    sex        ethnicity head_circumference_cm
#>           <int> <fctr>           <fctr>                 <num>
#>   1:          2   Male     Not Hispanic              40.70000
#>   2:          6   Male     Not Hispanic              45.10000
#>   3:          4 Female     Not Hispanic              43.00000
#>   4:          3   Male Mexican-American              43.00000
#>   5:          8   Male Mexican-American              42.70341
#>  ---                                                         
#> 346:          4 Female     Not Hispanic              42.30000
#> 347:          3 Female     Not Hispanic              38.40000
#> 348:         10   Male     Not Hispanic              45.20000
#> 349:          8   Male     Not Hispanic              47.00000
#> 350:          7   Male     Not Hispanic              48.20000
#>      recumbent_length_cm weight_kg
#>                    <num>     <num>
#>   1:            59.20000  6.400000
#>   2:            68.60000  8.050000
#>   3:            63.00000  7.800000
#>   4:            62.00000  6.200000
#>   5:            67.01804  7.270353
#>  ---                              
#> 346:            65.80000  7.450000
#> 347:            58.00000  5.300000
#> 348:            75.90000  8.650000
#> 349:            74.70000 10.500000
#> 350:            72.50000  9.800000
#> 
#> [[2]]
#>      age_months    sex        ethnicity head_circumference_cm
#>           <int> <fctr>           <fctr>                 <num>
#>   1:          2   Male     Not Hispanic              40.70000
#>   2:          6   Male     Not Hispanic              45.10000
#>   3:          4 Female     Not Hispanic              43.00000
#>   4:          3   Male Mexican-American              43.00000
#>   5:          8   Male Mexican-American              43.15326
#>  ---                                                         
#> 346:          4 Female     Not Hispanic              42.30000
#> 347:          3 Female     Not Hispanic              38.40000
#> 348:         10   Male     Not Hispanic              45.20000
#> 349:          8   Male     Not Hispanic              47.00000
#> 350:          7   Male     Not Hispanic              48.20000
#>      recumbent_length_cm weight_kg
#>                    <num>     <num>
#>   1:            59.20000  6.400000
#>   2:            68.60000  8.050000
#>   3:            63.00000  7.800000
#>   4:            62.00000  6.200000
#>   5:            66.67313  7.614265
#>  ---                              
#> 346:            65.80000  7.450000
#> 347:            58.00000  5.300000
#> 348:            75.90000  8.650000
#> 349:            74.70000 10.500000
#> 350:            72.50000  9.800000
#> 

# use the saved imputer to impute new data
test.imputed <- impute_new(object = mixgb.obj, newdata = test.data)
test.imputed
#> [[1]]
#>      age_months    sex    ethnicity head_circumference_cm recumbent_length_cm
#>           <int> <fctr>       <fctr>                 <num>               <num>
#>   1:         11   Male Not Hispanic                  46.3                74.0
#>   2:          8   Male Not Hispanic                  47.7                73.1
#>   3:          5   Male Not Hispanic                  44.6                71.3
#>   4:          9 Female Not Hispanic                  44.8                69.3
#>   5:          2 Female Not Hispanic                  41.1                58.5
#>  ---                                                                         
#> 146:          8   Male Not Hispanic                  47.1                71.8
#> 147:          7   Male Not Hispanic                  46.3                70.0
#> 148:          4   Male Not Hispanic                  44.4                66.7
#> 149:          6   Male Not Hispanic                  43.1                67.8
#> 150:          7   Male Not Hispanic                  46.8                73.6
#>      weight_kg
#>          <num>
#>   1:      9.50
#>   2:      9.75
#>   3:      7.90
#>   4:      7.65
#>   5:      5.85
#>  ---          
#> 146:      9.80
#> 147:      8.55
#> 148:      7.50
#> 149:      7.65
#> 150:     10.05
#> 
#> [[2]]
#>      age_months    sex    ethnicity head_circumference_cm recumbent_length_cm
#>           <int> <fctr>       <fctr>                 <num>               <num>
#>   1:         11   Male Not Hispanic                  46.3                74.0
#>   2:          8   Male Not Hispanic                  47.7                73.1
#>   3:          5   Male Not Hispanic                  44.6                71.3
#>   4:          9 Female Not Hispanic                  44.8                69.3
#>   5:          2 Female Not Hispanic                  41.1                58.5
#>  ---                                                                         
#> 146:          8   Male Not Hispanic                  47.1                71.8
#> 147:          7   Male Not Hispanic                  46.3                70.0
#> 148:          4   Male Not Hispanic                  44.4                66.7
#> 149:          6   Male Not Hispanic                  43.1                67.8
#> 150:          7   Male Not Hispanic                  46.8                73.6
#>      weight_kg
#>          <num>
#>   1:      9.50
#>   2:      9.75
#>   3:      7.90
#>   4:      7.65
#>   5:      5.85
#>  ---          
#> 146:      9.80
#> 147:      8.55
#> 148:      7.50
#> 149:      7.65
#> 150:     10.05
#> 
```
