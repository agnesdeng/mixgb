# mixgb: Multiple Imputation Through XGBoost

## Introduction

Mixgb offers a scalable solution for imputing large datasets using
XGBoost, subsampling and predictive mean matching. Our method utilizes
the capabilities of XGBoost, a highly efficient implementation of
gradient boosted trees, to capture interactions and non-linear relations
automatically. Moreover, we have integrated subsampling and predictive
mean matching to minimize bias and reflect appropriate imputation
variability. Our package supports various types of variables and offers
flexible settings for subsampling and predictive mean matching. We also
include diagnostic tools for evaluating the quality of the imputed
values.

## Impute missing values with `mixgb`

We first load the `mixgb` package and the `nhanes3_newborn` dataset,
which contains 16 variables of various types
(integer/numeric/factor/ordinal factor). There are 9 variables with
missing values.

``` r
library(mixgb)
str(nhanes3_newborn)
#> tibble [2,107 × 16] (S3: tbl_df/tbl/data.frame)
#>  $ HSHSIZER: int [1:2107] 4 3 5 4 4 3 5 3 3 3 ...
#>  $ HSAGEIR : int [1:2107] 2 5 10 10 8 3 10 7 2 7 ...
#>  $ HSSEX   : Factor w/ 2 levels "1","2": 2 1 2 2 1 1 2 2 2 1 ...
#>  $ DMARACER: Factor w/ 3 levels "1","2","3": 1 1 2 1 1 1 2 1 2 2 ...
#>  $ DMAETHNR: Factor w/ 3 levels "1","2","3": 3 1 3 3 3 3 3 3 3 3 ...
#>  $ DMARETHN: Factor w/ 4 levels "1","2","3","4": 1 3 2 1 1 1 2 1 2 2 ...
#>  $ BMPHEAD : num [1:2107] 39.3 45.4 43.9 45.8 44.9 42.2 45.8 NA 40.2 44.5 ...
#>   ..- attr(*, "label")= chr "Head circumference (cm)"
#>  $ BMPRECUM: num [1:2107] 59.5 69.2 69.8 73.8 69 61.7 74.8 NA 64.5 70.2 ...
#>   ..- attr(*, "label")= chr "Recumbent length (cm)"
#>  $ BMPSB1  : num [1:2107] 8.2 13 6 8 8.2 9.4 5.2 NA 7 5.9 ...
#>   ..- attr(*, "label")= chr "First subscapular skinfold (mm)"
#>  $ BMPSB2  : num [1:2107] 8 13 5.6 10 7.8 8.4 5.2 NA 7 5.4 ...
#>   ..- attr(*, "label")= chr "Second subscapular skinfold (mm)"
#>  $ BMPTR1  : num [1:2107] 9 15.6 7 16.4 9.8 9.6 5.8 NA 11 6.8 ...
#>   ..- attr(*, "label")= chr "First triceps skinfold (mm)"
#>  $ BMPTR2  : num [1:2107] 9.4 14 8.2 12 8.8 8.2 6.6 NA 10.9 7.6 ...
#>   ..- attr(*, "label")= chr "Second triceps skinfold (mm)"
#>  $ BMPWT   : num [1:2107] 6.35 9.45 7.15 10.7 9.35 7.15 8.35 NA 7.35 8.65 ...
#>   ..- attr(*, "label")= chr "Weight (kg)"
#>  $ DMPPIR  : num [1:2107] 3.186 1.269 0.416 2.063 1.464 ...
#>   ..- attr(*, "label")= chr "Poverty income ratio"
#>  $ HFF1    : Factor w/ 2 levels "1","2": 2 2 1 1 1 2 2 1 2 1 ...
#>  $ HYD1    : Ord.factor w/ 5 levels "1"<"2"<"3"<"4"<..: 1 3 1 1 1 1 1 1 2 1 ...
colSums(is.na(nhanes3_newborn))
#> HSHSIZER  HSAGEIR    HSSEX DMARACER DMAETHNR DMARETHN  BMPHEAD BMPRECUM 
#>        0        0        0        0        0        0      124      114 
#>   BMPSB1   BMPSB2   BMPTR1   BMPTR2    BMPWT   DMPPIR     HFF1     HYD1 
#>      161      169      124      167      117      192        7        0
```

To impute this dataset, we can use the default settings. The default
number of imputed datasets is `m = 5`. Note that we do not need to
convert our data into dgCMatrix or one-hot coding format. Our package
will automatically convert it for you. Variables should be of the
following types: numeric, integer, factor or ordinal factor.

``` r
# use mixgb with default settings
imputed.data <- mixgb(data = nhanes3_newborn, m = 5)
```

### Customize imputation settings

We can also customize imputation settings:

- The number of imputed datasets `m`

- The number of imputation iterations `maxit`

- XGBoost hyperparameters and verbose settings. `xgb.params`, `nrounds`,
  `early_stopping_rounds`, `print_every_n` and `verbose`.

- Subsampling ratio. By default, `subsample = 0.7`. Users can change
  this value under the `xgb.params` argument.

- Predictive mean matching settings `pmm.type`, `pmm.k` and `pmm.link`.

- Whether ordinal factors should be converted to integer (imputation
  process may be faster) `ordinalAsInteger`

- Initial imputation methods for different types of variables
  `initial.num`, `initial.int` and `initial.fac`.

- Whether to save models for imputing newdata `save.models` and
  `save.vars`.

``` r
# Use mixgb with chosen settings
params <- list(
  max_depth = 5,
  subsample = 0.9,
  nthread = 2,
  tree_method = "hist"
)

imputed.data <- mixgb(
  data = nhanes3_newborn, m = 10, maxit = 2,
  ordinalAsInteger = FALSE, 
  pmm.type = "auto", pmm.k = 5, pmm.link = "prob",
  initial.num = "normal", initial.int = "mode", initial.fac = "mode",
  save.models = FALSE, save.vars = NULL,
  xgb.params = params, nrounds = 200, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0
)
```

### Tune hyperparameters

Imputation performance can be affected by the hyperparameter settings.
Although tuning a large set of hyperparameters may appear intimidating,
it is often possible to narrowing down the search space because many
hyperparameters are correlated. In our package, the function
[`mixgb_cv()`](../reference/mixgb_cv.md) can be used to tune the number
of boosting rounds - `nrounds`. There is no default `nrounds` value in
`XGBoost,` so users are required to specify this value themselves. The
default `nrounds` in [`mixgb()`](../reference/mixgb.md) is 100. However,
we recommend using [`mixgb_cv()`](../reference/mixgb_cv.md) to find the
optimal `nrounds` first.

``` r
params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
cv.results <- mixgb_cv(data = nhanes3_newborn, nrounds = 100, xgb.params = params, verbose = FALSE)
cv.results$evaluation.log
#>      iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
#>     <int>           <num>          <num>          <num>         <num>
#>  1:     1       1.2382245     0.01524170      1.2462764    0.05407329
#>  2:     2       1.0172800     0.01978693      1.0346177    0.06796148
#>  3:     3       0.8716095     0.02392298      0.8973763    0.07628028
#>  4:     4       0.7737795     0.02485966      0.8069313    0.08639529
#>  5:     5       0.7107600     0.02636517      0.7475762    0.09090390
#>  6:     6       0.6696049     0.02619157      0.7103543    0.09955684
#>  7:     7       0.6376157     0.02758166      0.6890671    0.10248279
#>  8:     8       0.6171461     0.02818625      0.6717541    0.10640338
#>  9:     9       0.6032265     0.02889641      0.6618395    0.10739952
#> 10:    10       0.5925047     0.02951154      0.6546447    0.10994697
#> 11:    11       0.5838466     0.02945878      0.6492278    0.11131472
#> 12:    12       0.5745035     0.02943350      0.6447715    0.11071591
#> 13:    13       0.5694571     0.02856204      0.6411329    0.11163370
#> 14:    14       0.5610366     0.02652188      0.6386005    0.11142349
#> 15:    15       0.5558165     0.02702086      0.6376387    0.11026971
#> 16:    16       0.5520989     0.02739661      0.6361806    0.10904214
#> 17:    17       0.5439046     0.02391973      0.6376113    0.10932729
#> 18:    18       0.5385980     0.02279961      0.6381685    0.10894676
#> 19:    19       0.5341658     0.02270163      0.6379352    0.10928847
#> 20:    20       0.5314952     0.02173837      0.6376643    0.10855460
#> 21:    21       0.5277887     0.02279365      0.6371404    0.10729228
#> 22:    22       0.5233745     0.02125712      0.6382106    0.10547097
#> 23:    23       0.5200587     0.02157460      0.6388530    0.10678994
#> 24:    24       0.5169381     0.02148033      0.6397333    0.10653500
#> 25:    25       0.5132553     0.01970753      0.6389741    0.10791424
#> 26:    26       0.5109004     0.01959456      0.6390225    0.10727696
#>      iter train_rmse_mean train_rmse_std test_rmse_mean test_rmse_std
cv.results$response
#> [1] "BMPWT"
cv.results$best.nrounds
#> [1] 16
```

By default, [`mixgb_cv()`](../reference/mixgb_cv.md) will randomly
choose an incomplete variable as the response and build an XGBoost model
with other variables as explanatory variables using the complete cases
of the dataset. Therefore, each run of
[`mixgb_cv()`](../reference/mixgb_cv.md) will likely return different
results. Users can also specify the response and covariates in the
argument `response` and `select_features` respectively.

``` r
cv.results <- mixgb_cv(
  data = nhanes3_newborn, nfold = 10, nrounds = 100, early_stopping_rounds = 1,
  response = "BMPHEAD", select_features = c("HSAGEIR", "HSSEX", "DMARETHN", "BMPRECUM", "BMPSB1", "BMPSB2", "BMPTR1", "BMPTR2", "BMPWT"), xgb.params = params, verbose = FALSE
)

cv.results$best.nrounds
#> [1] 12
```

Let us just try setting `nrounds = cv.results$best.nrounds` in
[`mixgb()`](../reference/mixgb.md) to obtain 5 imputed datasets.

``` r
imputed.data <- mixgb(data = nhanes3_newborn, m = 5, nrounds = cv.results$best.nrounds)
```

## Inspect multiply imputed values

The `mixgb` package used to provide a few visual diagnostics functions.
However, we have moved these functions to the `vismi` package, which
provides a wide range of visualisation tools for multiple imputation.

For more details, please check the `vismi` package on GitHub
[Visualisation Tools for Multiple
Imputation](https://github.com/agnesdeng/vismi).
