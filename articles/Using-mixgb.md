# mixgb: Multiple Imputation Through XGBoost

## Introduction

The **mixgb** package provides a scalable approach to imputation for
large data using XGBoost, subsampling, and predictive mean matching. It
leverages XGBoost—an efficient implementation of gradient-boosted
trees—to automatically capture complex interactions and non-linear
relationships. Subsampling and predictive mean matching are incorporated
to reduce bias and to preserve realistic imputation variability. The
package accommodates a wide range of variable types and offers flexible
control over subsampling and predictive matching settings.

We also recommend our package **vismi** ([Visualisation Tools for
Multiple Imputation](https://agnesdeng.github.io/vismi/)), which offers
a comprehensive set of diagnostics for assessing the quality of multiply
imputed data.

## Impute missing values with `mixgb`

We first load the `mixgb` package and the `newborn` dataset, which
contains 16 variables of various types (integer/numeric/factor/ordinal
factor). There are 9 variables with missing values.

``` r
library(mixgb)
str(newborn)
#> tibble [2,107 × 16] (S3: tbl_df/tbl/data.frame)
#>  $ household_size                : int [1:2107] 4 3 5 4 4 3 5 3 3 3 ...
#>  $ age_months                    : int [1:2107] 2 5 10 10 8 3 10 7 2 7 ...
#>  $ sex                           : Factor w/ 2 levels "Male","Female": 2 1 2 2 1 1 2 2 2 1 ...
#>  $ race                          : Factor w/ 3 levels "White","Black",..: 1 1 2 1 1 1 2 1 2 2 ...
#>  $ ethnicity                     : Factor w/ 3 levels "Mexican-American",..: 3 1 3 3 3 3 3 3 3 3 ...
#>  $ race_ethinicity               : Factor w/ 4 levels "Non-Hispanic White",..: 1 3 2 1 1 1 2 1 2 2 ...
#>  $ head_circumference_cm         : num [1:2107] 39.3 45.4 43.9 45.8 44.9 42.2 45.8 NA 40.2 44.5 ...
#>  $ recumbent_length_cm           : num [1:2107] 59.5 69.2 69.8 73.8 69 61.7 74.8 NA 64.5 70.2 ...
#>  $ first_subscapular_skinfold_mm : num [1:2107] 8.2 13 6 8 8.2 9.4 5.2 NA 7 5.9 ...
#>  $ second_subscapular_skinfold_mm: num [1:2107] 8 13 5.6 10 7.8 8.4 5.2 NA 7 5.4 ...
#>  $ first_triceps_skinfold_mm     : num [1:2107] 9 15.6 7 16.4 9.8 9.6 5.8 NA 11 6.8 ...
#>  $ second_triceps_skinfold_mm    : num [1:2107] 9.4 14 8.2 12 8.8 8.2 6.6 NA 10.9 7.6 ...
#>  $ weight_kg                     : num [1:2107] 6.35 9.45 7.15 10.7 9.35 7.15 8.35 NA 7.35 8.65 ...
#>  $ poverty_income_ratio          : num [1:2107] 3.186 1.269 0.416 2.063 1.464 ...
#>  $ smoke                         : Factor w/ 2 levels "Yes","No": 2 2 1 1 1 2 2 1 2 1 ...
#>  $ health                        : Ord.factor w/ 5 levels "Excellent"<"Very Good"<..: 1 3 1 1 1 1 1 1 2 1 ...
colSums(is.na(newborn))
#>                 household_size                     age_months 
#>                              0                              0 
#>                            sex                           race 
#>                              0                              0 
#>                      ethnicity                race_ethinicity 
#>                              0                              0 
#>          head_circumference_cm            recumbent_length_cm 
#>                            124                            114 
#>  first_subscapular_skinfold_mm second_subscapular_skinfold_mm 
#>                            161                            169 
#>      first_triceps_skinfold_mm     second_triceps_skinfold_mm 
#>                            124                            167 
#>                      weight_kg           poverty_income_ratio 
#>                            117                            192 
#>                          smoke                         health 
#>                              7                              0
```

To impute this dataset, we use the default settings. By default, the
number of imputed datasets is set to `m = 5`. The data do not need to be
converted to a `dgCMatrix` or one-hot encoded format, as these
transformations are handled automatically by the package. Supported
variable types include numeric, integer, factor, and ordinal factor.

``` r
# use mixgb with default settings
imp_list <- mixgb(data = newborn, m = 5)
```

### Customise imputation settings

We can also customise imputation settings:

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
set.seed(2026)
# Use mixgb with chosen settings
params <- list(
  max_depth = 5,
  subsample = 0.9,
  nthread = 2,
  tree_method = "hist"
)

imp_list <- mixgb(
  data = newborn, m = 10, maxit = 2,
  ordinalAsInteger = FALSE,
  pmm.type = "auto", pmm.k = 5, pmm.link = "prob",
  initial.num = "normal", initial.int = "mode", initial.fac = "mode",
  save.models = FALSE, save.vars = NULL,
  xgb.params = params, nrounds = 200, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0
)
```

### Tune hyperparameters

Imputation performance can be influenced by the choice of
hyperparameters. While tuning a large number of hyperparameters may seem
daunting, the search space can often be substantially reduced because
many of them are correlated. In mixgb, the function
[`mixgb_cv()`](../reference/mixgb_cv.md) is provided to tune the number
of boosting rounds (`nrounds`). As XGBoost does not define a default
value for `nrounds`, users must specify this parameter explicitly. The
default setting in mixgb() is `nrounds = 100`; however, we recommend
using [`mixgb_cv()`](../reference/mixgb_cv.md) to get an appropriate
value first.

``` r
params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
cv.results <- mixgb_cv(data = newborn, nrounds = 100, xgb.params = params, verbose = FALSE)
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
#>     <int>           <num>          <num>          <num>         <num>
cv.results$response
#> [1] "weight_kg"
cv.results$best.nrounds
#> [1] 16
```

By default, [`mixgb_cv()`](../reference/mixgb_cv.md) randomly selects an
incomplete variable as the response and fits an XGBoost model using the
remaining variables as predictors, based on the complete cases of the
dataset. As a result, repeated runs of
[`mixgb_cv()`](../reference/mixgb_cv.md) may yield different results.
Users may instead explicitly specify the response variable and the set
of covariates via the `response` and `select_features` arguments,
respectively.

``` r
cv.results <- mixgb_cv(
  data = newborn, nfold = 10, nrounds = 100, early_stopping_rounds = 1,
  response = "head_circumference_cm", select_features = c("age_months", "sex", "race_ethinicity", "recumbent_length_cm", "first_subscapular_skinfold_mm", "second_subscapular_skinfold_mm", "first_triceps_skinfold_mm", "second_triceps_skinfold_mm", "weight_kg"), xgb.params = params, verbose = FALSE
)

cv.results$best.nrounds
#> [1] 12
```

We can then set `nrounds = cv.results$best.nrounds` in
[`mixgb()`](../reference/mixgb.md) to generate five imputed datasets.

``` r
imp_list <- mixgb(data = newborn, m = 5, nrounds = cv.results$best.nrounds)
```

## Inspect multiply imputed values

Older version of **mixgb** package included a few visual diagnostic
functions. These have now been removed from **mixgb**.

We recommend our standalone package **vismi** ([Visualisation Tools for
Multiple Imputation](https://agnesdeng.github.io/vismi/)), which
provides a comprehensive set of visual diagnostics for evaluating
multiply imputed data.

For more details, please visit:

<https://agnesdeng.github.io/vismi/>

<https://github.com/agnesdeng/vismi>.
