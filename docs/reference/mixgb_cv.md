# Use cross-validation to find the optimal `nrounds`

Use cross-validation to find the optimal `nrounds` for an `Mixgb`
imputer. Note that this method relies on the complete cases of a dataset
to obtain the optimal `nrounds`.

## Usage

``` r
mixgb_cv(
  data,
  nfold = 5,
  nrounds = 100,
  early_stopping_rounds = 10,
  response = NULL,
  select_features = NULL,
  xgb.params = list(),
  stringsAsFactors = FALSE,
  verbose = TRUE,
  ...
)
```

## Arguments

- data:

  A data.frame or a data.table with missing values.

- nfold:

  The number of subsamples which are randomly partitioned and of equal
  size. Default: 5

- nrounds:

  The max number of iterations in XGBoost training. Default: 100

- early_stopping_rounds:

  An integer value `k`. Training will stop if the validation performance
  has not improved for `k` rounds.

- response:

  The name or the column index of a response variable. Default: `NULL`
  (Randomly select an incomplete variable).

- select_features:

  The names or the indices of selected features. Default: `NULL` (Select
  all the other variables in the dataset).

- xgb.params:

  A list of XGBoost parameters. For more details, please check [XGBoost
  documentation on
  parameters](https://xgboost.readthedocs.io/en/stable/parameter.html).

- stringsAsFactors:

  A logical value indicating whether all character vectors in the
  dataset should be converted to factors.

- verbose:

  A logical value. Whether to print out cross-validation results during
  the process.

- ...:

  Extra arguments to be passed to XGBoost.

## Value

A list of the optimal `nrounds`, `evaluation.log` and the chosen
`response`.

## Examples

``` r
params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
cv.results <- mixgb_cv(data = nhanes3, xgb.params = params)
#> Multiple eval metrics are present. Will use test_rmse for early stopping.
#> Will train until test_rmse hasn't improved in 10 rounds.
#> 
#> [1]  train-rmse:1.325541±0.054794    test-rmse:1.328004±0.200554 
#> [2]  train-rmse:1.120618±0.057377    test-rmse:1.120987±0.228403 
#> [3]  train-rmse:0.977183±0.060819    test-rmse:1.008580±0.243726 
#> [4]  train-rmse:0.890541±0.060028    test-rmse:0.943404±0.259013 
#> [5]  train-rmse:0.838193±0.062854    test-rmse:0.900796±0.266055 
#> [6]  train-rmse:0.790665±0.062452    test-rmse:0.871239±0.274423 
#> [7]  train-rmse:0.759332±0.064733    test-rmse:0.857790±0.279167 
#> [8]  train-rmse:0.741639±0.063543    test-rmse:0.849731±0.279353 
#> [9]  train-rmse:0.730104±0.063602    test-rmse:0.850516±0.277500 
#> [10] train-rmse:0.702745±0.048944    test-rmse:0.858593±0.274182 
#> [11] train-rmse:0.691603±0.050012    test-rmse:0.860244±0.273899 
#> [12] train-rmse:0.687875±0.048874    test-rmse:0.859973±0.274179 
#> [13] train-rmse:0.681551±0.050469    test-rmse:0.863008±0.274802 
#> [14] train-rmse:0.664586±0.041199    test-rmse:0.863226±0.276052 
#> [15] train-rmse:0.660445±0.041594    test-rmse:0.862346±0.276682 
#> [16] train-rmse:0.656897±0.043685    test-rmse:0.862776±0.276146 
#> [17] train-rmse:0.649109±0.040902    test-rmse:0.861455±0.272649 
#> Stopping. Best iteration:
#> [18] train-rmse:0.638577±0.033485    test-rmse:0.873910±0.267153
#> 
#> [18] train-rmse:0.638577±0.033485    test-rmse:0.873910±0.267153 
cv.results$best.nrounds
#> [1] 8

imputed.data <- mixgb(data = nhanes3, m = 3, xgb.params = params,
                      nrounds = cv.results$best.nrounds)
```
