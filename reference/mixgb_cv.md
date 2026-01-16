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
#> [1]  train-rmse:1.963320±0.031171    test-rmse:2.021989±0.166801 
#> [2]  train-rmse:1.635703±0.036784    test-rmse:1.722371±0.175585 
#> [3]  train-rmse:1.425266±0.040358    test-rmse:1.541440±0.170586 
#> [4]  train-rmse:1.296524±0.040932    test-rmse:1.441356±0.174966 
#> [5]  train-rmse:1.211539±0.039615    test-rmse:1.371976±0.163681 
#> [6]  train-rmse:1.158323±0.039793    test-rmse:1.335619±0.149652 
#> [7]  train-rmse:1.119461±0.041829    test-rmse:1.318944±0.151686 
#> [8]  train-rmse:1.094165±0.038712    test-rmse:1.301223±0.155397 
#> [9]  train-rmse:1.074211±0.041667    test-rmse:1.289382±0.160836 
#> [10] train-rmse:1.057512±0.040667    test-rmse:1.287639±0.176059 
#> [11] train-rmse:1.044720±0.039432    test-rmse:1.286882±0.175661 
#> [12] train-rmse:1.033794±0.039727    test-rmse:1.282698±0.181149 
#> [13] train-rmse:1.022246±0.039490    test-rmse:1.276626±0.185726 
#> [14] train-rmse:1.007901±0.035897    test-rmse:1.272276±0.179694 
#> [15] train-rmse:1.000166±0.035963    test-rmse:1.271480±0.179027 
#> [16] train-rmse:0.992511±0.037126    test-rmse:1.269869±0.180173 
#> [17] train-rmse:0.985283±0.041051    test-rmse:1.272495±0.187991 
#> [18] train-rmse:0.976042±0.036297    test-rmse:1.269337±0.182042 
#> [19] train-rmse:0.968928±0.033149    test-rmse:1.270250±0.181051 
#> [20] train-rmse:0.960499±0.031753    test-rmse:1.273157±0.179921 
#> [21] train-rmse:0.954404±0.033598    test-rmse:1.268842±0.178302 
#> [22] train-rmse:0.948135±0.034911    test-rmse:1.271862±0.177748 
#> [23] train-rmse:0.939070±0.030973    test-rmse:1.271855±0.179913 
#> [24] train-rmse:0.932000±0.033710    test-rmse:1.276054±0.172815 
#> [25] train-rmse:0.925331±0.032511    test-rmse:1.280610±0.178959 
#> [26] train-rmse:0.916091±0.031723    test-rmse:1.280791±0.177164 
#> [27] train-rmse:0.910110±0.032802    test-rmse:1.287943±0.174522 
#> [28] train-rmse:0.903795±0.032132    test-rmse:1.292798±0.175057 
#> [29] train-rmse:0.898237±0.033322    test-rmse:1.298807±0.175802 
#> [30] train-rmse:0.892281±0.034050    test-rmse:1.296191±0.181797 
#> Stopping. Best iteration:
#> [31] train-rmse:0.883372±0.036310    test-rmse:1.303166±0.178872
#> 
#> [31] train-rmse:0.883372±0.036310    test-rmse:1.303166±0.178872 
cv.results$best.nrounds
#> [1] 21

imputed.data <- mixgb(data = nhanes3, m = 3, xgb.params = params,
                      nrounds = cv.results$best.nrounds)
```
