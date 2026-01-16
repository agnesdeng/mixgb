# Multiple imputation through XGBoost

This function is used to generate multiply-imputed datasets using
XGBoost, subsampling and predictive mean matching (PMM).

## Usage

``` r
mixgb(
  data,
  m = 5,
  maxit = 1,
  ordinalAsInteger = FALSE,
  pmm.type = NULL,
  pmm.k = 5,
  pmm.link = "prob",
  initial.num = "normal",
  initial.int = "mode",
  initial.fac = "mode",
  save.models = FALSE,
  save.vars = NULL,
  save.models.folder = NULL,
  verbose = F,
  xgb.params = list(),
  nrounds = 100,
  early_stopping_rounds = NULL,
  print_every_n = 10L,
  xgboost_verbose = 0,
  ...
)
```

## Arguments

- data:

  A data.frame or data.table with missing values

- m:

  The number of imputed datasets. Default: 5

- maxit:

  The number of imputation iterations. Default: 1

- ordinalAsInteger:

  Whether to convert ordinal factors to integers. By default,
  `ordinalAsInteger = FALSE`. Setting `ordinalAsInteger = TRUE` may
  speed up the imputation process for large datasets.

- pmm.type:

  The type of predictive mean matching (PMM). Possible values:

  - `NULL` (default): Imputations without PMM;

  - `0`: Imputations with PMM type 0;

  - `1`: Imputations with PMM type 1;

  - `2`: Imputations with PMM type 2;

  - `"auto"`: Imputations with PMM type 2 for numeric/integer variables;
    imputations without PMM for categorical variables.

- pmm.k:

  The number of donors for predictive mean matching. Default: 5

- pmm.link:

  The link for predictive mean matching in binary variables

  - `"prob"` (default): use probabilities;

  - `"logit"`: use logit values.

- initial.num:

  Initial imputation method for numeric type data:

  - `"normal"` (default);

  - `"mean"`;

  - `"median"`;

  - `"mode"`;

  - `"sample"`.

- initial.int:

  Initial imputation method for integer type data:

  - `"mode"` (default);

  - `"sample"`.

- initial.fac:

  Initial imputation method for factor type data:

  - `"mode"` (default);

  - `"sample"`.

- save.models:

  Whether to save imputation models for imputing new data later on.
  Default: `FALSE`

- save.vars:

  For the purpose of imputing new data, the imputation models for
  response variables specified in `save.vars` will be saved. The values
  in `save.vars` can be a vector of names or indices. By default, only
  the imputation models for variables with missing values in the
  original data will be saved (`save.vars = NULL`). To save imputation
  models for all variables, users can specify
  `save.vars = colnames(data)`.

- save.models.folder:

  Users can specify a directory to save all imputation models. Models
  will be saved in JSON format by internally calling `xgb.save()`, which
  is recommended by XGBoost.

- verbose:

  Verbose setting for mixgb. If `TRUE`, will print out the progress of
  imputation. Default: `FALSE`.

- xgb.params:

  A list of XGBoost parameters. For more details, please check [XGBoost
  documentation on
  parameters](https://xgboost.readthedocs.io/en/stable/parameter.html).

- nrounds:

  The maximum number of boosting iterations for XGBoost. Default: 100

- early_stopping_rounds:

  An integer value `k`. XGBoost training will stop if the validation
  performance has not improved for `k` rounds. Default: 10.

- print_every_n:

  Print XGBoost evaluation information at every nth iteration if
  `xgboost_verbose > 0`.

- xgboost_verbose:

  Verbose setting for XGBoost training: 0 (silent), 1 (print
  information) and 2 (print additional information). Default: 0

- ...:

  Extra arguments to be passed to XGBoost

## Value

If `save.models = FALSE`, this function will return a list of `m`
imputed datasets. If `save.models = TRUE`, it will return an object with
imputed datasets, saved models and parameters.

## Examples

``` r
# obtain m multiply datasets without saving models
params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
mixgb.data <- mixgb(data = nhanes3, m = 2, xgb.params = params, nrounds = 10)

# obtain m multiply imputed datasets and save models for imputing new data later on
mixgb.obj <- mixgb(
  data = nhanes3, m = 2, xgb.params = params, nrounds = 10,
  save.models = TRUE, save.models.folder = tempdir()
)
```
