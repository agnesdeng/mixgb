# Imputing newdata with a saved mixgb imputer

## Impute new unseen data using a saved imputer object

To demonstrate how to impute new data using a saved imputer, we first
split the `newborn` dataset into training data and test data.

``` r
set.seed(2026)
n <- nrow(newborn)
idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
train_data <- newborn[idx, ]
test_data <- newborn[-idx, ]
```

Next, we impute the training data using
[`mixgb()`](../reference/mixgb.md). This allows us to generate `m`
imputed datasets and save the corresponding imputation models by setting
`save.models = TRUE`. By default, models for all variables with missing
values in the training data are saved (`save.vars = NULL`).

If the new or unseen data may contain missing values in additional
variables, users can save models for all variables by setting
`save.vars = colnames(train_data)`. This can be time-consuming, as a
model needs to be trained and saved for each variable. When users are
confident that only certain variables will have missing values in future
data, it is more efficient to specify only those variables in
`save.vars`.

To save the imputer object, specify a local directory using the
`save.models.folder` argument in [`mixgb()`](../reference/mixgb.md). In
the backend, models are saved in JSON format via `xgb.save()`. This
approach, recommended by XGBoost, is preferred over directly saving the
XGBoost models with [`saveRDS()`](https://rdrr.io/r/base/readRDS.html)
because it ensures compatibility with future XGBoost releases.

``` r
# create a temporary folder
tmp_folder <- file.path(tempdir(), "mixgb_models")
dir.create(tmp_folder, showWarnings = FALSE)

# obtain m imputed datasets for train_data and save imputation models
mixgb_obj <- mixgb(
  data = train_data, m = 5,
  save.models = TRUE, save.models.folder = tmp_folder
)

saveRDS(
  object = mixgb_obj,
  file = file.path(tmp_folder, "mixgb_imputer.rds")
)
```

If users specify the `save.models.folder`, the return object will
include the following:

- `imputed_data`: a list of `m` imputed datasets for training data

- `XGB.models`: a list of directories of `m` sets of XGBoost models for
  variables specified in `save.vars`.

- `params`: a list of parameters that are required for imputing new data
  using [`impute_new()`](../reference/impute_new.md) later on.

- `XGB.save` : a parameter indicates whether `XGB.models` are the saved
  models or the directories for the saved models.

As the `mixgb_obj` does not contain the models themselves, users need
not worry about saving this object via
[`saveRDS()`](https://rdrr.io/r/base/readRDS.html). For later use, one
can load the object into R and impute new data.

To impute new data with this saved imputer object, we can use the
[`impute_new()`](../reference/impute_new.md) function.

``` r
mixgb_obj <- readRDS(file = file.path(tmp_folder, "mixgb_imputer.rds"))
test_imp <- impute_new(object = mixgb_obj, newdata = test_data)
```

Users can decide whether to perform initial imputation on new data. By
default, the training data is used to impute missing values in the new
dataset initially (`initial.newdata = FALSE`). After this step, the
missing values in the new data are imputed using the saved models from
the imputer object. This approach is considerably faster, as it avoids
retraining the imputation models.

``` r
test_imp <- impute_new(object = mixgb_obj, newdata = test_data)
```

When predictive mean matching (PMM) is used in
[`mixgb()`](../reference/mixgb.md), predicted values of missing entries
in the new dataset will be matched with donors from the training data.
Users can also specify the number of donors for PMM. The default setting
`pmm.k = NULL` indicates that the `pmm.k` value from the training object
will be inherited.

Similarly, the number of imputed datasets for the new data can be set
via `m` in [`impute_new()`](../reference/impute_new.md). This value must
be less than or equal to the `m` used in
[`mixgb()`](../reference/mixgb.md). If not specified,
[`impute_new()`](../reference/impute_new.md) will use the `m` value
stored in the saved imputer object.

``` r
test_imp <- impute_new(
  object = mixgb_obj, newdata = test_data,
  initial.newdata = FALSE, pmm.k = 3, m = 4
)
```
