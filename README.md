
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mixgb

<!-- badges: start -->

[![Made with
R](https://img.shields.io/badge/Made%20With-R-9cf)](https://github.com/agnesdeng/mixgb)
[![CRAN
version](https://img.shields.io/cran/v/mixgb?color=9cf)](https://cran.r-project.org/package=mixgb)
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/mixgb)](https://cran.r-project.org/package=mixgb)
[![GitHub release (latest by
date)](https://img.shields.io/github/v/release/agnesdeng/mixgb?color=green)](https://github.com/agnesdeng/mixgb/releases)
[![R-CMD-check](https://github.com/agnesdeng/mixgb/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/agnesdeng/mixgb/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The R package **mixgb** provides a scalable approach for multiple
imputation by leveraging XGBoost, subsampling and predictive mean
matching. We have shown that our method can yield less biased estimates
and reflect appropriate imputation variability, while achieving high
computational efficiency. For further information, please refer to our
paper [Multiple Imputation Through
XGBoost](https://yongshideng.com/journal-article/mixgb/).

## References

- Yongshi Deng & Thomas Lumley. (2023), [Multiple Imputation Through
  XGBoost](https://yongshideng.com/journal-article/mixgb/), Journal of
  Computational and Graphical Statistics, 33(2), 352-363. DOI:
  10.1080/10618600.2023.2252501.

- Tianqi Chen & Carlos Guestrin. (2016), [XGBoost: A Scalable Tree
  Boosting System](http://arxiv.org/abs/1603.02754), In 22nd SIGKDD
  Conference on Knowledge Discovery and Data Mining.

## New updates

**New Release: Nov 2024**

- New CRAN version 1.5.2. A lot faster than 1.0.2!

- Some visual diagnostic functions have been moved to the `vismi`
  package, which provides a wide range of visualisation tools for
  multiple imputation. For more details, please check the `vismi`
  package on GitHub [Visualisation Tools for Multiple
  Imputation](https://github.com/agnesdeng/vismi).

**Nov 2023**

- New version of the `mixgb()` function has been optimized to greatly
  reduce imputation time for large datasets.

- **mixgb**(\>=1.4.2) is compatible with both XGBoost(\>=2.0.0) and
  XGBoost(CRAN version).

**Oct 2023**

- Dependency Change: Starting from mixgb version 1.3.1, the package
  requires XGBoost version 2.0.0 or higher. As of now, this version is
  not available on CRAN. To get the required version with GPU support,
  please download it from [XGBoost GitHub
  Releases](https://github.com/dmlc/xgboost/releases). Then in R, one
  can install XGBoost 2.0.0 and the newest version of **mixgb** as
  follows:

``` r
# Change the file path where you saved the downloaded XGBoost package
install.packages("path_to_downloaded_file/xgboost_r_gpu_win64_2.0.0.tar.gz", repos = NULL)
```

``` r
devtools::install_github("agnesdeng/mixgb")
library(mixgb)
```

- If you prefer to use the CRAN version of XGBoost, consider using an
  earlier version of **mixgb** (versions \<1.3.1).

- Now compatible with XGBoost 2.0.0! To align with XGBoost 2.0.0,
  **mixgb** introduces new parameter `device` and removed
  parameters`gpu_id` and `predictor`. Also, `tree_method="hist"` by
  default.

  - `mixgb(device="cpu", tree_method="hist",.....)`

  - `mixgb(device="cuda", tree_method="hist",.....)`

- Now support saving imputation models in a local directory in JSON
  format.

**May 2023**

- Support logical data automatically without the need to convert it to
  factor type.

Now `mixgb(data,...)` support a dataset with the following data types:

    - numeric

    - integer

    - factor

    - logical

Please note that variables of `character` type need to be manually
converted to factor by users before imputation.

**January 2023**

- Major change of default settings for mixgb().

Our package has changed from using bootstrapping to subsampling with a
default setting of `subsample = 0.7`. This decision is based on the
discovery that although bootstrapping is generally effectively, it can
introduce bias in certain scenarios. As a result, subsampling has been
adopted as the default approach.

**May 2022**

- Introduce visual diagnostic functions for multiply imputed data.
- Use S3 instead of R6.
- Plot functions can now show masked missing data (if provided).

**April 2022**

- User can adjust the number of iterations with the `maxit` parameter.
- Both single and multiple imputation with XGBoost can use predictive
  mean matching.
- Bootstrapping data to obtain `m` imputations is optional. Users can
  set `bootstrap = FALSE` to disable bootstrap. Users can also set
  sampling-related hyperparameters of XGBoost (`subsample`,
  `colsample_bytree`, `colsample_bylevel`, `colsample_bynode`) to be
  less than 1 to achieve a similar effect.
- Add predicted mean matching type 0. Now the options for `pmm.type` are
  `NULL`,`0`,`1`,`2` or `"auto"` (type 2 for numeric/integer variables,
  no PMM for categorical variables).
- Add more validation checks.
- Compatible with `data.table`.
- Add function `mixgb_cv()` to pre-tune `nrounds` by cross-validation.

## Under development

- In progress: To Be Announced
- Planned: To Be Announced
- Under consideration: To implement PMM type 3

## Notice

- For multithreading, users can set the XGBoost nthread parameter with
  OpenMP support. Be advised, OpenMP support is currently disabled on
  MacOS.

## 1. Installation

You can install the development version of mixgb from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("agnesdeng/mixgb")
```

``` r
# load mixgb
library(mixgb)
```

### 1.1 Data cleaning before imputation

It is highly recommended to clean and check your data before imputation.
Here are some common issues:

- Data should be a data frame.
- ID should be removed
- Missing values should be coded as `NA` not `NaN`
- `Inf` or `-Inf` are not allowed
- Empty cells should be coded as `NA` or sensible values
- Variables of “character” type should be converted to “factor” instead
- Variables of “factor” type should have at least two levels

The function `data_clean()` serves the purpose of performing a
preliminary check and fix some evident issues. However, the function
cannot resolve all data quality-related problems.

``` r
cleanWithNA.df <- data_clean(rawdata)
```

## 2. Impute missing values with `mixgb`

We first load the `mixgb` package and the `nhanes3_newborn` dataset,
which contains 16 variables of various types
(integer/numeric/factor/ordinal factor). There are 9 variables with
missing values.

``` r
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

### 2.1 Customize imputation settings

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

- Whether or not to use bootstrapping `bootstrap`

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
  ordinalAsInteger = FALSE, bootstrap = FALSE,
  pmm.type = "auto", pmm.k = 5, pmm.link = "prob",
  initial.num = "normal", initial.int = "mode", initial.fac = "mode",
  save.models = FALSE, save.vars = NULL,
  xgb.params = params, nrounds = 200, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0
)
```

### 2.2 Tune hyperparameters

Imputation performance can be affected by the hyperparameter settings.
Although tuning a large set of hyperparameters may appear intimidating,
it is often possible to narrowing down the search space because many
hyperparameters are correlated. In our package, the function
`mixgb_cv()` can be used to tune the number of boosting rounds -
`nrounds`. There is no default `nrounds` value in `XGBoost,` so users
are required to specify this value themselves. The default `nrounds` in
`mixgb()` is 100. However, we recommend using `mixgb_cv()` to find the
optimal `nrounds` first.

``` r
params <- list(max_depth = 3, subsample = 0.7, nthread = 2)
cv.results <- mixgb_cv(data = nhanes3_newborn, nrounds = 100, xgb.params = params, verbose = FALSE)
cv.results$response
#> [1] "BMPSB2"
cv.results$best.nrounds
#> [1] 16
```

By default, `mixgb_cv()` will randomly choose an incomplete variable as
the response and build an XGBoost model with other variables as
explanatory variables using the complete cases of the dataset.
Therefore, each run of `mixgb_cv()` will likely return different
results. Users can also specify the response and covariates in the
argument `response` and `select_features` respectively.

``` r
cv.results <- mixgb_cv(
  data = nhanes3_newborn, nfold = 10, nrounds = 100, early_stopping_rounds = 1,
  response = "BMPHEAD", select_features = c("HSAGEIR", "HSSEX", "DMARETHN", "BMPRECUM", "BMPSB1", "BMPSB2", "BMPTR1", "BMPTR2", "BMPWT"), xgb.params = params, verbose = FALSE
)

cv.results$best.nrounds
#> [1] 18
```

Let us just try setting `nrounds = cv.results$best.nrounds` in `mixgb()`
to obtain 5 imputed datasets.

``` r
imputed.data <- mixgb(data = nhanes3_newborn, m = 5, nrounds = cv.results$best.nrounds)
```

## 3. Visualize multiply imputed values

It is crucial to assess the plausibility of imputations before doing an
analysis.

The `mixgb` package used to provide a few visual diagnostics functions.
However, we have moved these functions to the `vismi` package, which
provides a wide range of visualisation tools for multiple imputation.

For more details, please check the `vismi` package on GitHub
[Visualisation Tools for Multiple
Imputation](https://github.com/agnesdeng/vismi).

## 4. Impute new unseen data using a saved imputer object

To demonstrate how to impute new data using a saved imputer, we first
split the `nhanes3_newborn` dataset into training data and test data.

``` r
set.seed(2022)
n <- nrow(nhanes3)
idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
train.data <- nhanes3[idx, ]
test.data <- nhanes3[-idx, ]
```

Next we impute the training data using `mixgb()`. We can use the
training data to generate `m` imputed datasets and save their imputation
models. To achieve this, users need to set `save.models = TRUE`. By
default, imputation models for all variables with missing values in the
training data will be saved (`save.vars = NULL`). However, it is
possible that unseen data may have missing values in other variables. To
be thorough, users can save models for all variables by setting
`save.vars = colnames(train.data)`. Note that this may take
significantly longer as it requires training and saving a model for each
variable. In cases where users are confident that only certain variables
will have missing values in the new data, it is advisable to specify the
names or indices of these variables in `save.vars` rather than saving
models for all variables.

To save the imputer object, users need to specify a local directory in
the parameter `save.models.folder` in the main function `mixgb()`.
Models will be save as JSON format by calling `xgb.save()` internally.
Saving XGBoost models in this way instead of using `saveRDS` in R is
recommended by XGBoost. This can ensure that the imputation models can
still be used in later release of XGBoost.

``` r
# obtain m imputed datasets for train.data and save imputation models
mixgb.obj <- mixgb(data = train.data, m = 5, save.models = TRUE, save.models.folder = "C:/Users/.....")
saveRDS(object = mixgb.obj, file = "C:/Users/.../mixgbimputer.rds")
```

If users specify the `save.models.folder`, the return object will
include the following:

- `imputed.data`: a list of `m` imputed datasets for training data

- `XGB.models`: a list of directories of `m` sets of XGBoost models for
  variables specified in `save.vars`.

- `params`: a list of parameters that are required for imputing new data
  using `impute_new()` later on.

- `XGB.save` : a parameter indicates whether `XGB.models` are the saved
  models or the directories for the saved models.

As the `mixgb.obj` does not contain the models themselves, users need
not worry about saving this object via `saveRDS()`. For later use, one
can load the object into R and impute new data.

To impute new data with this saved imputer object, we can use the
`impute_new()` function.

``` r
mixgb.obj <- readRDS(file = "C:/Users/.../mixgbimputer.rds")
test.imputed <- impute_new(object = mixgb.obj, newdata = test.data)
```

Users can choose whether to use new data for initial imputation. By
default, the information of training data is used to initially impute
the missing data in the new dataset (`initial.newdata = FALSE`). After
this, the missing values in the new dataset will be imputed using the
saved models from the imputer object. This process will be considerably
faster because it does not involve rebuilding the imputation models.

``` r
test.imputed <- impute_new(object = mixgb.obj, newdata = test.data)
```

If PMM is used in `mixgb()`, predicted values of missing entries in the
new dataset will be matched with donors from the training data.
Additionally, users can set the number of donors to be used in PMM when
imputing new data. The default setting `pmm.k = NULL` indicates that the
same setting as the training object will be used.

Similarly, users can set the number of imputed datasets `m` in
`impute_new()`. Note that this value has to be less than or equal to the
`m` value specified in `mixgb()`. If this value is not specified, the
function will use the same `m` value as the saved object.

``` r
test.imputed <- impute_new(object = mixgb.obj, newdata = test.data, initial.newdata = FALSE, pmm.k = 3, m = 4)
```

## 5. Install `mixgb` with GPU support

Multiple imputation can be run with GPU support for machines with NVIDIA
GPUs. Users must first install the R package `xgboost` with GPU support.

### Newest Version (XGBoost \>= 2.0.0, mixgb \>= 1.3.1)

Please download the Newest version of XGBoost with GPU support via
[XGBoost GitHub Releases](https://github.com/dmlc/xgboost/releases).

``` r
# Change the file path where you saved the downloaded XGBoost package
install.packages("path_to_downloaded_file/xgboost_r_gpu_win64_2.0.0.tar.gz", repos = NULL)
```

Then users can install the newest version of our package `mixgb` in R.

``` r
devtools::install_github("agnesdeng/mixgb")
library(mixgb)
```

To utilize the GPU version of mixgb(), users can simply specify
`device = "cuda"` in the params list which will then be passed to the
`xgb.params` argument in the function `mixgb()`. Note that by default,
`tree_method = "hist"` from XGBoost 2.0.0.

``` r
params <- list(
  device = "cuda",
  subsample = 0.7,
  nthread = 1,
  tree_method = "hist"
)

mixgb.data <- mixgb(data = withNA.df, m = 5, xgb.params = params)
```

### Old Version (XGBoost \< 2.0.0, mixgb \< 1.3.1)

The `xgboost` R package pre-built binary on Linux x86_64 with GPU
support can be downloaded from the release page
<https://github.com/dmlc/xgboost/releases/tag/v1.4.0>

The package can then be installed by running the following commands:

    # Install dependencies
    $ R -q -e "install.packages(c('data.table', 'jsonlite'))"

    # Install XGBoost
    $ R CMD INSTALL ./xgboost_r_gpu_linux.tar.gz

Then users can install package `mixgb` in R.

``` r
devtools::install_github("agnesdeng/mixgb")
library(mixgb)
```

To utilize the GPU version of mixgb(), users can simply specify
`tree_method = "gpu_hist"` in the params list which will then be passed
to the `xgb.params` argument in the function `mixgb()`. Other adjustable
GPU-related arguments include `gpu_id` and `predictor`. By default,
`gpu_id = 0` and `predictor = "auto"`.

``` r
params <- list(
  max_depth = 3,
  subsample = 0.7,
  nthread = 1,
  tree_method = "gpu_hist",
  gpu_id = 0,
  predictor = "auto"
)


mixgb.data <- mixgb(data = withNA.df, m = 5, xgb.params = params)
```
