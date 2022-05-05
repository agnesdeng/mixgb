[![](https://img.shields.io/badge/Made%20With-R-9cf)](https://github.com/agnesdeng/misle)
[![](https://img.shields.io/badge/Version-1.0.0-brightgreen)](https://github.com/agnesdeng/misle)
[![](https://img.shields.io/badge/Lifecycle-Experimental-ff69b4)](https://github.com/agnesdeng/misle)

# mixgb
mixgb: multiple imputation through XGBoost

Mixgb is a scalable multiple imputation framework based on XGBoost, bootstrapping and predictive mean matching. The proposed framework is implemented in an R package `mixgb`. We have shown that our framework obtains less biased estimates and reflects appropriate imputation variability, while achieving high computational efficiency. For more details, please check our paper https://arxiv.org/abs/2106.01574


## Install `mixgb` 
If users only want to use multiple imputation through XGBoost, please install this simplified R package `mixgb`.
```r
devtools::install_github("agnesdeng/mixgb")
library(mixgb)
```

## Data cleaning before feeding in the imputer

It is highly recommended to clean and check your data before feeding in the imputer. Here are some common issues:

- Data should be a data frame.
- ID should be removed 
- Missing values should be coded as NA not NaN
- Inf or -Inf are not allowed
- Empty cells should be coded as NA or sensible values
- Variables of "character" type should be converted to "factor" instead
- Variables of "factor" type should have at least two levels

The function `data_clean()` can do a preliminary check and fix some obvious problems. However, it would not fix all issues related to data quality. 

```
cleanWithNA.df<-data_clean(rawdata=rawWithNA.df)
```

## Example: multiple imputation through XGBoost

We first load the NHANES dataset from the R package "hexbin". We can see that there are seven variables in this dataset has missing values.
``` r
library(hexbin)
data("NHANES")
colSums(is.na(NHANES))
```

Create an Mixgb imputer with your choice of settings or leave it as default.

Note that users do not need to convert the data frame into dgCMatrix or one-hot coding themselves. Ths imputer will convert it automatically for you. The type of variables should be one of the following: numeric, integer, or factor (binary/multiclass).

``` r
MIXGB <- Mixgb$new(data = NHANES, pmm.type = "auto", pmm.k = 5)
imputed.data <- MIXGB$impute(m = 5, maxit = 1)
```
Use this imputer to obtain m imputed datasets. Users can also specify the number of iterations `maxit`.

A list of hyperparameters of xgboost can be passed to xgboost,The default values are as follows.

``` r

params = list(max_depth = 6, gamma = 0.1, eta = 0.3, min_child_weight = 1, 
                  subsample = 1, colsample_bytree = 1, colsample_bylevel = 1, colsample_bynode, 
                  tree_method = "auto", gpu_id = 0, predictor = "auto")


MIXGB <- Mixgb$new(
  data = NHANES, xgb.params = params,
  nrounds = 50, early_stopping_rounds = 10, print_every_n = 10L, verbose = 0,
  pmm.k = 5, pmm.type = "auto", pmm.link = "prob",
  initial.num = "normal", initial.fac = "mode", bootstrap = TRUE
)

imputed.data<-MIXGB$impute(m=5,maxit=5)
```



## Example: impute new unseen data
First we can split a dataset as training data and test data.
``` r
set.seed(2022)
n <- nrow(NHANES)
idx <- sample(1:n, size = round(0.7 * n), replace = FALSE)
NHANES.train <- NHANES[idx, ]
NHANES.test <- NHANES[-idx, ]
```

We can use the training data (with missing values) to obtain m imputed datasets and their imputation models. User need to set `save.models = TRUE`. By default `save.vars = NULL`, imputation models for variables with missing data in the training data will be saved in the object `mixgb.obj`. This save time, however, the unseen data may also have missing values in other variable, it would be comprehensive to save models for all variables. In this case, just set `save.vars = colnames(NHANES.train)`. This would take way longer time and space for large datasets. If users are confident that only certain variables of future data will have missing values, we recommend to specify the names or indices of these variables in `save.vars` instead of saving all variables to speed up the process.

``` r
MIXGB <- Mixgb$new(data = NHANES.train)
mixgb.obj <- MIXGB$impute(m = 5, maxit = 1, save.models = TRUE, save.vars = NULL)
```
To obtain the m imputed datasets, simply use `$imputed.data`. 
```r
imputed.traindata <- mixgb.obj$imputed.data
head(imputed.traindata[[1]])
```
To use this saved imputer object to impute new data, we use the `impute_new()` function. User can specify whether to use new data to do initial imputation. By default, this is set to be FALSE, because the size of new data may be too small (e.g, only one observation).

``` r
imputed.testdata <- impute_new(object = mixgb.obj, newdata = NHANES.test, initial.newdata = FALSE)
```
If PMM is use in the training models, predicted values of missing entries in the new dataset are matched with training data. Users can also set the number of donors for PMM when impute the new dataset. By default, `pmm.k = NULL `, which means using the same setting as the training object. 

Similarly, users can set the number of imputed datasets `m`.  Note that this value has to be smaller than or equal to the one set in the training object. If it is not specified, it will use the same `m` value as the training object.

``` r
imputed.testdata <- impute_new(object = mixgb.obj, newdata = NHANES.test, initial.newdata = FALSE,pmm.k = NULL, m = NULL)
imputed.testdata <- impute_new(object = mixgb.obj, newdata = NHANES.test, initial.newdata = FALSE,pmm.k = 3, m = 4)
```


## Install `mixgb` with GPU support
Multiple imputation can be run with GPU support for machines with NVIDIA GPUs. Note that users have to install the R package `xgboost` with GPU support first. 

The xgboost R package pre-built binary on Linux x86_64 with GPU supportcan be downloaded from the release page https://github.com/dmlc/xgboost/releases/tag/v1.4.0

The package can then be installed by running the following commands:
``` 
# Install dependencies
$ R -q -e "install.packages(c('data.table', 'jsonlite'))"

# Install XGBoost
$ R CMD INSTALL ./xgboost_r_gpu_linux.tar.gz
```

Then users can install package `mixgb` in R. 
```r
devtools::install_github("agnesdeng/mixgb")
library(mixgb)
```
Users just need to add the argument ` tree_method = "gpu_list"` in the Mixgb imputer. Other GPU-realted arguments include  `gpu_id`  and  `predictor`. By default, `gpu_id = 0` and `predictor = "auto"`.

``` r
MIXGB<-Mixgb$new(withNA.df,tree_method='gpu_list')

mixgb.data<-MIXGB$impute(m=5)
```
