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

## Example: multiple imputation through XGBoost

We first load the NHANES dataset from the R package "hexbin".
``` r
library(hexbin)
data("NHANES")
```

Create 30% MCAR missing data.
``` r
withNA.df<-createNA(NHANES,p=0.3)
```

Create an Mixgb imputer with your choice of settings or leave it as default.
``` r
MIXGB<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
```

Use this imputer to obtain m imputed datasets.
``` r
mixgb.data<-MIXGB$impute(m=5)
``` 

## Example: impute new unseen data
First we can split a dataset as training data and test data.
``` r
set.seed(2021)
n=nrow(iris)
idx=sample(1:n, size = round(0.7*n), replace=FALSE)

train.df=iris[idx,]
test.df=iris[-idx,]
```

Since the original data doesn't have any missing value, we create some.
``` r
trainNA.df=createNA(train.df,p=0.3)
testNA.df=createNA(test.df,p=0.3)
```

We can use the training data (with missing values) to obtain m imputed datasets. Imputed datasets, the models used in training processes and some parameters are saved in the object `mixgb.obj`.

``` r
MIXGB=Mixgb.train$new(trainNA.df)
mixgb.obj=MIXGB$impute(m=5)
```
We can now use this object to impute new unseen data by using the function `impute.new( )`.  If PMM is applied, predicted values of missing entries in the new dataset are matched with training data by default. Users can choose to match with the new dataset instead by setting `pmm.new = TRUE`.

``` r
test.impute=impute.new(object = mixgb.obj, newdata = testNA.df)
test.impute
```

``` r
test.impute=impute.new(object = mixgb.obj, newdata = testNA.df, pmm.new = TRUE)
test.impute
```
Users can also set the number of donors for PMM when impute the new dataset. If  `pmm.k` is not set here, it will use the saved parameter value from the training object  `mixgb.obj`.

``` r
test.impute=impute.new(object = mixgb.obj, newdata = testNA.df, pmm.new = TRUE, pmm.k=3)
test.impute
```

Similarly, users can set the number of imputed datasets `m`.  Note that this value has to be smaller than the one set in the training object. If it is not specified, it will use the same `m` value as the training object.

``` r
test.impute=impute.new(object = mixgb.obj, newdata = testNA.df, pmm.new = TRUE, m=4)
test.impute
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
