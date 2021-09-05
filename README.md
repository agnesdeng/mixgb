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

Note that users do not need to convert the data frame into dgCMatrix or one-hot coding themselves. Ths imputer will convert it automatically for you. The type of variables should be one of the following: numeric, integer, or factor (binary/multiclass).

``` r
MIXGB<-Mixgb$new(withNA.df,pmm.type="auto",pmm.k = 5)
```

Use this imputer to obtain m imputed datasets.
``` r
mixgb.data<-MIXGB$impute(m=5)
``` 

Users can change the values for hyperparameters in an imputer. The default values are as follows.

``` r
MIXGB<-Mixgb$new(data=.., nrounds=50,max_depth=6,gamma=0.1,eta=0.3,nthread=4,early_stopping_rounds=10,colsample_bytree=1,min_child_weight=1,subsample=1,pmm.k=5,pmm.type="auto",pmm.link="logit",scale_pos_weight=1,initial.imp="random",tree_method="auto",gpu_id=0,predictor="auto",print_every_n = 10L,verbose=0)
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
trainNA.df=createNA(data=train.df,p=0.3)
testNA.df=createNA(data=test.df,p=0.3)
```

We can use the training data (with missing values) to obtain m imputed datasets. Imputed datasets, the models used in training processes and some parameters are saved in the object `mixgb.obj`.

``` r
MIXGB=Mixgb.train$new(data=trainNA.df)
mixgb.obj=MIXGB$impute(m=5)
```

By default, an ensemble of imputation models for all variables in the training dataset will be saved in the object  `mixgb.obj`. This is convenient when we do not know which variables of the future unseen data have missing values. However, this process would take longer time and space.

If users are confident that only certain variables of future data will have missing values, they can choose to specify these variables to speed up the process. Users can either use the indices or the names of the variables in the argument `save.vars`. Models for variables with missing values in the training data and those specified in `save.vars` will be saved.

``` r
MIXGB=Mixgb.train$new(data=trainNA.df)
mixgb.obj=MIXGB$impute(m=5,save.vars=c(1,3,5))

#alternatively, specify the names of variables
mixgb.obj=MIXGB$impute(m=5,save.vars=c("Sepal.Length","Petal.Length","Species"))
```

We can now use the saved imputer object to impute new unseen data by using the function `impute.new( )`.  If PMM is applied, predicted values of missing entries in the new dataset are matched with training data by default.

``` r
test.impute=impute.new(object = mixgb.obj, newdata = testNA.df)
test.impute
```
Users can choose to match with the new dataset instead by setting `pmm.new = TRUE`.

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
