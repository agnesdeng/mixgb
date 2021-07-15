[![](https://img.shields.io/badge/Made%20With-R-9cf)](https://github.com/agnesdeng/misle)
[![](https://img.shields.io/badge/Version-1.0.0-brightgreen)](https://github.com/agnesdeng/misle)
[![](https://img.shields.io/badge/Lifecycle-Experimental-ff69b4)](https://github.com/agnesdeng/misle)

# mixgb
mixgb: multiple imputation through XGBoost

Mixgb is a scalable multiple imputation framework based on XGBoost, bootstrapping and predictive mean matching. The proposed framework is implemented in an R package `mixgb`. We have shown that our framework obtains less biased estimates and reflects appropriate imputation variability, while achieving high computational efficiency. For more details, please check our paper https://arxiv.org/abs/2106.01574


## Install `mixgb` 
If users only want to use multiple imputation through XGBoost, please install this simplified R package `mixgb` instead.
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

