# mixgb 1.0.1
  * Current default settings for mixgb().
    -  Subsampling (`subsample = 0.7`) 
    -  No bootstrapping (`bootstrap = FALSE`)
  
# mixgb 1.0.0.9000
 * Major change of default settings for mixgb(). 
  
   Instead of using bootstrapping, we have changed to use subsampling with
`subsample = 0.7` by default. After more investigations, we found that
even though bootstrapping perform well in general settings, it did add
bias for under some scenarios. We now use subsampling instead of
bootstrapping as our default setting.

# mixgb 0.1.1
* Minor bug fix for the function createNA()
* Change of default settings for mixgb()
  - ordinalAsInteger (from `TRUE` to `FALSE`)
  - max_depth (from 6 to 3)
  - nrounds (from 50 to 100)
  - `bootstrap = TRUE` by default. (Edited description)


# mixgb 0.1.0
## Main feature
* First release on CRAN
* Support single and multiple imputation
* Customisable settings regarding bootstrapping and predictive matching
* Visual diagnostics for multiply imputed data

## Notes
* Mac OSX users may not be able to use `mixgb` with multithreading. The package `xgboost` requires OpenMP to use multi-core. For more information please check https://mac.r-project.org/openmp/.
