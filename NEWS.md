# mixgb 1.3.1
*  Now compatible with XGBoost 2.0.0 
-  added new parameter `device` 
-  removed depreciated parameters `gpu_id` and `predictor`
-  set `tree_method = "hist"` by default, match with XGBoost 2.0.0

* Added feature
- Support saving imputation models in a local directory. Users can specify the directory in the parameter `save.models.folder` in the main function `mixgb()`.
- Models will be save as JSON format using `xgb.save()` internally. Saving XGBoost models in this way instead of using `saveRDS` in R is recommended by XGBoost. This can ensure that the imputation models can still be used in later release of XGBoost.
- If users specify the `save.models.folder`, the return object of the main function `mixgb()` will include the current m sets of imputed datasets, a list of directories of the imputation models, and relevant parameters. Users can still save this object using `saveRDS()`, as it does not contain the models themselves. For later use, one can load the object into R and impute new data using `impute_new(object,newdata,...)` 


# mixgb 1.2.1
* Added url of the published article 

# mixgb 1.2.0
* Added feature
- Support logical variable without the need to convert it to factor. 
  Now `mixgb(data,...)` support a dataset with the following data types: 
  - numeric
  - integer
  - factor
  - logical
Please note that variables of character type need to be manually converted to factor by the user first.  

# mixgb 1.1.0
  * Added feature:
  - Added an auxiliary function `default_params()` for the main function `mixgb()`. 
    This function is designed to validate the list of XGBoost hyperparameters that are provided by the user. If users only want to modify a subset of hyperparameter, they can simply pass in those specific hyperparameters as a list in the `xgb_params` argument, without having to listing all other hyperparameters along with their default values.
	
  * Bug fixes:
  - Bugs related to PMM for multiclass variables.
  - Updates `plot_hist()` and `plot_bar()` because the dot-dot notation (`..prop..`) was deprecated in ggplot2 3.4.0.
    - use `after_stat(density)` instead of `..density..` in the function `plot_hist()`
    - use `after_stat(prop)` instead of `..prop..` in the function `plot_bar()`
  
# mixgb 1.0.2
  * Minor change:
  - All examples use `nthread = 2` to meet the requirement of the CRAN policy.
 
# mixgb 1.0.1
  * Major change of default settings for mixgb().
     - Our package has changed from using bootstrapping to subsampling with a default setting of `subsample = 0.7`. After further investigations, we discovered that although bootstrapping often works effectively, it can introduce bias in certain situations. As a result, we have made subsampling the default method instead of bootstrapping.
     - 
  * Current default settings for mixgb().
    -  Subsampling (`subsample = 0.7`) 
    -  No bootstrapping (`bootstrap = FALSE`)
  

# mixgb 0.1.1
* Minor bug fix for the function createNA()
* Change of default settings for mixgb()
  - ordinalAsInteger (from `TRUE` to `FALSE`)
  - max_depth (from 6 to 3)
  - nrounds (from 50 to 100)
  - `bootstrap = TRUE` by default. 


# mixgb 0.1.0
## Main feature
* First release on CRAN
* Support single and multiple imputation
* Customisable settings regarding bootstrapping and predictive matching
* Visual diagnostics for multiply imputed data

## Notes
* Mac OSX users may not be able to use `mixgb` with multithreading. The package `xgboost` requires OpenMP to use multi-core. For more information please check https://mac.r-project.org/openmp/.
