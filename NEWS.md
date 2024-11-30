# mixgb 1.5.2
### For CRAN Submission
- Significantly faster imputation by optimising data preprocessing and the use of **RcppArmadillo**
- Visual diagnostic functions have been move to the `vismi` package
- Documentation


# mixgb 1.4.2
### Bug fix
- Set `drop.unused.levels = FALSE` in `fac2sparse()` to prevent dropping unused levels in factor or ordinal factor.
    - Ensure the feature matrix has the same number of columns to feed in XGBoost  


# mixgb 1.4.1
### Compatibility
- Compatible with both XGBoost >=2.0.0  or CRAN version of XGBoost (=1.7.5.1)

### Bug fix
- Correct feature type for numeric and integer variable for initial imputation
- Update `save_yhatobs()` for Type 1 pmm.


# mixgb 1.4.0

- Optimised `mixgb()` for large datasets: 
  * Significantly faster imputation by optimising data preprocessing and the use of **RcppArmadillo**
  * Enhanced memory efficiency with in-place modifications using **data.table**
  * Bootstrapping option removed from `mixgb()`. Users can still use bootstrap in the archived function `mixgb0()`.
  * `PMM` is now set to `NULL` by default.

# mixgb 1.3.2
### Miscellaneous
- Improves package documentation regarding the import of `xgb.save()` and `xgb.load()` from XGBoost.

# mixgb 1.3.1
### Compatibility
- Makes the package compatible with XGBoost 2.0.0 with GPU support:
  - Introduces a new parameter `device`.
  - Deprecates parameters `gpu_id` and `predictor`.
  - Sets `tree_method = "hist"`  by default, aligning with XGBoost 2.0.0.

### New Features
- Introduces support for saving imputation models to a local directory through the parameter `save.models.folder` in `mixgb()`. 
  - Models save in JSON format using `xgb.save()`, a method recommended by XGBoost for future compatibility.
  - When `save.models.folder` is specified, the return object of `mixgb()` includes the current imputed datasets, directories for imputation models, and relevant parameters. This object can save using `saveRDS()` as it doesn't directly contain the models. Users can later load this object into R and employ `impute_new(object, newdata, ...)` for new data imputation.

# mixgb 1.2.1
### Updates
- Includes the URL of the published article.

# mixgb 1.2.0
### New Features
- Enhances `mixgb(data,...)` to support datasets with diverse data types:
  - numeric
  - integer
  - factor
  - logical
  
  > Note: Users must manually convert character variables to factors.

# mixgb 1.1.0
### New Features
- Introduces `default_params()`, an auxiliary function for `mixgb()`, to validate the list of XGBoost hyperparameters supplied by the user. It simplifies hyperparameter modifications without requiring explicit specification of all default values.

### Bug Fixes
- Addresses issues related to PMM for multiclass variables.
- Updates `plot_hist()` and `plot_bar()` to align with changes in ggplot2 3.4.0:
  - Replaces `..density..` with `after_stat(density)` in `plot_hist()`.
  - Replaces `..prop..` with `after_stat(prop)` in `plot_bar()`.

# mixgb 1.0.2
### Minor Changes
- Adjusts examples to use `nthread = 2` to comply with CRAN policies.

# mixgb 1.0.1
### Changes in Default Settings
- Transitions from bootstrapping to subsampling. Subsampling, set at `subsample = 0.7`, becomes the default method due to identified biases with bootstrapping in certain scenarios.
  - Default for `mixgb()`:
    - Subsampling: `subsample = 0.7`.
    - No bootstrapping: `bootstrap = FALSE`.

# mixgb 0.1.1
### Minor Bug Fixes and Updates
- Resolves a minor issue in the `createNA()` function.
- Modifies default settings in `mixgb()`:
  - `ordinalAsInteger`: Changes from `TRUE` to `FALSE`.
  - `max_depth`: Changes from 6 to 3.
  - `nrounds`: Changes from 50 to 100.
  - `bootstrap`: Sets to `TRUE` by default.

# mixgb 0.1.0
### Initial Release
- First version releases on CRAN.
- Supports both single and multiple imputation.
- Offers customisable settings for bootstrapping and predictive matching.
- Provides visual diagnostics for multiply imputed data.

### Notes
- Mac OSX users might face challenges with multithreading in `mixgb` as `xgboost` requires OpenMP for multi-core operations. For details, please refer to [OpenMP for Mac](https://mac.r-project.org/openmp/).
