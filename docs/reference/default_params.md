# Auxiliary function for validating xgb.params

Auxiliary function for setting up the default XGBoost-related
hyperparameters for mixgb and checking the `xgb.params` argument in
[`mixgb()`](https://agnesdeng.github.io/mixgb/reference/mixgb.md). For
more details on XGBoost hyperparameters, please refer to [XGBoost
documentation on
parameters](https://xgboost.readthedocs.io/en/stable/parameter.html).

## Usage

``` r
default_params(
  device = "cpu",
  tree_method = "hist",
  eta = 0.3,
  gamma = 0,
  max_depth = 3,
  min_child_weight = 1,
  max_delta_step = 0,
  subsample = 0.7,
  sampling_method = "uniform",
  colsample_bytree = 1,
  colsample_bylevel = 1,
  colsample_bynode = 1,
  lambda = 1,
  alpha = 0,
  max_leaves = 0,
  max_bin = 256,
  num_parallel_tree = 1,
  nthread = -1
)
```

## Arguments

- device:

  Can be either `"cpu"` or `"cuda"`. For other options please refer to
  [XGBoost documentation on
  parameters](https://xgboost.readthedocs.io/en/stable/parameter.html#general-parameters).

- tree_method:

  Options: `"auto"`, `"exact"`, `"approx"`, and `"hist"`. Default:
  `"hist"`.

- eta:

  Step size shrinkage. Default: 0.3.

- gamma:

  Minimum loss reduction required to make a further partition on a leaf
  node of the tree. Default: 0

- max_depth:

  Maximum depth of a tree. Default: 3.

- min_child_weight:

  Minimum sum of instance weight needed in a child. Default: 1.

- max_delta_step:

  Maximum delta step. Default: 0.

- subsample:

  Subsampling ratio of the data. Default: 0.7.

- sampling_method:

  The method used to sample the data. Default: `"uniform"`.

- colsample_bytree:

  Subsampling ratio of columns when constructing each tree. Default: 1.

- colsample_bylevel:

  Subsampling ratio of columns for each level. Default: 1.

- colsample_bynode:

  Subsampling ratio of columns for each node. Default: 1.

- lambda:

  L2 regularization term on weights. Default: 1.

- alpha:

  L1 regularization term on weights. Default: 0.

- max_leaves:

  Maximum number of nodes to be added (Not used when
  `tree_method = "exact"`). Default: 0.

- max_bin:

  Maximum number of discrete bins to bucket continuous features (Only
  used when `tree_method` is either `"hist"`, `"approx"` or
  `"gpu_hist"`). Default: 256.

- num_parallel_tree:

  The number of parallel trees used for boosted random forests. Default:
  1.

- nthread:

  The number of CPU threads to be used. Default: -1 (all available
  threads).

## Value

A list of hyperparameters.

## Examples

``` r
default_params()
#> $device
#> [1] "cpu"
#> 
#> $tree_method
#> [1] "hist"
#> 
#> $eta
#> [1] 0.3
#> 
#> $gamma
#> [1] 0
#> 
#> $max_depth
#> [1] 3
#> 
#> $min_child_weight
#> [1] 1
#> 
#> $subsample
#> [1] 0.7
#> 
#> $sampling_method
#> [1] "uniform"
#> 
#> $colsample_bytree
#> [1] 1
#> 
#> $colsample_bylevel
#> [1] 1
#> 
#> $colsample_bynode
#> [1] 1
#> 
#> $lambda
#> [1] 1
#> 
#> $alpha
#> [1] 0
#> 
#> $max_leaves
#> [1] 0
#> 
#> $max_bin
#> [1] 256
#> 
#> $num_parallel_tree
#> [1] 1
#> 
#> $nthread
#> [1] -1
#> 

xgb.params <- list(device = "cuda", subsample = 0.9, nthread = 2)
default_params(
  device = xgb.params$device,
  subsample = xgb.params$subsample,
  nthread = xgb.params$nthread
)
#> $device
#> [1] "cuda"
#> 
#> $tree_method
#> [1] "hist"
#> 
#> $eta
#> [1] 0.3
#> 
#> $gamma
#> [1] 0
#> 
#> $max_depth
#> [1] 3
#> 
#> $min_child_weight
#> [1] 1
#> 
#> $subsample
#> [1] 0.9
#> 
#> $sampling_method
#> [1] "uniform"
#> 
#> $colsample_bytree
#> [1] 1
#> 
#> $colsample_bylevel
#> [1] 1
#> 
#> $colsample_bynode
#> [1] 1
#> 
#> $lambda
#> [1] 1
#> 
#> $alpha
#> [1] 0
#> 
#> $max_leaves
#> [1] 0
#> 
#> $max_bin
#> [1] 256
#> 
#> $num_parallel_tree
#> [1] 1
#> 
#> $nthread
#> [1] 2
#> 

xgb.params <- do.call("default_params", xgb.params)
xgb.params
#> $device
#> [1] "cuda"
#> 
#> $tree_method
#> [1] "hist"
#> 
#> $eta
#> [1] 0.3
#> 
#> $gamma
#> [1] 0
#> 
#> $max_depth
#> [1] 3
#> 
#> $min_child_weight
#> [1] 1
#> 
#> $subsample
#> [1] 0.9
#> 
#> $sampling_method
#> [1] "uniform"
#> 
#> $colsample_bytree
#> [1] 1
#> 
#> $colsample_bylevel
#> [1] 1
#> 
#> $colsample_bynode
#> [1] 1
#> 
#> $lambda
#> [1] 1
#> 
#> $alpha
#> [1] 0
#> 
#> $max_leaves
#> [1] 0
#> 
#> $max_bin
#> [1] 256
#> 
#> $num_parallel_tree
#> [1] 1
#> 
#> $nthread
#> [1] 2
#> 
```
