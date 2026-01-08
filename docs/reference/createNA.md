# Create missing values for a dataset

This function creates missing values under the missing complete at
random (MCAR) mechanism. It is for demonstration purposes only.

## Usage

``` r
createNA(data, var.names = NULL, p = 0.3)
```

## Arguments

- data:

  A complete data frame.

- var.names:

  The names of variables where missing values will be generated.

- p:

  The proportion of missing values in the data frame or the proportions
  of missing values corresponding to the variables specified in
  `var.names`.

## Value

A data frame with artificial missing values

## Examples

``` r
# Create 30% MCAR data across all variables in a dataset
withNA.df <- createNA(data = iris, p = 0.3)

# Create 30% MCAR data in a specified variable in a dataset
withNA.df <- createNA(data = iris, var.names = c("Sepal.Length"), p = 0.3)

# Create MCAR data in several specified variables in a dataset
withNA.df <- createNA(
  data = iris,
  var.names = c("Sepal.Length", "Petal.Width", "Species"),
  p = c(0.3, 0.2, 0.1)
)
```
