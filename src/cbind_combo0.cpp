#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat cbind_combo0(const Rcpp::List& list_items) {
  int n = list_items.size();
  std::vector<arma::sp_mat> matrices;

  for (int i = 0; i < n; ++i) {
    SEXP item = list_items[i];

    // Check if the current item is a dgCMatrix
    if (Rf_inherits(item, "dgCMatrix")) {
      arma::sp_mat matrix = Rcpp::as<arma::sp_mat>(item);
      matrices.push_back(matrix);

    }
    // Check if the current item is a numeric vector
    else if (Rcpp::is<Rcpp::NumericVector>(item)) {
      Rcpp::NumericVector vec = Rcpp::as<Rcpp::NumericVector>(item);
      arma::sp_mat colMatrix(vec.size(), 1);
      for (int j = 0; j < vec.size(); ++j) {
        if (vec[j] != 0) {  // Only store non-zero entries in sparse matrix
          colMatrix(j, 0) = vec[j];
        }
      }
      matrices.push_back(colMatrix);

    }
    // Check if the current item is an integer vector
    else if (Rcpp::is<Rcpp::IntegerVector>(item)) {
      Rcpp::IntegerVector vec = Rcpp::as<Rcpp::IntegerVector>(item);
      arma::sp_mat colMatrix(vec.size(), 1);
      for (int j = 0; j < vec.size(); ++j) {
        if (vec[j] != 0) {  // Only store non-zero entries in sparse matrix
          colMatrix(j, 0) = vec[j];
        }
      }
      matrices.push_back(colMatrix);

    }
  }

  // If there are no matrices to concatenate, return an empty matrix
  if (matrices.empty()) {
    return arma::sp_mat();
  }

  // Initialize the result with the first matrix
  arma::sp_mat result = matrices[0];

  // Concatenate the matrices
  for (size_t i = 1; i < matrices.size(); ++i) {
    result = arma::join_horiz(result, matrices[i]);
  }

  return result;
}
