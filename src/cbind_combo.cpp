#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat cbind_combo(const Rcpp::List& list_items) {
  int n = list_items.size();
  std::vector<arma::sp_mat> matrices;

  for (int i = 0; i < n; ++i) {
    SEXP item = list_items[i];

    // Check if the current item is a dgCMatrix
    if (Rf_inherits(item, "dgCMatrix")) {
      arma::sp_mat matrix = Rcpp::as<arma::sp_mat>(item);
      matrices.push_back(matrix);
    }
    // Check if the current item is a numeric matrix or vector
    else if (Rcpp::is<Rcpp::NumericMatrix>(item) || Rcpp::is<Rcpp::NumericVector>(item)) {
      arma::mat denseMatrix = Rcpp::as<arma::mat>(item);
      arma::sp_mat spMatrix(denseMatrix);
      matrices.push_back(spMatrix);
    }
    // Check if the current item is an integer matrix or vector
    else if (Rcpp::is<Rcpp::IntegerMatrix>(item) || Rcpp::is<Rcpp::IntegerVector>(item)) {
      // Convert integer matrix/vector to numeric and then to sparse
      arma::mat denseMatrix = Rcpp::as<arma::mat>(Rcpp::as<Rcpp::NumericMatrix>(item));
      arma::sp_mat spMatrix(denseMatrix);
      matrices.push_back(spMatrix);
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

