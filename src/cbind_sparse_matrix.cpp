#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::sp_mat cbind_sparse_matrix(const Rcpp::List& matrices_list) {
  // Number of matrices in the list
  int n = matrices_list.size();
  if (n == 0) {
    return arma::sp_mat();  // Return empty sparse matrix if list is empty
  }
  
  // Initialize with the first matrix
  arma::sp_mat result = Rcpp::as<arma::sp_mat>(matrices_list[0]);
  
  
  for (int i = 1; i < n; ++i) {
    // cbind the next matrix horizontally
    result = arma::join_horiz(result, Rcpp::as<arma::sp_mat>(matrices_list[i]));
  }
  
  return result;
}


