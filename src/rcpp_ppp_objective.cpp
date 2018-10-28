#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_ppp_objective(arma::sp_mat spp,
                                       arma::sp_mat branch_matrix,
                                       Rcpp::NumericVector branch_lengths,
                                       arma::sp_mat solutions) {
  return ppp_objective(spp, branch_matrix, branch_lengths, solutions);
}
