#ifndef FUNCTIONS_H
#define FUNCTIONS_H

#include "package.h"

Rcpp::NumericVector ppp_objective(arma::sp_mat, arma::sp_mat,
                                  Rcpp::NumericVector, arma::sp_mat);

#endif
