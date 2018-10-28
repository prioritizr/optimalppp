// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rcpp_branch_matrix
arma::sp_mat rcpp_branch_matrix(Rcpp::List x);
RcppExport SEXP _optimalppp_rcpp_branch_matrix(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::List >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_branch_matrix(x));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_heuristic_solution
Rcpp::LogicalMatrix rcpp_heuristic_solution(arma::sp_mat spp, double budget, arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths, Rcpp::NumericVector costs, Rcpp::IntegerVector locked_in, Rcpp::IntegerVector locked_out);
RcppExport SEXP _optimalppp_rcpp_heuristic_solution(SEXP sppSEXP, SEXP budgetSEXP, SEXP branch_matrixSEXP, SEXP branch_lengthsSEXP, SEXP costsSEXP, SEXP locked_inSEXP, SEXP locked_outSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type spp(sppSEXP);
    Rcpp::traits::input_parameter< double >::type budget(budgetSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type branch_lengths(branch_lengthsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_in(locked_inSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_out(locked_outSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_heuristic_solution(spp, budget, branch_matrix, branch_lengths, costs, locked_in, locked_out));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_mip_formulation
Rcpp::List rcpp_mip_formulation(arma::sp_mat spp, double budget, arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths, Rcpp::NumericVector costs, Rcpp::IntegerVector locked_in, Rcpp::IntegerVector locked_out, std::size_t n_approx_points);
RcppExport SEXP _optimalppp_rcpp_mip_formulation(SEXP sppSEXP, SEXP budgetSEXP, SEXP branch_matrixSEXP, SEXP branch_lengthsSEXP, SEXP costsSEXP, SEXP locked_inSEXP, SEXP locked_outSEXP, SEXP n_approx_pointsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type spp(sppSEXP);
    Rcpp::traits::input_parameter< double >::type budget(budgetSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type branch_lengths(branch_lengthsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_in(locked_inSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_out(locked_outSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n_approx_points(n_approx_pointsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_mip_formulation(spp, budget, branch_matrix, branch_lengths, costs, locked_in, locked_out, n_approx_points));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_ppp_objective
Rcpp::NumericVector rcpp_ppp_objective(arma::sp_mat spp, arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths, arma::sp_mat solutions);
RcppExport SEXP _optimalppp_rcpp_ppp_objective(SEXP sppSEXP, SEXP branch_matrixSEXP, SEXP branch_lengthsSEXP, SEXP solutionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type spp(sppSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type branch_lengths(branch_lengthsSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type solutions(solutionsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ppp_objective(spp, branch_matrix, branch_lengths, solutions));
    return rcpp_result_gen;
END_RCPP
}
