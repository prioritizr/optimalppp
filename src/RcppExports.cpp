// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// rcpp_random_solution
Rcpp::LogicalMatrix rcpp_random_solution(arma::sp_mat spp, arma::sp_mat actions, double budget, arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths, Rcpp::NumericVector costs, Rcpp::IntegerVector locked_in, Rcpp::IntegerVector locked_out, std::size_t n_solutions);
RcppExport SEXP _optimalppp_rcpp_random_solution(SEXP sppSEXP, SEXP actionsSEXP, SEXP budgetSEXP, SEXP branch_matrixSEXP, SEXP branch_lengthsSEXP, SEXP costsSEXP, SEXP locked_inSEXP, SEXP locked_outSEXP, SEXP n_solutionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type spp(sppSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type actions(actionsSEXP);
    Rcpp::traits::input_parameter< double >::type budget(budgetSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type branch_lengths(branch_lengthsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_in(locked_inSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_out(locked_outSEXP);
    Rcpp::traits::input_parameter< std::size_t >::type n_solutions(n_solutionsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_random_solution(spp, actions, budget, branch_matrix, branch_lengths, costs, locked_in, locked_out, n_solutions));
    return rcpp_result_gen;
END_RCPP
}
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
// rcpp_branch_probabilities
Rcpp::NumericMatrix rcpp_branch_probabilities(arma::sp_mat spp, arma::sp_mat branch_matrix, arma::sp_mat solutions);
RcppExport SEXP _optimalppp_rcpp_branch_probabilities(SEXP sppSEXP, SEXP branch_matrixSEXP, SEXP solutionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type spp(sppSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type solutions(solutionsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_branch_probabilities(spp, branch_matrix, solutions));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_heuristic_phylo_solution
Rcpp::LogicalMatrix rcpp_heuristic_phylo_solution(arma::sp_mat spp, arma::sp_mat actions, double budget, arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths, Rcpp::NumericVector costs, Rcpp::IntegerVector locked_in, Rcpp::IntegerVector locked_out);
RcppExport SEXP _optimalppp_rcpp_heuristic_phylo_solution(SEXP sppSEXP, SEXP actionsSEXP, SEXP budgetSEXP, SEXP branch_matrixSEXP, SEXP branch_lengthsSEXP, SEXP costsSEXP, SEXP locked_inSEXP, SEXP locked_outSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type spp(sppSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type actions(actionsSEXP);
    Rcpp::traits::input_parameter< double >::type budget(budgetSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type branch_lengths(branch_lengthsSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type costs(costsSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_in(locked_inSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type locked_out(locked_outSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_heuristic_phylo_solution(spp, actions, budget, branch_matrix, branch_lengths, costs, locked_in, locked_out));
    return rcpp_result_gen;
END_RCPP
}
// rcpp_ppp_epd
Rcpp::NumericVector rcpp_ppp_epd(arma::sp_mat spp, arma::sp_mat actions, arma::sp_mat branch_matrix, Rcpp::NumericVector branch_lengths, arma::sp_mat solutions);
RcppExport SEXP _optimalppp_rcpp_ppp_epd(SEXP sppSEXP, SEXP actionsSEXP, SEXP branch_matrixSEXP, SEXP branch_lengthsSEXP, SEXP solutionsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat >::type spp(sppSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type actions(actionsSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type branch_matrix(branch_matrixSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type branch_lengths(branch_lengthsSEXP);
    Rcpp::traits::input_parameter< arma::sp_mat >::type solutions(solutionsSEXP);
    rcpp_result_gen = Rcpp::wrap(rcpp_ppp_epd(spp, actions, branch_matrix, branch_lengths, solutions));
    return rcpp_result_gen;
END_RCPP
}
