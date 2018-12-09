#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::LogicalMatrix rcpp_random_solution(arma::sp_mat spp,
                                         arma::sp_mat actions,
                                         double budget,
                                         arma::sp_mat branch_matrix,
                                         Rcpp::NumericVector branch_lengths,
                                         Rcpp::NumericVector costs,
                                         Rcpp::IntegerVector locked_in,
                                         Rcpp::IntegerVector locked_out,
                                         std::size_t n_solutions) {
  // Initialization
  std::size_t n_actions = actions.n_rows;
  std::size_t curr_action;
  std::size_t n_remaining_actions;
  double curr_cost;
  arma::sp_mat::iterator curr_itr;
  arma::sp_mat curr_remaining_actions(1, n_actions);

  // Preliminary processing
  /// initialize cost
  double initial_cost = std::accumulate(costs.begin(), costs.end(), 0.0);

  /// initialize remaining actions matrix
  arma::sp_mat initial_remaining_actions(1, n_actions);
  for (std::size_t i = 0; i < n_actions; ++i)
    initial_remaining_actions(0, i) = 1.0;

  /// initialize solutions matrix
  Rcpp::LogicalMatrix out(n_solutions, n_actions);
  std::fill(out.begin(), out.end(), TRUE);

  /// initialize locked in solutions
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr) {
    initial_remaining_actions.col((*itr) - 1).zeros();
  }

  /// lock in solutions with zero cost
  for (std::size_t p = 0; p < n_actions; ++p) {
    if (costs[p] < 1.0e-15) {
      initial_remaining_actions.col(p).zeros();
    }
  }

  /// lock out actions
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr) {
    initial_remaining_actions.col((*itr) - 1).zeros();
    initial_cost -= costs[(*itr) - 1];
    for (std::size_t y = 0; y < n_solutions; ++y)
      out(y, (*itr) - 1) = FALSE;
  }

  // Main processing
  for (std::size_t y = 0; y < n_solutions; ++y) {
    /// innitialize cost and remaining actions
    curr_cost = initial_cost;
    curr_remaining_actions = initial_remaining_actions;
    n_remaining_actions = curr_remaining_actions.n_nonzero;

    //// generate random solutions
    while ((curr_cost > budget) & (n_remaining_actions > 0)) {
      /// randomly select solution
      curr_action = std::floor(
        R::runif(0.0, static_cast<double>(n_remaining_actions) - 1.0e-15));
      curr_itr = curr_remaining_actions.begin();
      for (std::size_t counter = 0; counter != curr_action; ++counter)
        ++curr_itr;
      curr_action = curr_itr.col();

      /// update the cost
      curr_cost -= costs[curr_action];

      /// remove the selected action from the remaining actions
      curr_remaining_actions.col(curr_action).zeros();
      --n_remaining_actions;

      /// add action to solutions matrix
      out(y, curr_action) = FALSE;
    }
  }

  // Exports
  return out;
}
