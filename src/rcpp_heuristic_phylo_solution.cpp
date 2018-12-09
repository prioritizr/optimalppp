#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::LogicalMatrix rcpp_heuristic_phylo_solution(
                                           arma::sp_mat spp,
                                           arma::sp_mat actions,
                                           double budget,
                                           arma::sp_mat branch_matrix,
                                           Rcpp::NumericVector branch_lengths,
                                           Rcpp::NumericVector costs,
                                           Rcpp::IntegerVector locked_in,
                                           Rcpp::IntegerVector locked_out) {
  // Initialization
  std::size_t n_actions = actions.n_cols;
  double curr_objective;
  std::vector<double> curr_action_benefit(n_actions);
  arma::sp_mat curr_sans_action;
  std::size_t curr_iteration = 1;
  std::size_t curr_action;
  double curr_objective_sans_action;
  std::size_t max_iterations = n_actions;

  // Preliminary processing
  /// initialize current cost
  double curr_cost = std::accumulate(costs.begin(), costs.end(), 0.0);

  /// initialize remaining solution matrix
  arma::sp_mat remaining_actions(1, n_actions);
  for (std::size_t i = 0; i < n_actions; ++i)
    remaining_actions(0, i) = 1.0;

  /// initialize lock in actions vector
  std::vector<bool> locked_in_vector(n_actions, FALSE);
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr) {
    locked_in_vector[(*itr) - 1] = TRUE;
    --max_iterations;
  }

  /// lock out actions
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr) {
    remaining_actions.col((*itr) - 1).zeros();
    curr_cost -= costs[(*itr) - 1];
    --max_iterations;
  }

  /// if multiple actions with zero cost, then subtract remaining from
  // from max iterations
  bool first_zero_cost = FALSE;
  for (std::size_t i = 0; i < n_actions; ++i) {
    if (costs[i] < 1.0e-15) {
      if (first_zero_cost)
        --max_iterations;
      first_zero_cost = TRUE;
    }
  }

  /// initialize n_remaining actions
  std::size_t n_remaining_actions = remaining_actions.n_nonzero;

  /// initialize output matrix with locked out solutions
  Rcpp::LogicalMatrix out(max_iterations, n_actions);
  for (std::size_t i = 0; i < (max_iterations * n_actions); ++i)
     out[i] = TRUE;
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr)
    for (std::size_t i = 0; i < max_iterations; ++i)
      out(i, *itr - 1) = FALSE;

  // Main processing
  while (curr_iteration < max_iterations) {
    /// calculate total objective with all the remaining actions
    curr_objective = ppp_epd(spp, actions, branch_matrix, branch_lengths,
                             remaining_actions)[0];

    /// calculate the benefit for each action
    for (std::size_t i = 0; i < n_actions; ++i) {
      if ((remaining_actions[i] > 0.5) &
          (costs[i] > 1e-16) &
          !locked_in_vector[i]) {
        //// calculate benefit for remaining actions that are not locked in or
        //// associated zero cost
        curr_sans_action = remaining_actions;
        curr_sans_action.col(i).zeros();
        curr_objective_sans_action = ppp_epd(spp, actions, branch_matrix,
                                             branch_lengths,
                                             curr_sans_action)[0];

        curr_action_benefit[i] = (curr_objective -
                                   curr_objective_sans_action) / costs[i];
      } else if (!(costs[i] > 1e-16) &
                 !locked_in_vector[i]) {
        // manually assign large, but finite, benefit to actions with zero cost
        curr_action_benefit[i] = std::numeric_limits<double>::max();
      } else {
        // manually assign infinite benefit to locked in actions,
        // this way zero cost actions are removed before locked in actions
        curr_action_benefit[i] = std::numeric_limits<double>::infinity();
      }
    }

    // find the next selected action
    curr_action = std::distance(curr_action_benefit.begin(),
                                std::min_element(curr_action_benefit.begin(),
                                                 curr_action_benefit.end()));

    // update the cost
    curr_cost -= costs[curr_action];

    // update solution
    for (std::size_t j = curr_iteration; j < max_iterations; ++j)
      out(j, curr_action) = FALSE;

    // remove the selected action from the remaining actions
    remaining_actions.col(curr_action).zeros();
    --n_remaining_actions;
    ++curr_iteration;
  }

  // Exports
  return out;
}
