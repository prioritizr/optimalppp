#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::LogicalMatrix rcpp_heuristic_solution(arma::sp_mat spp,
                                           double budget,
                                           arma::sp_mat branch_matrix,
                                           Rcpp::NumericVector branch_lengths,
                                           Rcpp::NumericVector costs,
                                           Rcpp::IntegerVector locked_in,
                                           Rcpp::IntegerVector locked_out) {
  // Initialization
  std::size_t n_spp = spp.n_cols;
  std::size_t n_projects = spp.n_rows;
  std::size_t n_branches = branch_matrix.n_cols;
  double curr_objective;
  std::vector<double> curr_project_benefit(n_projects);
  arma::sp_mat curr_sans_project;
  std::size_t curr_iteration = 1;
  std::size_t curr_project;
  double curr_objective_sans_project;
  std::size_t max_iterations = n_projects;

  // Preliminary processing
  /// initialize current cost
  double curr_cost = std::accumulate(costs.begin(), costs.end(), 0.0);

  /// initialize remaining solution matrix
  arma::sp_mat remaining_projects(1, n_projects);
  for (std::size_t i = 0; i < n_projects; ++i)
    remaining_projects(0, i) = 1.0;

  /// initialize lock in vector
  std::vector<bool> locked_in_vector(n_projects, FALSE);
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr) {
    locked_in_vector[(*itr) - 1] = TRUE;
    --max_iterations;
  }

  /// lock out projects
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr) {
    remaining_projects.col((*itr) - 1).zeros();
    curr_cost -= costs[(*itr) - 1];
    --max_iterations;
  }


  /// if multiple solutions with zero cost, then subtract remianing from
  // from max iterations
  bool first_zero_cost = FALSE;
  for (std::size_t i = 0; i < n_projects; ++i) {
    if (costs[i] < 1.0e-15) {
      if (first_zero_cost)
        --max_iterations;
      first_zero_cost = TRUE;
    }
  }

  /// initialize n_remaining projects
  std::size_t n_remaining_projects = remaining_projects.n_nonzero;

  /// initialize output matrix with locked out solutions
  Rcpp::LogicalMatrix out(max_iterations, n_projects);
  for (std::size_t i = 0; i < (max_iterations * n_projects); ++i)
     out[i] = TRUE;
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr)
    for (std::size_t i = 0; i < max_iterations; ++i)
      out(i, *itr - 1) = FALSE;

  // Main processing
  while (curr_iteration < max_iterations) {
    /// calculate total objective with all the remaining projects
    curr_objective = ppp_objective(spp, branch_matrix,
                                   branch_lengths, remaining_projects)[0];

    /// calculate the benefit for each project
    for (std::size_t i = 0; i < n_projects; ++i) {
      if ((remaining_projects[i] > 0.5) &
          (costs[i] > 1e-16) &
          !locked_in_vector[i]) {
        //// calculate benefit for remaining projects that are not locked in or
        //// associated zero cost
        curr_sans_project = remaining_projects;
        curr_sans_project.col(i).zeros();
        curr_objective_sans_project = ppp_objective(spp, branch_matrix,
                                                    branch_lengths,
                                                    curr_sans_project)[0];

        curr_project_benefit[i] = (curr_objective -
                                   curr_objective_sans_project) / costs[i];
      } else if (!(costs[i] > 1e-16) &
                 !locked_in_vector[i]) {
        // manually assign large, but finite, benefit to projects with zero cost
        curr_project_benefit[i] = std::numeric_limits<double>::max();
      } else {
        // manually assign infinite benefit to locked in projects,
        // this way zero cost projects are removed before locked in projects
        curr_project_benefit[i] = std::numeric_limits<double>::infinity();
      }
    }

    // find the next selected project
    curr_project = std::distance(curr_project_benefit.begin(),
                                 std::min_element(curr_project_benefit.begin(),
                                                  curr_project_benefit.end()));

    // update the cost
    curr_cost -= costs[curr_project];

    // update solution
    for (std::size_t j = curr_iteration; j < max_iterations; ++j)
      out(j, curr_project) = FALSE;

    // remove the selected project from the remaining projects
    remaining_projects.col(curr_project).zeros();
    --n_remaining_projects;
    ++curr_iteration;
  }

  // Exports
  return out;
}
