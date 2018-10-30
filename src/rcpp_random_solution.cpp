#include "package.h"
#include "functions.h"

// [[Rcpp::export]]
Rcpp::LogicalMatrix rcpp_random_solution(arma::sp_mat spp,
                                         double budget,
                                         arma::sp_mat branch_matrix,
                                         Rcpp::NumericVector branch_lengths,
                                         Rcpp::NumericVector costs,
                                         Rcpp::IntegerVector locked_in,
                                         Rcpp::IntegerVector locked_out,
                                         std::size_t n_solutions) {
  // Initialization
  std::size_t n_spp = spp.n_cols;
  std::size_t n_projects = spp.n_rows;
  std::size_t n_branches = branch_matrix.n_cols;
  std::size_t curr_project;
  std::size_t n_remaining_projects;
  double curr_cost;
  double curr_objective_sans_project;
  arma::sp_mat::iterator curr_itr;
  arma::sp_mat curr_remaining_projects(1, n_projects);

  // Preliminary processing
  /// initialize cost
  double initial_cost = std::accumulate(costs.begin(), costs.end(), 0.0);

  /// initialize remaining projects matrix
  std::size_t n_initial_projects = n_projects;
  arma::sp_mat initial_remaining_projects(1, n_projects);
  for (std::size_t i = 0; i < n_projects; ++i)
    initial_remaining_projects(0, i) = 1.0;

  /// initialize solutions matrix
  Rcpp::LogicalMatrix out(n_solutions, n_projects);
  std::fill(out.begin(), out.end(), TRUE);

  /// initialize locked in solutions
  for (auto itr = locked_in.begin(); itr != locked_in.end(); ++itr) {
    initial_remaining_projects.col((*itr) - 1).zeros();
  }

  /// lock in solutions with zero cost
  for (std::size_t p = 0; p < n_projects; ++p) {
    if (costs[p] < 1.0e-15) {
      initial_remaining_projects.col(p).zeros();
    }
  }

  /// lock out projects
  for (auto itr = locked_out.begin(); itr != locked_out.end(); ++itr) {
    initial_remaining_projects.col((*itr) - 1).zeros();
    initial_cost -= costs[(*itr) - 1];
    for (std::size_t y = 0; y < n_solutions; ++y)
      out(y, (*itr) - 1) = FALSE;
  }

  // Main processing
  for (std::size_t y = 0; y < n_solutions; ++y) {
    /// innitialize cost and remaining projects
    curr_cost = initial_cost;
    curr_remaining_projects = initial_remaining_projects;
    n_remaining_projects = curr_remaining_projects.n_nonzero;

    //// generate random solutions
    while ((curr_cost > budget) & (n_remaining_projects > 0)) {
      /// randomly select solution
      curr_project = std::floor(
        R::runif(0.0, static_cast<double>(n_remaining_projects) - 1.0e-15));
      curr_itr = curr_remaining_projects.begin();
      for (std::size_t counter = 0; counter != curr_project; ++counter)
        ++curr_itr;
      curr_project = curr_itr.col();

      /// update the cost
      curr_cost -= costs[curr_project];

      /// remove the selected project from the remaining projects
      curr_remaining_projects.col(curr_project).zeros();
      --n_remaining_projects;

      /// add project to solutions matrix
      out(y, curr_project) = FALSE;
    }
  }

  // Exports
  return out;
}
