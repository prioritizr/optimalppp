#include "package.h"

// [[Rcpp::export]]
Rcpp::NumericVector rcpp_ppp_objective(arma::sp_mat spp,
                                       arma::sp_mat branch_matrix,
                                       Rcpp::NumericVector branch_lengths,
                                       arma::sp_mat solutions) {
  // Initialization
  std::size_t n_spp = spp.n_cols;
  std::size_t n_projects = spp.n_rows;
  std::size_t n_branches = branch_matrix.n_cols;
  std::size_t n_solutions = solutions.n_rows;
  Rcpp::NumericVector out(n_solutions, 0.0);
  std::vector<double> curr_solution_best_project_per_species(n_spp);
  double curr_spp_prob;
  double curr_branch_prob;
  // Main processing
  for (std::size_t sol = 0; sol < n_solutions; ++sol) {
    // find best project for each species in the solution
    for (std::size_t s = 0; s < n_spp; ++s) {
      curr_solution_best_project_per_species[s] = 0.0;
      for (auto pitr = solutions.begin_row(sol);
           pitr != solutions.end_row(sol);
           ++pitr) {
          curr_spp_prob = (*pitr) * spp(pitr.col(), s);
        if (curr_spp_prob > curr_solution_best_project_per_species[s])
          curr_solution_best_project_per_species[s] = curr_spp_prob;
      }
    }
    // iterate over each branch and calculate the expected amount of
    // evolutionary history that is conserved given each solution
    for (std::size_t b = 0; b < n_branches; ++b) {
      curr_branch_prob = 1.0;
      for (auto sitr = branch_matrix.begin_col(b);
           sitr != branch_matrix.end_col(b);
           ++sitr) {
        curr_branch_prob *=
          (1.0 - curr_solution_best_project_per_species[sitr.row()]);
      }
      out[sol] += (branch_lengths[b] * (1.0 - curr_branch_prob));
    }
  }

  // Exports
  return out;
}
