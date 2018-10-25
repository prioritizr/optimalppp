#include "package.h"

// [[Rcpp::export]]
Rcpp::List rcpp_mip_formulation(arma::sp_mat spp,
                                double budget,
                                arma::sp_mat branch_matrix,
                                Rcpp::NumericVector branch_lengths,
                                Rcpp::NumericVector costs,
                                Rcpp::IntegerVector locked_in,
                                Rcpp::IntegerVector locked_out,
                                std::size_t n_approx_points) {
  // Initialization
  std::size_t n_spp = spp.n_cols;
  std::size_t n_projects = spp.n_rows;
  std::size_t n_branches = branch_lengths.size();

  // Preliminary processing
  /// identify branches that are not tips
  std::vector<std::size_t> branch_nontip_indices;
  branch_nontip_indices.reserve(n_branches);
  std::vector<std::size_t> branch_tip_indices;
  branch_tip_indices.reserve(n_branches);
  {
    arma::sp_mat sppsums = arma::sum(branch_matrix);
    for (std::size_t i = 0; i < n_branches; ++i) {
      if (sppsums[i] > 1.5) {
        branch_nontip_indices.push_back(i);
      } else {
        branch_tip_indices.push_back(i);
      }
    }
  }
  std::size_t n_branch_nontips = branch_nontip_indices.size();
  std::size_t n_branch_tips = branch_tip_indices.size();

  /// determine number of variables
  std::size_t n_variables = n_projects + (n_projects * n_spp) +
                            n_spp + n_branch_nontips;

  // Main processing
  /// linear component of objective function
  /// here, the branches that correspond to a single species are represented in
  /// the linear component of the objective function
  std::vector<double> model_obj(n_variables, 0.0);
  for (auto itr = branch_tip_indices.cbegin();
       itr != branch_tip_indices.cend(); ++itr)
    model_obj[n_projects + (n_projects * n_spp) + (*itr)] =
      branch_lengths[*itr];

  /// create model lb and ub variables
  /// initialize vectors
  std::vector<double> model_lb(n_variables, 0.0);
  std::vector<double> model_ub(n_variables, 0.0);

  /// set -Inf values for continuous nontip variables
  for (auto itr = branch_nontip_indices.cbegin();
       itr != branch_nontip_indices.cend(); ++itr)
    model_lb[n_projects + (n_projects + n_spp) + (*itr)] =
      -std::numeric_limits<double>::infinity();

  /// set Inf values for continuous nontip variables
  for (auto itr = branch_nontip_indices.cbegin();
       itr != branch_nontip_indices.cend(); ++itr)
    model_ub[n_projects + (n_projects + n_spp) + (*itr)] =
      std::numeric_limits<double>::infinity();

  /// apply locked in constraints
    for (auto itr = locked_in.cbegin(); itr != locked_in.cend(); ++itr)
      model_lb[*itr] = 1.0;

  /// apply locked out constraints
  for (auto itr = locked_out.cbegin(); itr != locked_out.cend(); ++itr)
    model_ub[*itr] = 1.0;

  /// create model vtype variables
  std::vector<std::string> model_vtype(n_variables, "S");
  for (std::size_t i = 0; i < n_projects; ++i)
    model_vtype[i] = "B";
  for (auto itr = branch_nontip_indices.cbegin();
       itr != branch_nontip_indices.cend(); ++itr)
     model_vtype[n_projects + (n_projects * n_spp) + (*itr)] = "C";

  /// linear constraints
  //// initialization
  std::size_t r = 0;
  std::size_t n_A_non_zeros_estimate = costs.size() + (n_projects * n_spp) +
                                       (n_projects * n_spp) +
                                       (n_projects * n_spp * n_branch_nontips);
  std::vector<std::size_t> model_Ai;
  model_Ai.reserve(n_A_non_zeros_estimate);
  std::vector<std::size_t> model_Aj;
  model_Aj.reserve(n_A_non_zeros_estimate);
  std::vector<double> model_Ax;
  model_Ax.reserve(n_A_non_zeros_estimate);
  std::vector<std::string> model_sense;
  model_sense.reserve(n_A_non_zeros_estimate);
  std::vector<double> model_rhs;
  model_rhs.reserve(n_A_non_zeros_estimate);

  //// budget constraint
  for (std::size_t p = 0; p < n_projects; ++p)
    model_Ai.push_back(r);
  for (std::size_t p = 0; p < n_projects; ++p)
    model_Aj.push_back(p);
  for (std::size_t p = 0; p < n_projects; ++p)
    model_Ax.push_back(costs[p]);
  model_sense.push_back("L");
  model_rhs.push_back(budget);

  //// project allocation constraints
  for (std::size_t s = 0; s < n_spp; ++s) {
    for (std::size_t p = 0; p < n_projects; ++p) {
      r += 1;
      model_Ai.push_back(r);
      model_Ai.push_back(r);
      model_Aj.push_back(p);
      model_Aj.push_back((p * n_spp) + s);
      model_Ax.push_back(1.0);
      model_Ax.push_back(-1.0);
      model_sense.push_back(">=");
      model_rhs.push_back(0.0);
    }
  }

  /// species allocation constraints
  for (std::size_t s = 0; s < n_spp; ++s) {
    r += 1;
    for (std::size_t p = 0; p < n_projects; ++p) {
      model_Ai.push_back(r);
      model_Aj.push_back(n_projects + (p * n_spp) + s);
      model_Ax.push_back(1.0);
    }
    model_sense.push_back("=");
    model_rhs.push_back(1.0);
  }

  /// constraints for persistence probabilities for tip branches
  std::size_t curr_branch;
  for (std::size_t s = 0; s < n_spp; ++s) {
    //// increment row
    r += 1;
    //// find tip associated with the s'th species
    curr_branch = branch_matrix.row(s).cbegin().col();
    //// apply constraint for the tip
    for (auto pitr = spp.row(s).cbegin(); pitr != spp.row(s).cend(); ++pitr) {
        model_Ai.push_back(r);
        model_Aj.push_back(n_projects + (pitr.col() * n_spp));
        model_Ax.push_back(*pitr);
    }
    model_Ai.push_back(r);
    model_Aj.push_back(n_projects + (n_projects * n_spp) + curr_branch);
    model_Ax.push_back(1.0);
    model_sense.push_back("=");
    model_rhs.push_back(1.0);
  }

  /// constraints for the log-sum probabilities for nontip branches
  std::vector<std::size_t> model_pwl_var;
  model_pwl_var.reserve(n_spp);
  std::vector<std::vector<double>> model_pwl_x;
  model_pwl_x.reserve(n_spp);
  std::vector<std::vector<double>> model_pwl_y;
  model_pwl_y.reserve(n_spp);
  std::vector<double> curr_x;
  std::vector<double> curr_y;
  double curr_min_value;
  double curr_max_value;
  double curr_frac;
  if (n_branch_nontips > 0) {
    /// initialize variables
    for (auto bitr = branch_nontip_indices.cbegin(); bitr !=
         branch_nontip_indices.cend(); ++bitr) {
      //// increment counters
      r += 1;
      //// apply linear constraints
      for (auto sitr = branch_matrix.col(*bitr).cbegin();
          sitr != branch_matrix.col(*bitr).cend();
          ++sitr) {
        for (auto pitr = spp.row(*sitr).cbegin(); pitr != spp.row(*sitr).cend();
             ++pitr) {
          model_Ai.push_back(r);
          model_Aj.push_back(n_projects + (pitr.col() * n_spp));
          model_Ax.push_back(std::log(1.0 - *pitr));
        }
      }
      model_Ai.push_back(r);
      model_Aj.push_back(n_projects + (n_projects * n_spp) + *bitr);
      model_Ax.push_back(1.0);
      model_sense.push_back("=");
      model_rhs.push_back(0);

      /// apply piecewise linear approximation constraints
      /// calculate extinction probabilities for each spp and project
      curr_min_value = 0;
      curr_max_value = 0;
      for (auto sitr = branch_matrix.col(*bitr).cbegin();
          sitr != branch_matrix.col(*bitr).cend();
          ++sitr) {
          curr_min_value += std::log(1.0 - spp.col(*sitr).max());
          // need to double check does not simply return zeros
          curr_max_value += std::log(1.0 - spp.col(*sitr).min());
      }
      /// inflate the range slighlty to account for floating point precision
      /// issues
      curr_min_value *= 0.99;
      curr_max_value *= 1.01;
      /// add pwl constraints
      model_pwl_var.push_back(*bitr);
      model_pwl_x[*bitr].reserve(n_approx_points);
      model_pwl_y[*bitr].reserve(n_approx_points);
      curr_frac = (curr_max_value - curr_min_value) /
                  static_cast<double>(n_approx_points);
      for (std::size_t i = 0; i < n_approx_points; ++i)
        model_pwl_x[*bitr].push_back(curr_min_value + (i * curr_frac));
      for (auto itr = model_pwl_x[*bitr].begin();
           itr != model_pwl_x[*bitr].cend(); ++itr)
        model_pwl_y[*bitr].push_back(branch_lengths[*bitr] *
                                     (1.0 - std::exp(*itr)));
    }
  }

  // Exports
  return Rcpp::List::create(Rcpp::Named("modelsense") = "min",
                            Rcpp::Named("n_variables") = n_variables,
                            Rcpp::Named("obj") = model_obj,
                            Rcpp::Named("lb") = model_lb,
                            Rcpp::Named("ub") = model_ub,
                            Rcpp::Named("vtype") = model_vtype,
                            Rcpp::Named("Ai") = model_Ai,
                            Rcpp::Named("Aj") = model_Aj,
                            Rcpp::Named("Ax") = model_Ax,
                            Rcpp::Named("rhs") = model_rhs,
                            Rcpp::Named("sense") = model_sense,
                            Rcpp::Named("pwl_var") = model_pwl_var,
                            Rcpp::Named("pwl_x") = model_pwl_x,
                            Rcpp::Named("pwl_y") = model_pwl_y);
}
