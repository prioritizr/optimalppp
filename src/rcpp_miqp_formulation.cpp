#include "package.h"

// [[Rcpp::export]]
Rcpp::List rcpp_miqp_formulation(Rcpp::NumericMatrix spp,
                                 double budget,
                                 arma::sp_mat branch_matrix,
                                 Rcpp::NumericVector branch_lengths,
                                 Rcpp::NumericVector costs,
                                 Rcpp::NumericVector success_probabilities,
                                 Rcpp::IntegerVector locked_in,
                                 Rcpp::IntegerVector locked_out) {
  // Initialization
  std::size_t n_spp = spp.ncol();
  std::size_t n_projects = spp.nrow();
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
      if (sppsums[i] > 0.5) {
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
  std::size_t ostart = n_projects + (n_projects * n_spp);
  for (std::size_t b = 0; b < n_branch_tips; ++b) {
    model_obj[ostart + b] = branch_lengths[branch_tip_indices[b]];
  }

  /// quadratic component of objective function
  /// here, the branches that correspond to multiple species are represented in
  /// the quadratic component of the objective function
  //// initialize Q
  std::size_t n_Q_non_zeros_estimate =
    static_cast<std::size_t>(arma::accu(branch_matrix)) * 2;
  std::vector<std::size_t> model_Qi;
  model_Qi.reserve(n_Q_non_zeros_estimate);
  std::vector<std::size_t> model_Qj;
  model_Qj.reserve(n_Q_non_zeros_estimate);
  std::vector<double> model_Qx;
  model_Qx.reserve(n_Q_non_zeros_estimate);

  //// fill Q with values
  {
    std::size_t firstspp;
    arma::sp_mat::col_iterator j;
    std::size_t qstart = n_projects + (n_projects * n_spp) + n_spp;
    for (std::size_t b = 0; b < branch_nontip_indices.size(); ++b) {
      j = branch_matrix.begin_col(branch_nontip_indices[b]);
      firstspp = j.row();
      for (;j != branch_matrix.end_col(branch_nontip_indices[b]); ++j) {
        model_Qi.push_back(qstart + static_cast<std::size_t>(firstspp));
        model_Qj.push_back(qstart + static_cast<std::size_t>(j.row()));
        model_Qx.push_back(branch_lengths[branch_nontip_indices[b]] * 0.5);
        model_Qi.push_back(qstart + static_cast<std::size_t>(j.row()));
        model_Qj.push_back(qstart + static_cast<std::size_t>(firstspp));
        model_Qx.push_back(branch_lengths[branch_nontip_indices[b]] * 0.5);
      }
    }
  }

  /// linear constraints
  //// initialization
  std::size_t r = 0;
  std::size_t n_A_non_zeros_estimate = costs.size() + (n_projects * n_spp) +
                                       (n_projects * n_spp) +
                                       (n_projects * n_spp) + n_spp;
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

  //// species allocation constraints
  for (std::size_t s = 0; s < n_spp; ++s) {
    for (std::size_t p = 0; p < n_projects; ++p) {
      r += 1;
      model_Ai.push_back(r);
      model_Ai.push_back(r);
      model_Aj.push_back(p);
      model_Aj.push_back((p * n_spp) + s);
      model_Ax.push_back(1.0);
      model_Ax.push_back(-1.0);
      model_sense.push_back("G");
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
    model_sense.push_back("E");
    model_rhs.push_back(1.0);
  }

  //// species extinction probability constraints
  for (std::size_t s = 0; s < n_spp; ++s) {
    r += 1;
    for (std::size_t p = 0; p < n_projects; ++p) {
      if (spp(p, s) > 1.0e-10) {
        model_Ai.push_back(r);
        model_Aj.push_back(n_projects + (p * n_spp) + s);
        model_Ax.push_back(spp(p, s));
      }
    }
    model_sense.push_back("E");
    model_rhs.push_back(1.0);
  }

  /// create model lb and ub variables
  std::vector<double> model_lb(n_variables, 0.0);
  std::vector<double> model_ub(n_variables, 0.0);
  for (std::size_t i = n_variables - 1, j = 0; j < n_branch_nontips; ++j)
       model_ub[i] = 1.0;
  for (std::size_t i = 0; i < locked_in.size(); ++i)
    model_lb[i] = 1.0;
  for (std::size_t i = 0; i < locked_out.size(); ++i)
    model_ub[i] = 0.0;

  /// create model vtype variables
  std::vector<std::string> model_vtype(n_variables, "S");
  for (std::size_t i = 0; i < n_projects; ++i)
    model_vtype[i] = "B";
  for (std::size_t i = n_variables - 1, j = 0; j < n_branch_nontips; --i, ++j)
       model_vtype[i] = "B";

  // Exports
  return Rcpp::List::create(Rcpp::Named("modelsense") = "min",
                            Rcpp::Named("n_variables") = n_variables,
                            Rcpp::Named("obj") = model_obj,
                            Rcpp::Named("Qi") = model_Qi,
                            Rcpp::Named("Qj") = model_Qj,
                            Rcpp::Named("Qx") = model_Qx,
                            Rcpp::Named("lb") = model_lb,
                            Rcpp::Named("ub") = model_ub,
                            Rcpp::Named("vtype") = model_vtype,
                            Rcpp::Named("Ai") = model_Ai,
                            Rcpp::Named("Aj") = model_Aj,
                            Rcpp::Named("Ax") = model_Ax,
                            Rcpp::Named("rhs") = model_rhs,
                            Rcpp::Named("sense") = model_sense);
}
