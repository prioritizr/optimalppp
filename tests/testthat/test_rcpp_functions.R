context("Rcpp functions")

test_that("rcpp_mip_formulation", {
  # load data
  source("functions.R")
  data(sim_project_data, sim_tree)
  budget <- sum(sim_project_data$cost) * 0.25
  spp_probs <- as.matrix(sim_project_data[, sim_tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(sim_project_data$success,
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(as(round(spp_probs, 5), "dgCMatrix"))
  # build formulation
  f1 <- rcpp_mip_formulation(
          spp = spp_probs,
          budget = budget,
          branch_matrix = branch_matrix(sim_tree),
          branch_lengths = sim_tree$edge.length,
          costs = sim_project_data$cost,
          locked_in = which(sim_project_data$locked_in),
          locked_out = which(sim_project_data$locked_out),
          n_approx_points = 5)
  f2 <- r_mip_formulation(
          project_data = sim_project_data,
          tree = sim_tree,
          budget = budget,
          n_approx_points = 5)
  f1$A <- Matrix::sparseMatrix(i = f1$Ai, j = f1$Aj, x = f1$Ax, index1 = FALSE)
  # run tests
  expect_equal(f1$modelsense, f2$modelsense)
  expect_equivalent(f1$obj, f2$obj)
  expect_equal(f1$rhs, f2$rhs)
  expect_equal(f1$sense, f2$sense)
  expect_equal(length(f1$pwl), length(f2$pwl))
  for (i in seq_along(f1$pwl)) {
    expect_equal(f1$pwl[[1]]$var, f2$pwl[[1]]$var)
    expect_true(max(abs(f1$pwl[[1]]$x - f2$pwl[[1]]$x)) < 1e-4)
    expect_true(max(abs(f1$pwl[[1]]$y - f2$pwl[[1]]$y)) < 1e-5)
  }
})
