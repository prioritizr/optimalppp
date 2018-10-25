context("ppp")

test_that("model formulation", {
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
  # run tests
  expect_equal(f1$modelsense, f2$modelsense)
  expect_equivalent(f1$obj, f2$obj)
  expect_equal(f1$Qi, f2$Qi)
  expect_equal(f1$Qj, f2$Qj)
  expect_equal(f1$Qx, f2$Qx)
  expect_equal(f1$rhs, f2$rhs)
  expect_equal(f1$sense, f2$sense)
  expect_equal(f1$Ai, f2$Ai)
  expect_equal(f1$Aj, f2$Aj)
  expect_equal(f1$Ax, f2$Ax)
})

test_that("solution", {
  stop("TODO")
})

test_that("invalid arguments", {
  stop("TODO")
})
