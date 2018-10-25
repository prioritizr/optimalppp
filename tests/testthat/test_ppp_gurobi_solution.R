context("ppp")

test_that("model formulation", {
  # load data
source("functions.R")
  data(sim_project_data, sim_tree)
  budget <- sum(sim_project_data$cost) * 0.25
  # build formulation
  f1 <- rcpp_miqp_formulation(
          spp = round(as.matrix(sim_project_data[, sim_tree$tip.label,
                                                 drop = FALSE]), 5),
          budget = budget,
          branch_matrix = branch_matrix(sim_tree),
          branch_lengths = sim_tree$edge.length,
          costs = sim_project_data$cost,
          success_probabilities = sim_project_data$success,
          locked_in = which(sim_project_data$locked_in),
          locked_out = which(sim_project_data$locked_out))
  f2 <- r_miqp_formulation(
          project_data = sim_project_data,
          tree = sim_tree,
          budget = budget)
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
