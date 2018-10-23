context("ppp")

test_that("model formulation", {
  # load data
  data(sim_project_data, sim_tree)
  budget <- sum(sim_project_data$cost) * 0.25
  # build formulation
  f <- rcpp_miqp_formulation(
         spp = round(as.matrix(sim_project_data[, sim_tree$tip.label,
                                                drop = FALSE]), 5),
         budget = budget,
         branch_matrix = branch_matrix(sim_tree),
         branch_lengths = sim_tree$edge.lengths,
         costs = sim_project_data$cost,
         success_probabilities = sim_project_data$success,
         locked_in = c(1L), locked_out = c(4L))


})

test_that("solution", {
  stop("TODO")
})

test_that("invalid arguments", {
  stop("TODO")
})
