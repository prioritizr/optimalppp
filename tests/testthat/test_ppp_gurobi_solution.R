context("ppp_gurobi_solution")

test_that("solution (single solution, no constraints)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(requireNamespace("gurobi", quietly = TRUE))
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_gurobi_solution(project_data, tree, 0.18, "name",
                           "cost", "success")
  # tests
  ## class
  expect_is(s, "ppp_results")
  ## project_data
  expect_identical(project_data, s$project_data)
  ## tree
  expect_identical(tree, s$tree)
  ## solution
  expect_is(s$solution, "tbl_df")
  expect_equal(ncol(s$solution), 1)
  expect_equal(nrow(s$solution), 4)
  expect_equal(names(s$solution), "solution_1")
  expect_equal(s$solution[[1]], c(FALSE, FALSE, TRUE, TRUE))
  ## statistics
  expect_equal(ncol(s$statistics), 4)
  expect_equal(nrow(s$statistics), 1)
  expect_equal(names(s$statistics), c("name", "objective", "cost", "optimal"))
  expect_equal(s$statistics$name, "solution_1")
  expect_equal(s$statistics$objective,
               (100 * (1 - (1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8))))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.94 * 0.8))
  expect_equal(s$statistics$cost, 0.15)
  expect_equal(s$statistics$optimal, TRUE)
  ## runtime
  expect_is(s$runtime, "numeric")
  expect_gte(s$runtime, 0)
  expect_lte(s$runtime, 10)
  ## status
  expect_identical(s$status, "OPTIMAL")

})

# test_that("solution (single solution, locked in + out constraints)", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#   skip_if_not(requireNamespace("gurobi", quietly = TRUE))
#   project_data <- data.frame(name = letters[1:3],
#                              cost =       c(1.00, 1.00, 0.00),
#                              success =    c(0.01, 0.96, 1.00),
#                              S1 =         c(0.01, 0.96, 0.10),
#                              S2 =         c(0.01, 0.96, 0.10),
#                              locked_in =  c(TRUE, FALSE, FALSE),
#                              locked_out = c(FALSE, TRUE, FALSE))
#   tree <- ape::read.tree(text = "(S1,S2);")
#   # expect warning because the tree has no edge length data
#   expect_warning(s <- ppp_gurobi_solution(project_data, tree, 2,
#                                           "cost", "success", "locked_in",
#                                           "locked_out"))
#   expect_is(s$solution, "tbl_df")
#   expect_equal(ncol(s$solution), 1)
#   expect_equal(nrow(s$solution), 3)
#   expect_equal(names(s$solution), "solution_1")
#   expect_equal(s$solution[[1]], c(TRUE, FALSE, TRUE))
# })
#
# test_that("solution (multiple solutions, no constraints)", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#   skip_if_not(requireNamespace("gurobi", quietly = TRUE))
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10))
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5)
#   # expect warning because there does not exist 100 solutions
#   expect_warning(s <- ppp_gurobi_solution(project_data, tree, 0.18, "cost",
#                                           "success", number_solutions = 100))
#   expect_is(s$solution, "tbl_df")
#   expect_equal(ncol(s$solution), 7)
#   expect_equal(nrow(s$solution), 4)
#   expect_equal(names(s$solution), paste0("solution_", seq_len(7)))
#   expect_equal(unique(vapply(s$solution, class, character(1))), "logical")
#   expect_equal(anyDuplicated(vapply(s$solution, paste, character(1),
#                                     collapse = ",")), 0)
# })
#
# test_that("solution (multiple solutions, locked in + out constraints)", {
#   skip_on_cran()
#   skip_on_travis()
#   skip_on_appveyor()
#   skip_if_not(requireNamespace("gurobi", quietly = TRUE))
#   project_data <- data.frame(name = letters[1:3],
#                              cost =       c(1.00, 1.00, 0.00),
#                              success =    c(0.01, 0.96, 1.00),
#                              S1 =         c(0.01, 0.96, 0.10),
#                              S2 =         c(0.01, 0.96, 0.10),
#                              locked_in =  c(TRUE, FALSE, FALSE),
#                              locked_out = c(FALSE, TRUE, FALSE))
#   tree <- ape::read.tree(text = "(S1,S2);")
#   # expect warning because the tree has no edge length data
#   expect_warning(s <- ppp_gurobi_solution(project_data, tree, 2,
#                                           "cost", "success", "locked_in",
#                                           "locked_out", number_solutions = 100))
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 2)
#   expect_equal(nrow(s), 3)
#   expect_equal(names(s), paste0("solution_", seq_len(2)))
#   expect_equal(unique(vapply(s, class, character(1))), "logical")
#   expect_equal(anyDuplicated(vapply(s, paste, character(1), collapse = ",")), 0)
# })

test_that("invalid arguments", {
  # invalid budget
  data(sim_project_data, sim_tree)
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, NA_real_,
                                   "cost", "success"))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, "A", "cost",
                                   "success"))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, -5, "cost",
                                   "success"))
  # invalid costs
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost[1] <- NA_real_
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost[1] <- -5
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost <- as.character(sim_project_data$cost)
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  # invalid success
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- NA_real_
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- -1
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- 2
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- NA_real_
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- -1
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- 2
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  # locked in column
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in <- 5
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in[1] <- NA_logical_
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in <- as.character(sim_project_data$locked_in)
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in")
  })
  # locked out column
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_out <- 5
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_out[1] <- NA_logical_
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_out <- as.character(sim_project_data$locked_in)
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in <- TRUE
    sim_project_data$locked_out <- TRUE
    ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
  # reload data
  data(sim_project_data, sim_tree)
  # gap
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", gap = -1))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", gap = NA_real_))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", gap = "a"))
  # threads
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", threads = -1))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", threads = NA_integer_))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", threads = "a"))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", threads = 1.2))
  # number_solutions
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_solutions = -1))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_solutions = NA_integer_))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_solutions = "a"))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_solutions = 1.2))
  # time_limit
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", time_limit = -1))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", time_limit = NA_real_))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", time_limit = 1.2))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", time_limit = "a"))
  # number_approx_points
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_approx_points = -1))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_approx_points = NA_real_))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_approx_points = 1.2))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", number_approx_points = "a"))
  # verbose
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", verbose = -1))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", verbose = NA_logical_))
  expect_error(ppp_gurobi_solution(sim_project_data, sim_tree, 200, "cost",
                                   "success", verbose = "a"))
})
