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
  f1$A <- Matrix::sparseMatrix(i = f1$Ai, j = f1$Aj, x = f1$Ax, index1 = FALSE)
  # run tests
  expect_equal(f1$modelsense, f2$modelsense)
  expect_equivalent(f1$obj, f2$obj)
  expect_equal(f1$rhs, f2$rhs)
  expect_equal(f1$sense, f2$sense)
  expect_equal(length(f1$pwl), length(f2$pwl))
  for (i in seq_along(f1$pwl)) {
    expect_equal(f1$pwl[[1]]$var, f2$pwl[[1]]$var)
    expect_true(max(abs(f1$pwl[[1]]$x - f2$pwl[[1]]$x)) < 1e-5)
    expect_true(max(abs(f1$pwl[[1]]$y - f2$pwl[[1]]$y)) < 1e-5)
  }
})

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
  tree$edge.length <- c(100, 5, 5)
  s <- ppp_gurobi_solution(project_data, tree, 0.18, "cost", "success")
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 1)
  expect_equal(nrow(s), 4)
  expect_equal(names(s), "solution_1")
  expect_equal(s[[1]], c(FALSE, FALSE, TRUE, TRUE))
})

test_that("solution (single solution, locked in + out constraints)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(requireNamespace("gurobi", quietly = TRUE))
  project_data <- data.frame(name = letters[1:3],
                             cost =       c(1.00, 1.00, 0.00),
                             success =    c(0.01, 0.96, 1.00),
                             S1 =         c(0.01, 0.96, 0.10),
                             S2 =         c(0.01, 0.96, 0.10),
                             locked_in =  c(TRUE, FALSE, FALSE),
                             locked_out = c(FALSE, TRUE, FALSE))
  tree <- ape::read.tree(text = "(S1,S2);")
  # expect warning because the tree has no edge length data
  expect_warning(s <- ppp_gurobi_solution(project_data, tree, 2,
                                          "cost", "success", "locked_in",
                                          "locked_out"))
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 1)
  expect_equal(nrow(s), 3)
  expect_equal(names(s), "solution_1")
  expect_equal(s[[1]], c(TRUE, FALSE, TRUE))
})

test_that("solution (multiple solutions, no constraints)", {
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
  tree$edge.length <- c(100, 5, 5)
  # expect warning because there does not exist 100 solutions
  expect_warning(s <- ppp_gurobi_solution(project_data, tree, 0.18, "cost",
                                          "success", number_solutions = 100))
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 7)
  expect_equal(nrow(s), 4)
  expect_equal(names(s), paste0("solution_", seq_len(7)))
  expect_equal(unique(vapply(s, class, character(1))), "logical")
  expect_equal(anyDuplicated(vapply(s, paste, character(1), collapse = ",")), 0)
})

test_that("solution (multiple solutions, locked in + out constraints)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not(requireNamespace("gurobi", quietly = TRUE))
  project_data <- data.frame(name = letters[1:3],
                             cost =       c(1.00, 1.00, 0.00),
                             success =    c(0.01, 0.96, 1.00),
                             S1 =         c(0.01, 0.96, 0.10),
                             S2 =         c(0.01, 0.96, 0.10),
                             locked_in =  c(TRUE, FALSE, FALSE),
                             locked_out = c(FALSE, TRUE, FALSE))
  tree <- ape::read.tree(text = "(S1,S2);")
  # expect warning because the tree has no edge length data
  expect_warning(s <- ppp_gurobi_solution(project_data, tree, 2,
                                          "cost", "success", "locked_in",
                                          "locked_out", number_solutions = 100))
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 2)
  expect_equal(nrow(s), 3)
  expect_equal(names(s), paste0("solution_", seq_len(2)))
  expect_equal(unique(vapply(s, class, character(1))), "logical")
  expect_equal(anyDuplicated(vapply(s, paste, character(1), collapse = ",")), 0)
})

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
