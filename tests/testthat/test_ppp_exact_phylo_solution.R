context("ppp_exact_phylo_solution")

test_that("solution (single solution, no constraints)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi", "8.0.0")
  project_data <- tibble::tibble(name = letters[1:4],
                                 success =  c(0.95, 0.96, 0.94, 1.00),
                                 S1 =       c(0.91, 0.00, 0.80, 0.10),
                                 S2 =       c(0.00, 0.92, 0.80, 0.10),
                                 S3 =       c(0.00, 0.00, 0.00, 0.10),
                                 A1 =       c(TRUE, FALSE, FALSE, FALSE),
                                 A2 =       c(FALSE, TRUE, FALSE, FALSE),
                                 A3 =       c(FALSE, FALSE, TRUE, FALSE),
                                 A4 =       c(FALSE, FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                                cost =      c(0.10, 0.10, 0.15, 0))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_exact_phylo_solution(project_data, action_data, tree, 0.18, "name",
                                "success", "name", "cost")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1L)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0.18)
  expect_equal(s$obj,
               (100 * (1 - ((1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8))))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.94 * 0.8)) +
               (5 * (1.00 * 0.1)))
  expect_equal(s$cost, 0.15)
  expect_equal(s$optimal, TRUE)
  expect_equal(s$method, "exact")
  ## solution columns
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
})

test_that("solution (single solution, locked in + out constraints)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi", "8.0.0")
  project_data <- tibble::tibble(name = letters[1:3],
                                 success =  c(0.9, 0.96, 1.00),
                                 S1 =       c(0.2, 0.99, 0.10),
                                 S2 =       c(0.00, 0.99, 0.10),
                                 S3 =       c(0.00, 0.99, 0.10),
                                 A1 =       c(TRUE, FALSE, FALSE),
                                 A2 =       c(FALSE, TRUE, FALSE),
                                 A3 =       c(FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3"),
                                cost =      c(0.10, 0.05, 0),
                                locked_in = c(TRUE, FALSE, FALSE),
                                locked_out = c(FALSE, TRUE, FALSE))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  # expect warning because the tree has no edge length data
  expect_warning(s <- ppp_exact_phylo_solution(project_data, action_data, tree,
                                               2, "name", "success", "name",
                                               "cost", "locked_in",
                                               "locked_out"))
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(nrow(s), 1L)
  expect_equal(ncol(s), 9L)
  ## solution columns
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  ## statistics columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 2)
  expect_equal(s$obj,
               (1 * (1 - ((1 - (0.9 * 0.2)) * (1 - (1 * 0.1))))) +
               (1 * (0.9 * .2)) +
               (1 * (1 * 0.1)) +
               (1 * (1 * 0.1)))
  expect_equal(s$cost, 0.1)
  expect_equal(s$optimal, TRUE)
  expect_equal(s$method, "exact")
})

test_that("solution (single solution, tricky problem)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi", "8.0.0")
  project_data <- tibble::tibble(name = letters[1:4],
                                 success =  c(0.9, 0.9, 0.9, 1.00),
                                 S1 =       c(0.3, 0.2, 0.1, 0.01),
                                 S2 =       c(0, 0, 0, 0.01),
                                 A1 =       c(TRUE, FALSE, FALSE, FALSE),
                                 A2 =       c(TRUE, TRUE, FALSE, FALSE),
                                 A3 =       c(TRUE, TRUE, TRUE, FALSE),
                                 A4 =       c(FALSE, FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                                cost =      c(0.5, 0.5, 0.5, 0))
  tree <- ape::read.tree(text = "(S1, S2);")
  tree$edge.length <- c(1, 1)
  s <- ppp_exact_phylo_solution(project_data, action_data, tree, 1.0, "name",
                                "success", "name", "cost")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(nrow(s), 1L)
  expect_equal(ncol(s), 10L)
  ## solution columns
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
  ## statistics columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 1)
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_equal(s$cost, 1)
  expect_equal(s$optimal, TRUE)
  expect_equal(s$method, "exact")

})

test_that("solution (multiple solutions, no constraints)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi", "8.0.0")
  project_data <- tibble::tibble(name = letters[1:3],
                                 success =  c(0.95, 0.96, 1.00),
                                 S1 =       c(0.94, 0.0, 0.10),
                                 S2 =       c(0.00, 0.95, 0.10),
                                 S3 =       c(0.00, 0.0, 0.10),
                                 A1 =       c(TRUE, FALSE, FALSE),
                                 A2 =       c(FALSE, TRUE, FALSE),
                                 A3 =       c(FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3"),
                                cost =      c(0.10, 0.05, 0))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  # expect warning because there does not exist 100 solutions
  expect_warning({
    s <- ppp_exact_phylo_solution(project_data, action_data, tree, 0.18, "name",
                                  "success", "name", "cost",
                                  number_solutions = 100)
  })
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(nrow(s), (2 ^ nrow(action_data)) - 1)
  expect_equal(ncol(s), 9L)
  ## solution columns
  expect_equal(unique(vapply(s[, action_data$name], class, character(1))),
               "logical")
  expect_equal(sum(duplicated(s[, action_data$name])), 0)
  ## statistics columns
  expect_equal(s$budget, rep(0.18, 7))
  expect_equal(s$solution, seq_len(7))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_is(s$cost, "numeric")
  expect_true(all(s$cost >= 0))
  expect_equal(s$optimal, c(TRUE, rep(FALSE, nrow(s) - 1)))
  expect_equal(s$method, rep("exact", nrow(s)))
})

test_that("solution (multiple solutions, locked in + out constraints)", {
  skip_on_cran()
  skip_on_travis()
  skip_on_appveyor()
  skip_if_not_installed("gurobi", "8.0.0")
  project_data <- tibble::tibble(name = letters[1:4],
                                 success =  c(0.95, 0.96, 0.94, 1.00),
                                 S1 =       c(0.91, 0.00, 0.80, 0.10),
                                 S2 =       c(0.00, 0.92, 0.80, 0.10),
                                 S3 =       c(0.00, 0.00, 0.00, 0.10),
                                 A1 =       c(TRUE, FALSE, FALSE, FALSE),
                                 A2 =       c(FALSE, TRUE, FALSE, FALSE),
                                 A3 =       c(FALSE, FALSE, TRUE, FALSE),
                                 A4 =       c(FALSE, FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3", "A4"),
                                cost =      c(0.10, 0.10, 0.15, 0),
                                locked_in = c(TRUE, FALSE, FALSE, FALSE),
                                locked_out = c(FALSE, TRUE, FALSE, FALSE))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_exact_phylo_solution(project_data, action_data, tree, 0.18, "name",
                                "success", "name", "cost")
  # expect warning because there do not exist 100 solutions
  expect_warning(s <- ppp_exact_phylo_solution(project_data, action_data, tree,
                                               100, "name", "success", "name",
                                               "cost", "locked_in",
                                               "locked_out",
                                               number_solutions = 100))
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(nrow(s), 4L)
  expect_equal(ncol(s), 10L)
  ## solution columns
  expect_equal(unique(vapply(s[, action_data$name], class, character(1))),
               "logical")
  expect_equal(sum(duplicated(s[, action_data$name])), 0)
  ## statistics columns
  expect_equal(s$budget, rep(100, nrow(s)))
  expect_equal(s$solution, seq_len(nrow(s)))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_true(all(s$cost >= 0))
  expect_equal(s$optimal, c(TRUE, rep(FALSE, nrow(s) - 1)))
  expect_equal(s$method, rep("exact", nrow(s)))
})

test_that("invalid arguments", {
  # verify that function works using built-in dataset
  data(sim_project_data, sim_action_data, sim_tree)
  expect_is(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                     sim_tree, 1000, "name", "success",
                                     "name", "cost"), "tbl_df")
  # invalid names
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, 100, "name1", "success", "name",
                              "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, 100, "name", "success1", "name",
                              "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, 100, "name", "success", "name1",
                              "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, 100, "name", "success", "name",
                              "cost1")
  })
  # invalid budget
  data(sim_project_data, sim_action_data, sim_tree)
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, NA_real_,
                                        "name", "success", "name", "cost"))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, "A", "name", "success",
                                        "name", "cost"))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, -5, "name", "success",
                                        "name", "cost"))
  # invalid success
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success[1] <- NA_real_
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success[1] <- -1
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success[1] <- 2
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  # invalid costs
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$cost[1] <- NA_real_
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$cost[1] <- -5
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$cost <- "2"
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1[1] <- NA_real_
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1[1] <- -1
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1[1] <- 2
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost")
  })
  # locked in column
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$locked_in <- 5
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost", "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$locked_in[1] <- NA
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost", "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$locked_in <- "T"
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost", "locked_in")
  })
  # locked out column
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$locked_out <- 5
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost",
                             "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$locked_out[1] <- NA
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost",
                             "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$locked_out <- "A"
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost",
                             "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$locked_in <- TRUE
    sim_action_data$locked_out <- TRUE
    ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree, 200,
                             "name", "success", "name", "cost",
                             "locked_in", "locked_out")
  })
  # reload data
  data(sim_project_data, sim_action_data, sim_tree)
  # gap
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", gap = -1))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", gap = NA_real_))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", gap = "a"))
  # threads
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", threads = -1))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", threads = NA_integer_))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", threads = "a"))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", threads = 1.2))
  # number_solutions
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", number_solutions = -1))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        number_solutions = NA_integer_))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        number_solutions = "a"))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        number_solutions = 1.2))
  # time_limit
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost", time_limit = -1))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        time_limit = NA_integer_))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        time_limit = "a"))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        time_limit = 1.2))
  # number_approx_points
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        number_approx_points = -1))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        number_approx_points = NA_integer_))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        number_approx_points = "a"))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        number_approx_points = 1.2))
  # verbose
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        verbose = -1))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        verbose = NA))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        verbose = "a"))
  expect_error(ppp_exact_phylo_solution(sim_project_data, sim_action_data,
                                        sim_tree, 200, "name", "success",
                                        "name", "cost",
                                        verbose = 1.2))
})
