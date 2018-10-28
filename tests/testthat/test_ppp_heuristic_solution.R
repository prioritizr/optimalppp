context("ppp_heuristic_solution")

test_that("single solution, no constraints", {
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_heuristic_solution(project_data, tree, 0.18, "name",
                              "cost", "success")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 9)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$objective,
               (100 * (1 - ((1 - (1.0 * 0.1)) * (1 - (0.96 * 0.92))))) +
               (5 * (1 * 0.1)) +
               (5 * (0.96 * 0.92)) +
               (5 * (1 * 0.1)))
  expect_equal(s$cost, 0.1)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "heuristic")
  ## solution columns
  expect_equal(s$a, FALSE)
  expect_equal(s$b, TRUE)
  expect_equal(s$c, FALSE)
  expect_equal(s$d, TRUE)
})

test_that("single solution, locked in constraints", {
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10),
                             locked_in = c(FALSE, FALSE, TRUE, FALSE))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_heuristic_solution(project_data, tree, 0.18, "name",
                              "cost", "success",
                              locked_in_column_name = "locked_in")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 9)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$objective,
               (100 * (1 - ((1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8))))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.94 * 0.8)) +
               (5 * (1 * 0.1)))
  expect_equal(s$cost, 0.15)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "heuristic")
  ## solution columns
  expect_equal(s$a, FALSE)
  expect_equal(s$b, FALSE)
  expect_equal(s$c, TRUE)
  expect_equal(s$d, TRUE)
})

test_that("single solution, locked out constraints", {
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10),
                             locked_out = c(FALSE, TRUE, FALSE, FALSE))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_heuristic_solution(project_data, tree, 0.18, "name",
                              "cost", "success",
                              locked_out_column_name = "locked_out")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 9)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$objective,
               (100 * (1 - ((1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8))))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.94 * 0.8)) +
               (5 * (1 * 0.1)))
  expect_equal(s$cost, 0.15)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "heuristic")
  ## solution columns
  expect_equal(s$a, FALSE)
  expect_equal(s$b, FALSE)
  expect_equal(s$c, TRUE)
  expect_equal(s$d, TRUE)
})

test_that("invalid arguments", {
  # invalid budget
  data(sim_project_data, sim_tree)
  expect_error(ppp_heuristic_solution(sim_project_data, sim_tree, NA_real_,
                                      "cost", "success"))
  expect_error(ppp_heuristic_solution(sim_project_data, sim_tree, "A", "cost",
                                      "success"))
  expect_error(ppp_heuristic_solution(sim_project_data, sim_tree, -5, "cost",
                                      "success"))
  # invalid costs
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost[1] <- NA_real_
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost[1] <- -5
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost <- as.character(sim_project_data$cost)
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  # invalid success
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- NA_real_
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- -1
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- 2
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- NA_real_
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- -1
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- 2
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success")
  })
  # locked in column
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in <- 5
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in[1] <- NA_logical_
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in <- as.character(sim_project_data$locked_in)
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in")
  })
  # locked out column
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_out <- 5
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_out[1] <- NA_logical_
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_out <- as.character(sim_project_data$locked_in)
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$locked_in <- TRUE
    sim_project_data$locked_out <- TRUE
    ppp_heuristic_solution(sim_project_data, sim_tree, 200, "cost", "success",
                        "locked_in", "locked_out")
  })
})
