context("ppp_heuristic_spp_solution")

test_that("single solution, no constraints", {
  # make data
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
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  s <- ppp_heuristic_spp_solution(project_data, action_data, spp, 0.18,
                                  "name", "success", "name", "cost", "name",
                                  "weight")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0.18)
  expect_equal(s$cost, 0.1)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "heuristic")
  expect_equal(s$obj,
               (5 * (1 * 0.1)) +
               (5 * (0.96 * 0.92)) +
               (1 * (1 * 0.1)))
  ## solution columns
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
})

test_that("single solution, zero budget", {
  # make data
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
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  s <- ppp_heuristic_spp_solution(project_data, action_data, spp, 0,
                                  "name", "success", "name", "cost", "name",
                                  "weight")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0)
  expect_equal(s$obj,
               (5 * (1 * 0.1)) +
               (5 * (1 * 0.1)) +
               (1 * (1 * 0.1)))
  expect_equal(s$cost, 0)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "heuristic")
  ## solution columns
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
})

test_that("single solution, locked in constraints", {
  # make data
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
                                locked_in = c(TRUE, FALSE, FALSE, FALSE))
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  s <- ppp_heuristic_spp_solution(project_data, action_data, spp, 0.18,
                                  "name", "success", "name", "cost", "name",
                                  "weight",
                                  locked_in_column_name = "locked_in")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0.18)
  expect_equal(s$obj,
               (5 * (0.95 * 0.91)) +
               (5 * (1 * 0.1)) +
               (1 * (1 * 0.1)))
  expect_equal(s$cost, 0.1)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "heuristic")
  ## solution columns
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
})

test_that("single solution, locked out constraints", {
  # make data
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
                                locked_out = c(FALSE, TRUE, FALSE, FALSE))
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  s <- ppp_heuristic_spp_solution(project_data, action_data, spp, 0.18,
                                  "name", "success", "name", "cost", "name",
                                  "weight",
                                  locked_out_column_name = "locked_out")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0.18)
  expect_equal(s$obj,
               (5 * (0.94 * 0.8)) +
               (5 * (0.94 * 0.8)) +
               (1 * (1 * 0.1)))
  expect_equal(s$cost, 0.15)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "heuristic")
  ## solution columns
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
})

test_that("multiple solutions, cost limit", {
  # make data
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
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  expect_warning(s <- ppp_heuristic_spp_solution(project_data, action_data,
                                                 spp, 0.15, "name",
                                                 "success", "name",
                                                 "cost", "name", "weight",
                                                  number_solutions = 100))
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 2L)
  ## statistic columns
  expect_equal(s$solution, seq_len(2L))
  expect_equal(s$budget, rep(0.15, 2))
  expect_equal(s$obj[1],
               (5 * (1 * 0.1)) +
               (5 * (0.96 * 0.92)) +
               (1 * (1 * 0.1)))
  expect_equal(s$obj[2],
               (5 * (1 * 0.1)) +
               (5 * (1 * 0.1)) +
               (1 * (1 * 0.1)))
  expect_equal(s$cost, c(0.1, 0))
  expect_equal(s$optimal, rep(NA, 2))
  expect_equal(s$method, rep("heuristic", 2))
  ## solution columns
  expect_equal(s$A1, c(FALSE, FALSE))
  expect_equal(s$A2, c(TRUE, FALSE))
  expect_equal(s$A3, c(FALSE, FALSE))
  expect_equal(s$A4, rep(TRUE, 2))
})

test_that("multiple solutions, no constraints", {
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
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  expect_warning(s <- ppp_heuristic_spp_solution(project_data, action_data,
                                                 spp, 100, "name",
                                                 "success", "name",
                                                 "cost", "name", "weight",
                                                 number_solutions = 100))
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 4L)
  ## statistic columns
  expect_equal(s$solution, seq_len(4L))
  expect_equal(s$budget, rep(100, 4))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_equal(s$cost, c(0.35, 0.2, 0.1, 0))
  expect_equal(s$optimal, rep(NA, 4))
  expect_equal(s$method, rep("heuristic", 4))
  ## solution columns
  expect_equal(s$A1, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(s$A2, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(s$A3, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(s$A4, rep(TRUE, 4))
})

test_that("multiple solutions, locked in constraints", {
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
                                locked_in = c(TRUE, FALSE, FALSE, FALSE))
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  expect_warning(s <- ppp_heuristic_spp_solution(project_data, action_data,
                                                   tree, 100, "name",
                                                   "success", "name",
                                                   "cost", "name", "weight",
                                                   locked_in_column_name =
                                                     "locked_in",
                                                   number_solutions = 100))
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 3L)
  ## statistic columns
  expect_equal(s$solution, seq_len(3L))
  expect_equal(s$budget, rep(100, 3L))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_equal(s$cost, c(0.35, 0.2, 0.1))
  expect_equal(s$optimal, rep(NA, 3L))
  expect_equal(s$method, rep("heuristic", 3L))
  ## solution columns
  expect_equal(s$A1, c(TRUE, TRUE, TRUE))
  expect_equal(s$A2, c(TRUE, TRUE, FALSE))
  expect_equal(s$A3, c(TRUE, FALSE, FALSE))
  expect_equal(s$A4, rep(TRUE, 3L))
})

test_that("multiple solutions, locked out constraints", {
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
                                locked_out = c(FALSE, TRUE, FALSE, FALSE))
  spp <- tibble::tibble(name = c("S1", "S2", "S3"), weight = c(5, 5, 1))
  tree <- star_phylogeny(spp[[1]], spp[[2]])
  expect_warning(s <- ppp_heuristic_spp_solution(project_data, action_data,
                                                   tree, 100, "name",
                                                   "success", "name",
                                                   "cost", "name", "weight",
                                                   locked_out_column_name =
                                                     "locked_out",
                                                   number_solutions = 100))
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 3L)
  ## statistic columns
  expect_equal(s$solution, seq_len(3L))
  expect_equal(s$budget, rep(100, 3L))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_equal(s$cost, c(0.25, 0.15, 0))
  expect_equal(s$optimal, rep(NA, 3))
  expect_equal(s$method, rep("heuristic", 3L))
  ## solution columns
  expect_equal(s$A1, c(TRUE, FALSE, FALSE))
  expect_equal(s$A2, c(FALSE, FALSE, FALSE))
  expect_equal(s$A3, c(TRUE, TRUE, FALSE))
  expect_equal(s$A4, rep(TRUE, 3L))
})

test_that("invalid arguments", {
  data(sim_project_data, sim_action_data, sim_species_data)
  # verify simulated data actually works
  expect_is(ppp_heuristic_spp_solution(sim_project_data, sim_action_data,
                                       sim_tree, 1000, "name", "success",
                                       "name", "cost", "name", "weight"),
                                       "tbl_df")
  # invalid budget
  expect_error(ppp_heuristic_spp_solution(
    sim_project_data, sim_action_data, sim_tree, NA_real_, "name", "success",
    "name", "cost", "name", "weight"))
  expect_error(ppp_heuristic_spp_solution(
    sim_project_data, sim_action_data, sim_tree, "A", "name", "success",
    "name", "cost", "name", "weight"))
  expect_error(ppp_heuristic_spp_solution(
    sim_project_data, sim_action_data, sim_tree, -5, "name", "success",
    "name", "cost", "name", "weight"))
  # invalid success
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$success[1] <- NA_real_
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost",
                               "name", "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$success[1] <- -1
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost",
                               "name", "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$success[1] <- 2
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$S1[1] <- NA_real_
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost",
                               "name", "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$S1[1] <- -1
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$S1[1] <- 2
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  # invalid costs
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$cost[1] <- NA_real_
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$cost[1] <- -5
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$cost <- as.character(sim_action_data$cost)
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight")
  })
  # locked in column
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$locked_in <- 5
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight", "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$locked_in[1] <- NA
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight", "locked_in")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$locked_in <- "T"
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight", "locked_in")
  })
  # locked out column
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$locked_out <- 5
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost",
                               "name", "weight", locked_out = "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$locked_out[1] <- NA
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight", locked_out = "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$locked_out <- "F"
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight", locked_out =  "locked_out")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_species_data)
    sim_action_data$locked_in <- TRUE
    sim_action_data$locked_out <- TRUE
    ppp_heuristic_spp_solution(sim_project_data, sim_action_data, sim_tree,
                               200, "name", "success", "name", "cost", "name",
                               "weight", "locked_in", "locked_out")
  })
})
