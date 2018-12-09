context("ppp_manual_phylo_solution")

test_that("single solution", {
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
  m <- tibble::tibble(
    name = paste0("solution_1"),
    A1 = c(FALSE),
    A2 = c(TRUE),
    A3 = c(TRUE),
    A4 = c(TRUE))
  s <- ppp_manual_phylo_solution(project_data, action_data, tree, m, "name",
                                 "success", "name", "cost")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 11)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$method, "manual")
  expect_equal(s$budget, NA_real_)
  expect_equal(s$epd,
               (100 * (1 - ((1 - (0.94 * 0.8)) * (1 - (0.96 * 0.92))))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.96 * 0.92)) +
               (5 * (1.00 * 0.1)))
  expect_equal(s$er,
               (0.94 * 0.8) +
               (0.96 * 0.92) +
               (1.00 * 0.1))
  expect_equal(s$cost, 0.25)
  expect_equal(s$optimal, NA)
  ## solution columns
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, TRUE)
  expect_equal(s$A3, TRUE)
  expect_equal(s$A4, TRUE)
})

test_that("multiple solutions", {
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
  m <- tibble::tibble(
    name = paste0("solution_", seq_len(2)),
    A1 = c(TRUE, FALSE),
    A2 = c(FALSE, TRUE),
    A3 = c(FALSE, TRUE),
    A4 = c(TRUE, TRUE))
  s <- ppp_manual_phylo_solution(project_data, action_data, tree, m, "name",
                                 "success", "name", "cost")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 11)
  expect_equal(nrow(s), 2)
  ## statistics
  expect_equal(s$solution, seq_len(2))
  expect_equal(s$method, rep("manual", 2))
  expect_equal(s$budget, rep(NA_real_, nrow(s)))
  expect_equal(s$cost, c(0.1, 0.25))
  expect_equal(s$epd,
               c((100 * (1 - (1 - (0.95 * 0.91)) * (1 - (1 * 0.1)))) +
                 (5 * (0.95 * 0.91)) +
                 (5 * (1 * 0.1)) +
                 (5 * (1 * 0.1)),
                 (100 * (1 - (1 - (0.94 * 0.8)) * (1 - (0.96 * 0.92)))) +
                 (5 * (0.94 * 0.8)) +
                 (5 * (0.96 * 0.92)) +
                 (5 * (1 * 0.1))))
  expect_equal(s$optimal, rep(NA, 2))
  ## solution columns
  expect_equal(s[, c("A1", "A2", "A3", "A4")], m[, -1])
})

test_that("invalid arguments", {
  # invalid budget
  data(sim_project_data, sim_action_data, sim_tree)
  m <- data.frame(S1_action =  FALSE, S2_action = FALSE, S3_action = TRUE,
                  S4_action = TRUE, S5_action = FALSE, baseline_action = TRUE)
  expect_is(ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                                      sim_tree, m, "name", "success", "name",
                                      "cost"), "tbl_df")
  # invalid costs
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$cost[1] <- NA_real_
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$cost[1] <- -5
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_action_data$cost <- as.character(sim_action_data$cost)
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  # invalid success
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success[1] <- NA_real_
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success[1] <- -1
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success[1] <- 2
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1[1] <- NA_real_
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1[1] <- -1
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1[1] <- 2
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  # invalid solutions
  expect_error({
    m <- data.frame(S1_action = "T", S2_action = FALSE, S3_action = TRUE,
                    S4_action = TRUE, S5_action = FALSE, baseline_action = TRUE)
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    m <- data.frame(S1_action = NA, S2_action = FALSE, S3_action = TRUE,
                    S4_action = TRUE, S5_action = FALSE, baseline_action = TRUE)
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    m <- data.frame(S1_action = -1, S2_action = FALSE, S3_action = TRUE,
                    S4_action = TRUE, S5_action = FALSE, baseline_action = TRUE)
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
  expect_error({
    m <- data.frame(S2_action = FALSE, S3_action = TRUE,
                    S4_action = TRUE, S5_action = FALSE, baseline_action = TRUE)
    ppp_manual_phylo_solution(sim_project_data, sim_action_data,
                              sim_tree, m, "name", "success", "name", "cost")
  })
})
