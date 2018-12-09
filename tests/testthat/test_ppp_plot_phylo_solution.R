context("ppp_phylo_plot")

test_that("some projects funded", {
  skip_if_not_installed("ggtree")
  # make data
  project_data <- tibble::tibble(name = letters[1:4],
                                 success =  c(0.95, 0.96, 0.94, 1.00),
                                 S1 =       c(0.91, 0.00, 0.80, 0.10),
                                 S2 =       c(0.00, 0.92, 0.80, 0.10),
                                 S3 =       c(0.00, 0.00, 0.00, 0.10),
                                 A1 =       c(TRUE, FALSE, TRUE, FALSE),
                                 A2 =       c(FALSE, TRUE, TRUE, FALSE),
                                 A3 =       c(FALSE, FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3"),
                                cost =      c(0.10, 0.10, 0))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  m <- tibble::tibble(
    name = paste0("solution_1"),
    A1 = c(FALSE),
    A2 = c(TRUE),
    A3 = c(TRUE))
  p <- ppp_plot_phylo_solution(project_data, action_data, tree, m, "name",
                               "success", "name", "cost")
  # tests
  expect_is(p, "ggtree")
})

test_that("all projects funded", {
  skip_if_not_installed("ggtree")
  # make data
  project_data <- tibble::tibble(name = letters[1:4],
                                 success =  c(0.95, 0.96, 0.94, 1.00),
                                 S1 =       c(0.91, 0.00, 0.80, 0.10),
                                 S2 =       c(0.00, 0.92, 0.80, 0.10),
                                 S3 =       c(0.00, 0.00, 0.00, 0.10),
                                 A1 =       c(TRUE, FALSE, TRUE, FALSE),
                                 A2 =       c(FALSE, TRUE, TRUE, FALSE),
                                 A3 =       c(FALSE, FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3"),
                                cost =      c(0.10, 0.10, 0))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  m <- tibble::tibble(
    name = paste0("solution_1"),
    A1 = c(TRUE),
    A2 = c(TRUE),
    A3 = c(TRUE))
  p <- ppp_plot_phylo_solution(project_data, action_data, tree, m, "name",
                               "success", "name", "cost")
  # tests
  expect_is(p, "ggtree")
})

test_that("no projects funded", {
  skip_if_not_installed("ggtree")
  # make data
  project_data <- tibble::tibble(name = letters[1:4],
                                 success =  c(0.95, 0.96, 0.94, 1.00),
                                 S1 =       c(0.91, 0.00, 0.80, 0.10),
                                 S2 =       c(0.00, 0.92, 0.80, 0.10),
                                 S3 =       c(0.00, 0.00, 0.00, 0.10),
                                 A1 =       c(TRUE, FALSE, TRUE, FALSE),
                                 A2 =       c(FALSE, TRUE, TRUE, FALSE),
                                 A3 =       c(FALSE, FALSE, FALSE, TRUE))
  action_data <- tibble::tibble(name =      c("A1", "A2", "A3"),
                                cost =      c(0.10, 0.10, 0))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  m <- tibble::tibble(
    name = paste0("solution_1"),
    A1 = c(FALSE),
    A2 = c(FALSE),
    A3 = c(TRUE))
  p <- ppp_plot_phylo_solution(project_data, action_data, tree, m, "name",
                               "success", "name", "cost")
  # tests
  expect_is(p, "ggtree")
})

test_that("invalid arguments", {
  # invalid n argument
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost", 0)
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost", 12)
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost", 0.5)
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost", NA_integer_)
  })
  # invalid column name arguments
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name1", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success1", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name1", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost1")
  })
  # invalid costs
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_action_data$cost[1] <- NA_real_
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_action_data$cost[1] <- -5
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_action_data$cost <- "4"
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  # invalid success
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$success[1] <- NA_real_
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$success[1] <- -1
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$success[1] <- 2
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$S1[1] <- NA_real_
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$S1[1] <- -1
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$S1[1] <- 2
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = TRUE, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  # invalid solutions
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = "F", S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = 8, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S1_action = NA, S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
  expect_error({
    data(sim_project_data, sim_action_data, sim_tree)
    s <- tibble::tibble(S2_action = TRUE, S3_action = TRUE,
                        S4_action = TRUE, S5_action = TRUE,
                        baseline_action = TRUE)
    ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s,
                            "name", "success", "name", "cost")
  })
})
