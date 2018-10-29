context("ppp_plot")

test_that("some projects funded", {
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
  p <- ppp_plot(project_data, tree, "name", "cost", "success", s)
  # tests
  expect_is(p, "ggtree")
})

test_that("all projects funded", {
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
  p <- ppp_plot(project_data, tree, "name", "cost", "success", s)
  # tests
  expect_is(p, "ggtree")
})

test_that("no projects funded", {
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- tibble::tibble(a = FALSE, b = FALSE, c = FALSE, d = FALSE)
  p <- ppp_plot(project_data, tree, "name", "cost", "success", s)
  # tests
  expect_is(p, "ggtree")
})

test_that("invalid arguments", {
  # invalid column name arguments
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    ppp_plot(sim_project_data, sim_tree, "name1", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    ppp_plot(sim_project_data, sim_tree, "name", "cost1", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success1", s)
  })
  # invalid costs
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$cost[1] <- NA_real_
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$cost[1] <- -5
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$cost <- as.character(sim_project_data$cost)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  # invalid success
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$success[1] <- NA_real_
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$success[1] <- -1
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$success[1] <- 2
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$S1[1] <- NA_real_
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$S1[1] <- -1
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$S1[1] <- 2
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  # invalid solutions
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = NA, b = TRUE, c = TRUE, d = TRUE)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = 1, b = TRUE, c = TRUE, d = TRUE)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)

  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = "a", b = TRUE, c = TRUE, d = TRUE)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
  expect_error({
    data(sim_project_data, sim_tree)
    s <- tibble::tibble(a = TRUE, b = TRUE, d = TRUE)
    ppp_plot(sim_project_data, sim_tree, "name", "cost", "success", s)
  })
})
