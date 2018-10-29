context("ppp_manual_solution")

test_that("single solution", {
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10))
  m <- data.frame(a =  FALSE, b = FALSE, c = TRUE, d = TRUE)
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_manual_solution(project_data, tree, m, "name",
                           "cost", "success")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10)
  expect_equal(nrow(s), 1)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, NA_real_)
  expect_equal(s$objective,
               (100 * (1 - ((1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8))))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.94 * 0.8)) +
               (5 * (1.00 * 0.1)))
  expect_equal(s$cost, 0.15)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "manual")
  ## solution columns
  expect_equal(s$a, FALSE)
  expect_equal(s$b, FALSE)
  expect_equal(s$c, TRUE)
  expect_equal(s$d, TRUE)
})

test_that("multiple solutions", {
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10),
                             stringsAsFactors = FALSE)
  m <- data.frame(a = c(FALSE, FALSE),
                  b = c(FALSE, FALSE),
                  c = c(TRUE, FALSE),
                  d = c(TRUE, FALSE))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_manual_solution(project_data, tree, m, "name", "cost",
                           "success")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10)
  expect_equal(nrow(s), 2)
  ## solution columns
  expect_equal(as.data.frame(s[, c("a", "b", "c", "d")]), m)
  ## statistics columns
  expect_equal(s$solution, seq_len(2))
  expect_equal(s$budget, rep(NA_real_, nrow(s)))
  expect_equal(s$cost, c(0.15, 0))
  expect_equal(s$objective,
               c((100 * (1 - ((1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8))))) +
                (5 * (0.94 * 0.8)) +
                (5 * (0.94 * 0.8)) +
                (5 * (1.00 * 0.1)),
                0))
  expect_equal(s$optimal, rep(NA, 2))
  expect_equal(s$method, rep("manual", 2))
})

test_that("invalid arguments", {
  # invalid budget
  data(sim_project_data, sim_tree)
  m <- data.frame(a =  FALSE, b = FALSE, c = TRUE, d = TRUE)
  expect_error(ppp_manual_solution(sim_project_data, sim_tree, NA_real_, m,
                                   "name", "cost", "success"))
  expect_error(ppp_manual_solution(sim_project_data, sim_tree, "A", "cost",
                                   "name", "success"))
  expect_error(ppp_manual_solution(sim_project_data, sim_tree, -5, "cost",
                                   "name", "success"))
  # invalid costs
  m <- data.frame(a =  FALSE, b = FALSE, c = TRUE, d = TRUE)
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost[1] <- NA_real_
    ppp_manual_solution(sim_project_data, sim_tree, m, "name",
                        "cost", "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost[1] <- -5
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$cost <- as.character(sim_project_data$cost)
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  # invalid success
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- NA_real_
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- -1
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success[1] <- 2
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$success <- as.character(sim_project_data$success)
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  # invalid species probabilities
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- NA_real_
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- -1
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1[1] <- 2
    ppp_manual_solution(sim_project_data, sim_tree, 200, m, "name", "cost",
                        "success")
  })
  expect_error({
    data(sim_project_data, sim_tree)
    sim_project_data$S1 <- as.character(sim_project_data$S1)
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  # invalid solutions
  expect_error({
    m <- data.frame(a = 9, b = FALSE, c = TRUE, d = TRUE)
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    m <- data.frame(a = NA, b = FALSE, c = TRUE, d = TRUE)
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
  expect_error({
    m <- data.frame(a = "a", b = FALSE, c = TRUE, d = TRUE)
    ppp_manual_solution(sim_project_data, sim_tree, m, "name", "cost",
                        "success")
  })
})
