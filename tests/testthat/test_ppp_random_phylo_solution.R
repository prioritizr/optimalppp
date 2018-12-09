context("ppp_random_phylo_solution")

test_that("single solution, no constraints)", {
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
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_random_phylo_solution(project_data, action_data, tree, 0.18,
                                 "name", "success", "name", "cost")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1L)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0.18)
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_gt(s$cost, 0)
  expect_lte(s$cost, 0.18)
  expect_true(is.finite(s$cost))
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "random")
  ## solution columns
  expect_is(s$A1, "logical")
  expect_is(s$A2, "logical")
  expect_is(s$A3, "logical")
  expect_is(s$A4, "logical")
  expect_true(!anyNA(s$A1))
  expect_true(!anyNA(s$A2))
  expect_true(!anyNA(s$A3))
  expect_true(!anyNA(s$A4))
  expect_equal(s$A1 + s$A2 + s$A3, 1)
  expect_equal(s$A4, TRUE)
})

test_that("single solution, zero cost)", {
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
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_random_phylo_solution(project_data, action_data, tree, 0,
                                 "name", "success", "name", "cost")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1L)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0)
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_equal(s$cost, 0)
  expect_equal(s$budget, 0)
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "random")
  ## solution columns
  expect_equal(s$A1, FALSE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
})

test_that("single solution, locked in constraints)", {
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
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_random_phylo_solution(project_data, action_data, tree, 0.18,
                                 "name", "success", "name", "cost",
                                 locked_in_column_name = "locked_in")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1L)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0.18)
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_gt(s$cost, 0)
  expect_lte(s$cost, 0.18)
  expect_true(is.finite(s$cost))
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "random")
  ## solution columns
  expect_is(s$A1, "logical")
  expect_is(s$A2, "logical")
  expect_is(s$A3, "logical")
  expect_is(s$A4, "logical")
  expect_equal(s$A1, TRUE)
  expect_equal(s$A2, FALSE)
  expect_equal(s$A3, FALSE)
  expect_equal(s$A4, TRUE)
})

test_that("single solution, locked out constraints)", {
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
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_random_phylo_solution(project_data, action_data, tree, 0.18,
                                 "name", "success", "name", "cost",
                                 locked_out_column_name = "locked_out")
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 1L)
  ## statistic columns
  expect_equal(s$solution, 1L)
  expect_equal(s$budget, 0.18)
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_gt(s$cost, 0)
  expect_lte(s$cost, 0.18)
  expect_true(is.finite(s$cost))
  expect_equal(s$optimal, NA)
  expect_equal(s$method, "random")
  ## solution columns
  expect_is(s$A1, "logical")
  expect_is(s$A2, "logical")
  expect_is(s$A3, "logical")
  expect_is(s$A4, "logical")
  expect_equal(s$A2, FALSE)
  expect_equal(s$A1 + s$A3, 1)
  expect_equal(s$A4, TRUE)
})

test_that("multiple solutions, no constraints)", {
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
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_random_phylo_solution(project_data, action_data, tree, 0.18,
                                 "name", "success", "name", "cost",
                                  number_solutions = 100L)

  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 100)
  ## statistic columns
  expect_equal(s$solution, seq_len(100L))
  expect_equal(s$budget, rep(0.18, 100L))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_true(all(s$cost > 0))
  expect_true(all(s$cost < 0.18))
  expect_true(all(is.finite(s$cost)))
  expect_equal(s$optimal, rep(NA, 100))
  expect_equal(s$method, rep("random", 100))
  ## solution columns
  expect_is(s$A1, "logical")
  expect_is(s$A2, "logical")
  expect_is(s$A3, "logical")
  expect_is(s$A4, "logical")
  expect_gte(sum(s$A1), 5)
  expect_gte(sum(s$A2), 5)
  expect_gte(sum(s$A3), 5)
  expect_equal(s$A4, rep(TRUE, 100))
})

test_that("multiple solutions, locked in constraints)", {
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
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_random_phylo_solution(project_data, action_data, tree, 0.18,
                                 "name", "success", "name", "cost",
                                 locked_in_column_name = "locked_in",
                                 number_solutions = 100L)
  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 100)
  ## statistic columns
  expect_equal(s$solution, seq_len(100L))
  expect_equal(s$budget, rep(0.18, 100L))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_true(all(s$cost > 0))
  expect_true(all(s$cost <= 0.18))
  expect_true(all(is.finite(s$cost)))
  expect_equal(s$optimal, rep(NA, 100))
  expect_equal(s$method, rep("random", 100))
  ## solution columns
  expect_is(s$A1, "logical")
  expect_is(s$A2, "logical")
  expect_is(s$A3, "logical")
  expect_is(s$A4, "logical")
  expect_equal(s$A1, rep(TRUE, 100))
  expect_equal(s$A2, rep(FALSE, 100))
  expect_equal(s$A3, rep(FALSE, 100))
  expect_equal(s$A4, rep(TRUE, 100))
})

test_that("multiple solutions, locked out constraints)", {
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
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- ppp_random_phylo_solution(project_data, action_data, tree, 0.18,
                                 "name", "success", "name", "cost",
                                 locked_out_column_name = "locked_out",
                                 number_solutions = 100L)

  # tests
  ## class
  expect_is(s, "tbl_df")
  expect_equal(ncol(s), 10L)
  expect_equal(nrow(s), 100)
  ## statistic columns
  expect_equal(s$solution, seq_len(100L))
  expect_equal(s$budget, rep(0.18, 100L))
  expect_equal(s$obj, ppp_epd(project_data, action_data, tree,
                              s[, action_data$name], "name", "success",
                              "name"))
  expect_true(all(s$cost > 0))
  expect_true(all(s$cost <= 0.18))
  expect_true(all(is.finite(s$cost)))
  expect_equal(s$optimal, rep(NA, 100))
  expect_equal(s$method, rep("random", 100))
  ## solution columns
  expect_is(s$A1, "logical")
  expect_is(s$A2, "logical")
  expect_is(s$A3, "logical")
  expect_is(s$A4, "logical")
  expect_gte(sum(s$A1), 5)
  expect_equal(s$A2, rep(FALSE, 100))
  expect_gte(sum(s$A3), 5)
  expect_equal(s$A4, rep(TRUE, 100))
})
