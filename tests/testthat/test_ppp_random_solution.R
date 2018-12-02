context("ppp_random_solution")

# test_that("single solution, no constraints)", {
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10),
#                              stringsAsFactors = FALSE)
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- ppp_random_solution(project_data, tree, 0.18, "name",
#                            "cost", "success")
#   # tests
#   ## class
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 10)
#   expect_equal(nrow(s), 1)
#   ## statistic columns
#   expect_equal(s$solution, 1L)
#   expect_equal(s$budget, 0.18)
#   expect_equal(s$objective, ppp_objective_value(project_data, tree, "name",
#                                                 "success",
#                                                 s[, project_data$name]))
#   expect_gt(s$cost, 0)
#   expect_lte(s$cost, 0.18)
#   expect_true(is.finite(s$cost))
#   expect_equal(s$optimal, NA)
#   expect_equal(s$method, "random")
#   ## solution columns
#   expect_is(s$a, "logical")
#   expect_is(s$b, "logical")
#   expect_is(s$c, "logical")
#   expect_is(s$d, "logical")
#   expect_true(!anyNA(s$a))
#   expect_true(!anyNA(s$b))
#   expect_true(!anyNA(s$c))
#   expect_equal(s$a + s$b + s$c, 1)
#   expect_equal(s$d, TRUE)
# })
#
# test_that("single solution, zero cost)", {
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10),
#                              stringsAsFactors = FALSE)
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- ppp_random_solution(project_data, tree, 0, "name",
#                            "cost", "success")
#   # tests
#   ## class
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 10)
#   expect_equal(nrow(s), 1)
#   ## statistic columns
#   expect_equal(s$solution, 1L)
#   expect_equal(s$budget, 0)
#   expect_equal(s$objective, ppp_objective_value(project_data, tree, "name",
#                                                 "success",
#                                                 s[, project_data$name]))
#   expect_equal(s$cost, 0)
#   expect_equal(s$budget, 0)
#   expect_equal(s$optimal, NA)
#   expect_equal(s$method, "random")
#   ## solution columns
#   expect_equal(s$a, FALSE)
#   expect_equal(s$b, FALSE)
#   expect_equal(s$c, FALSE)
#   expect_equal(s$d, TRUE)
# })
#
# test_that("single solution, locked in constraints)", {
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10),
#                              locked_in = c(TRUE, FALSE, FALSE, FALSE),
#                              stringsAsFactors = FALSE)
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- ppp_random_solution(project_data, tree, 0.18, "name",
#                            "cost", "success",
#                            locked_in_column_name = "locked_in")
#   # tests
#   ## class
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 10)
#   expect_equal(nrow(s), 1)
#   ## statistic columns
#   expect_equal(s$solution, 1L)
#   expect_equal(s$budget, 0.18)
#   expect_equal(s$objective, ppp_objective_value(project_data, tree, "name",
#                                                 "success",
#                                                 s[, project_data$name]))
#   expect_gt(s$cost, 0)
#   expect_lte(s$cost, 0.18)
#   expect_true(is.finite(s$cost))
#   expect_equal(s$optimal, NA)
#   expect_equal(s$method, "random")
#   ## solution columns
#   expect_is(s$a, "logical")
#   expect_is(s$b, "logical")
#   expect_is(s$c, "logical")
#   expect_is(s$d, "logical")
#   expect_equal(s$a, TRUE)
#   expect_equal(s$b, FALSE)
#   expect_equal(s$c, FALSE)
#   expect_equal(s$d, TRUE)
# })
#
# test_that("single solution, locked out constraints)", {
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10),
#                              locked_out = c(TRUE, FALSE, FALSE, FALSE),
#                              stringsAsFactors = FALSE)
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- ppp_random_solution(project_data, tree, 0.18, "name",
#                            "cost", "success",
#                            locked_out_column_name = "locked_out")
#   # tests
#   ## class
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 10)
#   expect_equal(nrow(s), 1)
#   ## statistic columns
#   expect_equal(s$solution, 1L)
#   expect_equal(s$budget, 0.18)
#   expect_equal(s$objective, ppp_objective_value(project_data, tree, "name",
#                                                 "success",
#                                                 s[, project_data$name]))
#   expect_gt(s$cost, 0)
#   expect_lte(s$cost, 0.18)
#   expect_true(is.finite(s$cost))
#   expect_equal(s$optimal, NA)
#   expect_equal(s$method, "random")
#   ## solution columns
#   expect_is(s$a, "logical")
#   expect_is(s$b, "logical")
#   expect_is(s$c, "logical")
#   expect_is(s$d, "logical")
#   expect_equal(s$a, FALSE)
#   expect_equal(s$b + s$c, 1)
#   expect_equal(s$d, TRUE)
# })
#
# test_that("multiple solutions, no constraints)", {
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10),
#                              stringsAsFactors = FALSE)
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- ppp_random_solution(project_data, tree, 0.18, "name",
#                            "cost", "success", number_solutions = 100L)
#   # tests
#   ## class
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 10)
#   expect_equal(nrow(s), 100)
#   ## statistic columns
#   expect_equal(s$solution, seq_len(100L))
#   expect_equal(s$budget, rep(0.18, 100L))
#   expect_equal(s$objective, ppp_objective_value(project_data, tree, "name",
#                                                 "success",
#                                                 s[, project_data$name]))
#   expect_true(all(s$cost > 0))
#   expect_true(all(s$cost < 0.18))
#   expect_true(all(is.finite(s$cost)))
#   expect_equal(s$optimal, rep(NA, 100))
#   expect_equal(s$method, rep("random", 100))
#   ## solution columns
#   expect_is(s$a, "logical")
#   expect_is(s$b, "logical")
#   expect_is(s$c, "logical")
#   expect_is(s$d, "logical")
#   expect_gte(sum(s$a), 5)
#   expect_gte(sum(s$b), 5)
#   expect_gte(sum(s$c), 5)
#   expect_equal(s$d, rep(TRUE, 100))
# })
#
# test_that("multiple solutions, locked in constraints)", {
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10),
#                              locked_in = c(TRUE, FALSE, FALSE, FALSE),
#                              stringsAsFactors = FALSE)
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- ppp_random_solution(project_data, tree, 0.18, "name",
#                            "cost", "success",
#                            locked_in_column_name = "locked_in",
#                            number_solutions = 100L)
#   # tests
#   ## class
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 10)
#   expect_equal(nrow(s), 100)
#   ## statistic columns
#   expect_equal(s$solution, seq_len(100L))
#   expect_equal(s$budget, rep(0.18, 100L))
#   expect_equal(s$objective, ppp_objective_value(project_data, tree, "name",
#                                                 "success",
#                                                 s[, project_data$name]))
#   expect_true(all(s$cost > 0))
#   expect_true(all(s$cost <= 0.18))
#   expect_true(all(is.finite(s$cost)))
#   expect_equal(s$optimal, rep(NA, 100))
#   expect_equal(s$method, rep("random", 100))
#   ## solution columns
#   expect_is(s$a, "logical")
#   expect_is(s$b, "logical")
#   expect_is(s$c, "logical")
#   expect_is(s$d, "logical")
#   expect_equal(s$a, rep(TRUE, 100))
#   expect_equal(s$b, rep(FALSE, 100))
#   expect_equal(s$c, rep(FALSE, 100))
#   expect_equal(s$d, rep(TRUE, 100))
# })
#
# test_that("multiple solutions, locked out constraints)", {
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10),
#                              locked_out = c(TRUE, FALSE, FALSE, FALSE),
#                              stringsAsFactors = FALSE)
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- ppp_random_solution(project_data, tree, 0.18, "name",
#                            "cost", "success",
#                            locked_out_column_name = "locked_out",
#                            number_solutions = 100L)
#   # tests
#   ## class
#   expect_is(s, "tbl_df")
#   expect_equal(ncol(s), 10)
#   expect_equal(nrow(s), 100)
#   ## statistic columns
#   expect_equal(s$solution, seq_len(100L))
#   expect_equal(s$budget, rep(0.18, 100L))
#   expect_equal(s$objective, ppp_objective_value(project_data, tree, "name",
#                                                 "success",
#                                                 s[, project_data$name]))
#   expect_true(all(s$cost > 0))
#   expect_true(all(s$cost <= 0.18))
#   expect_true(all(is.finite(s$cost)))
#   expect_equal(s$optimal, rep(NA, 100))
#   expect_equal(s$method, rep("random", 100))
#   ## solution columns
#   expect_is(s$a, "logical")
#   expect_is(s$b, "logical")
#   expect_is(s$c, "logical")
#   expect_is(s$d, "logical")
#   expect_equal(s$a, rep(FALSE, 100))
#   expect_gte(sum(s$b), 5)
#   expect_gte(sum(s$c), 5)
#   expect_equal(s$d, rep(TRUE, 100))
# })
