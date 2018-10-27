context("ppp_simulate_data")

test_that("valid arguments", {
  s <- ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01, 0.4, 0.01,
                         0.01)
  # verify object structure
  expect_is(s, "list")
  expect_is(s$project_data, "tbl_df")
  expect_is(s$tree, "phylo")
  expect_equal(nrow(s$project_data), 101)
  expect_equal(ncol(s$project_data), 105)
  # name column
  expect_is(s$project_data$name, "character")
  expect_equal(anyDuplicated(s$project_data$name), 0)
  expect_equal(s$project_data$name[nrow(s$project_data)], "baseline_project")
  expect_equal(s$project_data$name[-nrow(s$project_data)],
               paste0("S", seq_len(100), "_project"))
  # cost column
  expect_is(s$project_data$cost, "numeric")
  expect_true(all(s$project_data$cost >= 0))
  expect_true(s$project_data$cost[nrow(s$project_data)] < 1e-16)
  expect_true(all(is.finite(s$project_data$cost)))
  # success column
  expect_is(s$project_data$success, "numeric")
  expect_true(all(s$project_data$success >= 0.7))
  expect_true(all(s$project_data$success[-nrow(s$project_data)] <= 0.99))
  expect_equal(s$project_data$success[nrow(s$project_data)], 1)
  expect_true(all(is.finite(s$project_data$success)))
  # locked in column
  expect_is(s$project_data$locked_in, "logical")
  expect_equal(sum(s$project_data$locked_in), 1L)
  expect_true(assertthat::noNA(s$project_data$locked_in))
  # locked out column
  expect_is(s$project_data$locked_out, "logical")
  expect_equal(sum(s$project_data$locked_out), 1L)
  expect_true(assertthat::noNA(s$project_data$locked_out))
  # species persistence columns
  expect_equal(unique(vapply(s$project_data[, grepl("S", names(s$project_data)),
                                            drop = FALSE], class,
                                            character(1))), "numeric")
  expect_equal(rowSums(as.matrix(s$project_data[, grepl("S",
                                                        names(s$project_data)),
                                                drop = FALSE]) > 0),
               c(rep(1, 100), 100))
  expect_gte(min(as(as.matrix(s$project_data[-101,
                                          grepl("S", names(s$project_data)),
                                          drop = FALSE]), "dgCMatrix")@x), 0.5)
  expect_lte(max(as.matrix(s$project_data[-101,
                                          grepl("S", names(s$project_data)),
                                          drop = FALSE])), 0.9)
  expect_gte(min(as.matrix(s$project_data[101,
                                          grepl("S", names(s$project_data)),
                                          drop = FALSE])), 0.01)
  expect_lte(max(as.matrix(s$project_data[101,
                                          grepl("S", names(s$project_data)),
                                          drop = FALSE])), 0.4)
})


test_that("invalid arguments", {
  # number species
  expect_error(ppp_simulate_data(-1, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(0.5, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(NA_integer_, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data("a", 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # cost mean
  expect_error(ppp_simulate_data(100, -1, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, NA_real_, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, "a", 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # cost sd
  expect_error(ppp_simulate_data(100, 100, -1, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, NA_real_, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, "a", 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # success_min_probability
  expect_error(ppp_simulate_data(100, 100, 5, -0.1, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 1.1, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, NA_real_, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, "a", 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # success_max_probability
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 1.1, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, -0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, "a", 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, NA_real_, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.6, 0.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # funded_min_persistence_probability
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, -0.1, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 1.5, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, "a", 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, NA_real_, 0.9, 0.01,
                                 0.4, 0.01, 0.01))
  # funded_max_persistence_probability
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, -0.9, 0.01, 0.4,
                                 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 1.9,
                                 0.01, 100, 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, "a", 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, NA_real_, 0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.3, 0.01,
                                 0.4, 0.01, 0.01))
  # not_funded_min_persistence_probability
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, -0.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 1.01,
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, "a",
                                 0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, NA_real_,
                                 0.4, 0.01, 0.01))
  # not_funded_max_persistence_probability
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 -0.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 1.4, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 NA_real_, 0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.004, 0.01, 0.01))
  # locked_in_proportion
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 1.1, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, -0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, NA_real_, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, "a", 0.01))
  # locked_out_proportion
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 1.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, -0.01, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, NA_real_, 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, "a", 0.01))
  expect_error(ppp_simulate_data(100, 100, 5, 0.7, 0.99, 0.5, 0.9, 0.01,
                                 0.4, 0.5, 0.6))
})
