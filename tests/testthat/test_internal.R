context("internal functions")

test_that("ppp_epd", {
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
  solution <- tibble::tibble(
    name = paste0("solution_", seq_len(4)),
    A1 = c(FALSE, TRUE, FALSE, FALSE),
    A2 = c(FALSE, FALSE, TRUE, TRUE),
    A3 = c(FALSE, FALSE, FALSE, TRUE),
    A4 = c(FALSE, FALSE, TRUE, TRUE))
  o <- ppp_epd(project_data, action_data, tree, solution,
               "name", "success", "name")
  expect_equal(o[1],
               (100 * (1 - (1 - (0 * 0)) * (1 - (0 * 0)))) +
               (5 * (0 * 0)) +
               (5 * (0 * 0)) +
               (5 * (0 * 0)))
  expect_equal(o[2],
               (100 * (1 - (1 - (0.95 * 0.91)) * (1 - (0 * 0)))) +
               (5 * (0.95 * 0.91)) +
               (5 * (0 * 0)) +
               (5 * (0 * 0)))
  expect_equal(o[3],
               (100 * (1 - (1 - (1.0 * 0.1)) * (1 - (0.96 * 0.92)))) +
               (5 * (1.0 * 0.1)) +
               (5 * (0.96 * 0.92)) +
               (5 * (1.0 * 0.1)))
  expect_equal(o[4],
               (100 * (1 - (1 - (0.94 * 0.8)) * (1 - (0.96 * 0.92)))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.96 * 0.92)) +
               (5 * (1 * 0.1)))
})
