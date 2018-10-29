context("internal functions")

test_that("ppp_objective_value", {
  project_data <- tibble::tibble(name = letters[1:4],
                                 cost =     c(0.10, 0.10, 0.15, 0.00),
                                 success =  c(0.95, 0.96, 0.94, 1.00),
                                 S1 =       c(0.91, 0.00, 0.80, 0.10),
                                 S2 =       c(0.00, 0.92, 0.80, 0.10),
                                 S3 =       c(0.00, 0.00, 0.00, 0.10))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  solution <- tibble::tibble(
    name = paste0("solution_", seq_len(3)),
    a = c(FALSE, TRUE, TRUE),
    b = c(FALSE, FALSE, FALSE),
    c = c(TRUE, FALSE, FALSE),
    d = c(FALSE, FALSE, TRUE))
  o <- ppp_objective_value(project_data, tree, "name", "success", solution)
  expect_equal(o[1],
               (100 * (1 - (1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8)))) +
               (5 * (0.94 * 0.8)) +
               (5 * (0.94 * 0.8)) +
               (5 * (0 * 0)))
  expect_equal(o[2],
               (100 * (1 - (1 - (0.95 * 0.91)) * (1 - (0 * 0)))) +
               (5 * (0.95 * 0.91)) +
               (5 * (0 * 0)) +
               (5 * (0 * 0)))
  expect_equal(o[3],
               (100 * (1 - (1 - (0.95 * 0.91)) * (1 - (1 * 0.1)))) +
               (5 * (0.95 * 0.91)) +
               (5 * (1 * 0.1)) +
               (5 * (1 * 0.1)))
})

test_that("rcpp_branch_probabilities", {
  # create data
  project_data <- data.frame(name = letters[1:4],
                             cost =     c(0.10, 0.10, 0.15, 0.00),
                             success =  c(0.95, 0.96, 0.94, 1.00),
                             S1 =       c(0.91, 0.00, 0.80, 0.10),
                             S2 =       c(0.00, 0.92, 0.80, 0.10),
                             S3 =       c(0.00, 0.00, 0.00, 0.10))
  tree <- ape::read.tree(text = "((S1,S2),S3);")
  tree$edge.length <- c(100, 5, 5, 5)
  s <- tibble::tibble(a = FALSE, b = FALSE, c = TRUE, d = TRUE)
  s <- rbind(s, tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE))
  s <- rbind(s, tibble::tibble(a = FALSE, b = FALSE, c = FALSE, d = FALSE))
  # format data for low-level Rcpp function
  spp_probs <- as.matrix(project_data[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(project_data$success,
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(as(round(spp_probs, 5), "dgCMatrix"))
  # calculate probabilities
  p <- rcpp_branch_probabilities(spp_probs, branch_matrix(tree),
                                 as(as.matrix(s), "dgCMatrix"))
  # tests
  ## object structure
  expect_is(p, "matrix")
  expect_is(p[1], "numeric")
  expect_equal(nrow(p), 3)
  expect_equal(ncol(p), 4)
  ## verify calculations
  ### solution with some projects funded
  expect_equal(p[1, 1], 1 - (1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8)))
  expect_equal(p[1, 2], 0.94 * 0.8)
  expect_equal(p[1, 3], 0.94 * 0.8)
  expect_equal(p[1, 4], 1 * 0.1)

  ### solution with all projects funded
  expect_equal(p[2, 1], 1 - (1 - (0.95 * 0.91)) * (1 - (0.96 * 0.92)))
  expect_equal(p[2, 2], 0.95 * 0.91)
  expect_equal(p[2, 3], 0.96 * 0.92)
  expect_equal(p[2, 4], 1 * 0.1)

  ### solution with no projects funded
  expect_equal(p[3, ], rep(0, 4))
})
