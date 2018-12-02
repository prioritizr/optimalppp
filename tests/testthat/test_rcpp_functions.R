context("Rcpp functions")

# test_that("rcpp_mip_formulation", {
#   # load data
#   source("functions.R")
#   data(sim_project_data, sim_tree)
#   budget <- sum(sim_project_data$cost) * 0.25
#   spp_probs <- as.matrix(sim_project_data[, sim_tree$tip.label, drop = FALSE])
#   spp_probs <- spp_probs * matrix(sim_project_data$success,
#                                   ncol = ncol(spp_probs),
#                                   nrow = nrow(spp_probs))
#   spp_probs <- Matrix::drop0(as(spp_probs, "dgCMatrix"))
#   # build formulation
#   f1 <- rcpp_mip_formulation(
#           spp = spp_probs,
#           budget = budget,
#           branch_matrix = branch_matrix(sim_tree),
#           branch_lengths = sim_tree$edge.length,
#           costs = sim_project_data$cost,
#           locked_in = which(sim_project_data$locked_in),
#           locked_out = which(sim_project_data$locked_out),
#           n_approx_points = 5)
#   f2 <- r_mip_formulation(
#           project_data = sim_project_data,
#           tree = sim_tree,
#           budget = budget,
#           n_approx_points = 5)
#   f1$A <- Matrix::sparseMatrix(i = f1$Ai, j = f1$Aj, x = f1$Ax, index1 = FALSE)
#   # run tests
#   expect_equal(f1$modelsense, f2$modelsense)
#   expect_equivalent(f1$obj, f2$obj)
#   expect_equal(f1$rhs, f2$rhs)
#   expect_equal(f1$sense, f2$sense)
#   expect_equal(length(f1$pwl), length(f2$pwl))
#   for (i in seq_along(f1$pwl)) {
#     expect_equal(f1$pwl[[1]]$var, f2$pwl[[1]]$var)
#     expect_true(max(abs(f1$pwl[[1]]$x - f2$pwl[[1]]$x)) < 1e-10)
#     expect_true(max(abs(f1$pwl[[1]]$y - f2$pwl[[1]]$y)) < 1e-10)
#   }
# })
#
# test_that("rcpp_branch_probabilities", {
#   # create data
#   project_data <- data.frame(name = letters[1:4],
#                              cost =     c(0.10, 0.10, 0.15, 0.00),
#                              success =  c(0.95, 0.96, 0.94, 1.00),
#                              S1 =       c(0.91, 0.00, 0.80, 0.10),
#                              S2 =       c(0.00, 0.92, 0.80, 0.10),
#                              S3 =       c(0.00, 0.00, 0.00, 0.10))
#   tree <- ape::read.tree(text = "((S1,S2),S3);")
#   tree$edge.length <- c(100, 5, 5, 5)
#   s <- tibble::tibble(a = FALSE, b = FALSE, c = TRUE, d = TRUE)
#   s <- rbind(s, tibble::tibble(a = TRUE, b = TRUE, c = TRUE, d = TRUE))
#   s <- rbind(s, tibble::tibble(a = FALSE, b = FALSE, c = FALSE, d = FALSE))
#   # format data for low-level Rcpp function
#   spp_probs <- as.matrix(project_data[, tree$tip.label, drop = FALSE])
#   spp_probs <- spp_probs * matrix(project_data$success, byrow = FALSE,
#                                   ncol = ncol(spp_probs),
#                                   nrow = nrow(spp_probs))
#   spp_probs <- Matrix::drop0(as(spp_probs, "dgCMatrix"))
#   # calculate probabilities
#   p <- rcpp_branch_probabilities(spp_probs, branch_matrix(tree),
#                                  as(as.matrix(s), "dgCMatrix"))
#   # tests
#   ## object structure
#   expect_is(p, "matrix")
#   expect_is(p[1], "numeric")
#   expect_equal(nrow(p), 3)
#   expect_equal(ncol(p), 4)
#   ## verify calculations
#   ### solution with some projects funded
#   expect_equal(p[1, 1], 1 - (1 - (0.94 * 0.8)) * (1 - (0.94 * 0.8)))
#   expect_equal(p[1, 2], 0.94 * 0.8)
#   expect_equal(p[1, 3], 0.94 * 0.8)
#   expect_equal(p[1, 4], 1 * 0.1)
#
#   ### solution with all projects funded
#   expect_equal(p[2, 1], 1 - (1 - (0.95 * 0.91)) * (1 - (0.96 * 0.92)))
#   expect_equal(p[2, 2], 0.95 * 0.91)
#   expect_equal(p[2, 3], 0.96 * 0.92)
#   expect_equal(p[2, 4], 1 * 0.1)
#
#   ### solution with no projects funded
#   expect_equal(p[3, ], rep(0, 4))
# })
