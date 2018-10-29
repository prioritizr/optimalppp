#' @include internal.R
NULL

#' Plot a 'Project Prioritization Protocol' solution
#'
#' Create a plot displaying a phylogenetic tree (phylogram) to visualize a
#' solution to the 'Project Prioritization Protocol' problem. In this plot, each
#' phylogenetic branch is colored according to probability
#' that it is expected to persist into the future. Additionally, species that
#' benefit from at least a single funded project with a non-zero cost are
#' denoted with large labels.
#'
#' @inheritParams help
#'
#' @param n \code{integer} solution to display. This should argument
#'   correspond to a row number in the argument to \code{solution}. Defaults
#'   to \code{1} such that the solution in the first row of the argument
#'   to \code{solution} is plotted.
#'
#' @details This function requires the \pkg{ggtree} and \pkg{treeio} packages.
#'   Since these packages are distributed exclusively through
#'   \href{https://bioconductor.org}{Bioconductor}, and are not available on the
#'   \href{https://cran.r-project.org/}{Comprehensive R Archive Network},
#'   please execute the following commands to install them:
#'   \code{source("https://bioconductor.org/biocLite.R");biocLite("ggtree")}.
#'   If the installation process fails, please consult the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} and \href{https://bioconductor.org/packages/release/bioc/html/treeio.html}{\pkg{treeio}} packages' online documentation.
#'
#' @seealso To generate solutions for the 'Project
#'   Prioritization Protocol' problem, see \code{\link{ppp_heuristic_solution}}
#'   \code{\link{ppp_exact_solution}}, \code{\link{ppp_manual_solution}},
#'   or \code{\link{ppp_random_solution}}.
#'
#' @references
#' Yu, G., Smith, D. K., Zhu, H., Guan, Y., & Lam, T. T. Y. (2017). ggtree: An
#' R package for visualization and annotation of phylogenetic trees with their
#' covariates and other associated data. \emph{Methods in Ecology and
#' Evolution}, \strong{8}, 28--36.
#'
#' @examples
#' # load built-in data
#' data(sim_project_data, sim_tree)
#'
#' # load packages to help with plotting
#' library(ggplot2)
#'
#' # print simulated project data set
#' print(sim_project_data)
#'
#' # print simulated phylogenetic tree data set
#' print(sim_tree)
#'
#' # plot the simulated phylogeny
#' plot(sim_tree, main = "simulated phylogeny")
#'
#' # create some solutions, note that the column names the same as the values
#' # in the "name" column of the sim_project_data object
#' solutions <- data.frame(S1_project = c(FALSE, FALSE, TRUE),
#'                         S2_project = c(TRUE, FALSE, TRUE),
#'                         S4_project = c(TRUE, FALSE, TRUE),
#'                         S3_project = c(FALSE, FALSE, TRUE),
#'                         S5_project = c(TRUE, FALSE, TRUE),
#'                         baseline_project = c(TRUE, TRUE, TRUE))
#'
#' print(solutions)
#'
#' # evaluate the solutions
#' s1 <- ppp_manual_solution(sim_project_data, sim_tree, solutions,
#'                           "name", "cost", "success")
#'
#' # print output
#' print(s1)
#'
#' # plot the first solution
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success")
#'
#' # plot the second solution
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success", n = 2)
#'
#' # since this function returns a ggplot2 plot object, we can customize the
#' # appearance of the plot using standard ggplot2 commands!
#' # for example, we can add a title
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success") +
#' ggtitle("solution")
#'
#' # we could also also set the minimum and maximum values in the color ramp to
#' # correspond to those in the data, rather than being capped at 0 and 1
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success") +
#' scale_color_gradientn(name = "Probability of\npersistence",
#'                       colors = viridisLite::inferno(150, begin = 0,
#'                                                     end = 0.9,
#'                                                     direction = -1)) +
#' ggtitle("solution")
#'
#' # we could also change the color ramp
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success") +
#' scale_color_gradient(name = "Probability of\npersistence",
#'                      low = "red", high = "black") +
#' ggtitle("solution")
#'
#' # we could even hide the legend if desired
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success") +
#' scale_color_gradient(name = "Probability of\npersistence",
#'                      low = "red", high = "black") +
#' theme(legend.position = "hide") +
#' ggtitle("solution")
#' @export
ppp_plot <- function(x, tree, solution, project_column_name, cost_column_name,
                     success_column_name, n = 1L) {
  # assertions
  ## assert that ggtree R package is installed
  assertthat::assert_that(requireNamespace("ggtree", quietly = TRUE),
                          msg = "ggtree R package not installed.")
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
  ## coerce solution to tibble if just a regular data.frame
  if (inherits(solution, "data.frame") && !inherits(solution, "tbl_df"))
    solution <- tibble::as_tibble(solution)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, "tbl_df"),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(solution, "tbl_df"),
                          ncol(solution) > 0, nrow(solution) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.string(project_column_name),
                          assertthat::has_name(x, project_column_name),
                          assertthat::noNA(x[[project_column_name]]),
                          inherits(x[[project_column_name]],
                                   c("character", "factor")),
                          assertthat::is.string(cost_column_name),
                          assertthat::has_name(x, cost_column_name),
                          is.numeric(x[[cost_column_name]]),
                          assertthat::noNA(x[[cost_column_name]]),
                          assertthat::is.string(success_column_name),
                          assertthat::has_name(x, success_column_name),
                          is.numeric(x[[success_column_name]]),
                          assertthat::noNA(x[[success_column_name]]),
                          assertthat::is.count(n),
                          is.finite(n),
                          isTRUE(n <= nrow(x)))
  ## coerce factor species names to character
  if (is.factor(x[[project_column_name]]))
    x[[project_column_name]] <- as.character(x[[project_column_name]])

  ## additional checks
  assertthat::assert_that(all(assertthat::has_name(solution,
                                                   x[[project_column_name]])))
  assertthat::assert_that(all(vapply(solution[, x[[project_column_name]],
                                              drop = FALSE],
                                     class, character(1)) == "logical"))

  # preliminary data processing
  ## check that branches have lengths
  if (is.null(tree$edge.length)) {
    tree$edge.length <- rep(1, nrow(tree$edge))
    warning(paste("tree does not have branch length data,",
                  "all branches are assumed to have equal lengths"))
  }  else {
    assertthat::assert_that(nrow(tree$edge) == length(tree$edge.length))
  }

  ## subset solution and reorder columns
  solution <- solution[n, x[[project_column_name]], drop = FALSE]

  ## determine which species received funding
  funded_spp <- as.matrix(x[, tree$tip.label, drop = FALSE]) > 1e-15
  funded_spp[which(x[[cost_column_name]] < 1e-15), ] <- 0.0
  funded_projects <- as.numeric(as.matrix(solution))
  funded_projects <- matrix(funded_projects, ncol = ncol(funded_spp),
                            nrow = nrow(funded_spp))
  funded_spp <- tree$tip.label[colSums(funded_spp * funded_projects) > 1e-15]

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(methods::as(round(spp_probs, 5), "dgCMatrix"))
  ## pre-compute probabilities that each branch will persist
  branch_probs <- rcpp_branch_probabilities(spp_probs, branch_matrix(tree),
                                            methods::as(as.matrix(solution),
                                               "dgCMatrix"))

  # Main processing
  ## format tree data for plotting
  tree2 <- tidytree::as_data_frame(tree)
  tree2$status <- tree2$label %in% funded_spp
  tree2$status <- c("Not funded", "Funded")[tree2$status + 1]
  tree2$prob <- c(branch_probs)[match(
    paste0(tree2$parent, "_", tree2$node),
    paste0(tree$edge[, 1], "_", tree$edge[, 2]))]
  tree2$label <- paste0(" ", tree2$label)
  tree2 <- tidytree::as.treedata(tree2)

  ## make plot
  p <- ggtree::ggtree(tree2, ggplot2::aes_string(color = "prob"), size = 1.1) +
       ggtree::geom_tiplab(ggplot2::aes_string(size = "status"),
                           color = "black") +
       ggplot2::scale_color_gradientn(name = "Probability of\npersistence",
                                      colors = viridisLite::inferno(
                                        150, begin = 0, end = 0.9,
                                        direction = -1),
                                      limits = c(0, 1)) +
       ggplot2::scale_size_manual(name = "Species",
                                  values = c("Funded" = 4.5,
                                             "Not funded" = 2.5)) +
        ggplot2::theme(legend.position = "right")

  # Exports
  # return plot
  p
}
