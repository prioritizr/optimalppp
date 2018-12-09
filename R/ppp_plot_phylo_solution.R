#' @include internal.R
NULL

#' Plot a 'Project Prioritization Protocol' solution with phylogenetic data
#'
#' Create a plot displaying a phylogenetic tree (phylogram) to visualize a
#' solution to the 'Project Prioritization Protocol' problem
#' (Joseph, Maloney & Possingham 2009) with phlyogenetic data.
#'
#' @inheritParams help
#'
#' @param n \code{integer} solution to display. This should argument
#'   correspond to a row number in the argument to \code{solution}. Defaults
#'   to \code{1} such that the solution in the first row of the argument
#'   to \code{solution} is plotted.
#'
#' @param symbol_hjust \code{numeric} horizontal adjustment parameter to
#'   manually align the asterisks and dashes in the plot. Defaults to
#'   \code{0.007}. Increasing this parameter will shift the symbols further
#'   left towards the species labels.
#'
#' @details This function requires the \pkg{ggtree} (Yu \emph{et al.} 2017).
#'   Since this package is distributed exclusively
#'   through \href{https://bioconductor.org}{Bioconductor}, and is not
#'   available on the
#'   \href{https://cran.r-project.org/}{Comprehensive R Archive Network},
#'   please execute the following command to install it:
#'   \code{source("https://bioconductor.org/biocLite.R");biocLite("ggtree")}.
#'   If the installation process fails, please consult the package's \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{online documentation}.
#'
#'   Here, each phylogenetic branch is colored according to probability
#'   that it is expected to persist into the future (based on Faith 2008).
#'   Additionally, species that directly benefit from at least a single
#'   completely funded project with a non-zero cost are denoted with an
#'   asterisk symbol. Species that indirectly benefit from funded
#'   projects---because they are associated with partially funded projects that
#'   have non-zero costs and share actions with at least one funded
#'   project---are denoted with an open circle symbols.
#'
#' @seealso To generate solutions for the 'Project
#'   Prioritization Protocol' problem, see
#'   \code{\link{ppp_heuristic_phylo_solution}}
#'   \code{\link{ppp_exact_phylo_solution}},
#'   \code{\link{ppp_manual_phylo_solution}},
#'   or \code{\link{ppp_random_phylo_solution}}.
#'
#' @references
#' Faith DP (2008) Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}: 1461--1470.
#'
#' Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
#' resources among threatened species: A project prioritization protocol.
#' \emph{Conservation Biology}, \strong{23}, 328--338.
#'
#' Yu G, Smith DK, Zhu H, Guan Y, & Lam TTY (2017) ggtree: an
#' R package for visualization and annotation of phylogenetic trees with their
#' covariates and other associated data. \emph{Methods in Ecology and
#' Evolution}, \strong{8}: 28--36.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
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
#'
#' solutions <- data.frame(S1_project = c(FALSE, FALSE, TRUE),
#'                         S2_project = c(TRUE, FALSE, TRUE),
#'                         S4_project = c(TRUE, FALSE, TRUE),
#'                         S3_project = c(FALSE, FALSE, TRUE),
#'                         S5_project = c(TRUE, FALSE, TRUE),
#'                         baseline_project = c(TRUE, TRUE, TRUE))
#'
#' print(solutions)
#'
#' # create random some solutions with a budget of 700
#' s1 <- ppp_random_solution(sim_project_data, sim_tree, 700,
#'                           "name", "cost", "success", number_solutions = 10)
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
ppp_plot_phylo_solution <- function(x, y, tree, solution, project_column_name,
                                    success_column_name, action_column_name,
                                    cost_column_name, n = 1L,
                                    symbol_hjust = 0.007) {
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
  ## coerce x to tibble if just a regular data.frame
  if (inherits(y, "data.frame") && !inherits(y, "tbl_df"))
    y <- tibble::as_tibble(y)
  ## coerce solution to tibble if just a regular data.frame
  if (inherits(solution, "data.frame") && !inherits(solution, "tbl_df"))
    solution <- tibble::as_tibble(solution)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, "tbl_df"),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(y, "tbl_df"),
                          ncol(y) > 0, nrow(y) > 0,
                          inherits(solution, "tbl_df"),
                          ncol(solution) > 0, nrow(solution) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.string(project_column_name),
                          assertthat::has_name(x, project_column_name),
                          assertthat::noNA(x[[project_column_name]]),
                          inherits(x[[project_column_name]],
                                   c("character", "factor")),
                          assertthat::is.string(success_column_name),
                          assertthat::has_name(x, success_column_name),
                          is.numeric(x[[success_column_name]]),
                          assertthat::noNA(x[[success_column_name]]),
                          all(x[[success_column_name]] >= 0),
                          all(x[[success_column_name]] <= 1),
                          assertthat::is.string(action_column_name),
                          assertthat::has_name(y, action_column_name),
                          assertthat::noNA(y[[action_column_name]]),
                          inherits(y[[action_column_name]],
                                   c("character", "factor")),
                          all(as.character(y[[action_column_name]]) %in%
                              names(x)),
                          assertthat::is.string(cost_column_name),
                          assertthat::has_name(y, cost_column_name),
                          is.numeric(y[[cost_column_name]]),
                          assertthat::noNA(y[[cost_column_name]]),
                          all(y[[cost_column_name]] >= 0),
                          assertthat::is.count(n),
                          is.finite(n),
                          isTRUE(n <= nrow(solution)),
                          assertthat::is.number(symbol_hjust),
                          is.finite(symbol_hjust))
  ## coerce factor project names to character
  if (is.factor(x[[project_column_name]]))
    x[[project_column_name]] <- as.character(x[[project_column_name]])
  ## coerce factor action names to character
  if (is.factor(y[[action_column_name]]))
    y[[action_column_name]] <- as.character(y[[action_column_name]])
  ## additional checks
  assertthat::assert_that(all(assertthat::has_name(solution,
                                                   y[[action_column_name]])))
  assertthat::assert_that(is.logical(as.matrix(
    solution[, y[[action_column_name]], drop = FALSE])))
  assertthat::assert_that(all(!is.na(as.matrix(
    solution[, y[[action_column_name]], drop = FALSE]))))
  assertthat::assert_that(is.logical(as.matrix(
    x[, y[[action_column_name]], drop = FALSE])))
  assertthat::assert_that(all(!is.na(as.matrix(
    x[, y[[action_column_name]], drop = FALSE]))))
  assertthat::assert_that(is.numeric(as.matrix(
    x[, tree$tip.label, drop = FALSE])))
  assertthat::assert_that(assertthat::noNA(
    as.matrix(x[, tree$tip.label, drop = FALSE])))
  assertthat::assert_that(min(as.matrix(x[, tree$tip.label,
                                          drop = FALSE])) >= 0)
  assertthat::assert_that(max(as.matrix(x[, tree$tip.label,
                                          drop = FALSE])) <= 1)
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
  solution <- solution[n, y[[action_column_name]], drop = FALSE]

  ## determine which projects are funded based on the funded actions
  ## and omit the baseline project
  prj <- as.matrix(x[, y[[action_column_name]], drop = FALSE])
  funding_matrix <- matrix(TRUE, ncol = nrow(y), nrow = nrow(x))
  pos <- which(prj > 0.5, arr.ind = TRUE)
  funding_matrix[pos] <- as.matrix(solution)[1, pos[, 2]]
  funded_projects <- rowSums(funding_matrix) == ncol(prj)
  partially_funded_projects <- (rowSums(funding_matrix) > 0) &
                               (rowSums(!funding_matrix ) < rowSums(prj)) &
                               (rowSums(funding_matrix) != ncol(prj))

  ## determine baseline project(s)
  zero_cost_projects <- rowSums(
    as.matrix(x[, y[[action_column_name]], drop = FALSE]) *
    matrix(y[[cost_column_name]], byrow = TRUE, ncol = nrow(y), nrow = nrow(x)))
  zero_cost_projects <- zero_cost_projects < 1e-15

  ## determine which species receive funding based on their project being funded
  completely_funded_spp <- as.matrix(x[, tree$tip.label, drop = FALSE]) > 1e-15
  completely_funded_spp[!funded_projects, ] <- 0.0
  completely_funded_spp[zero_cost_projects, ] <- 0.0
  completely_funded_spp <- colSums(completely_funded_spp) > 1e-15
  completely_funded_spp <- tree$tip.label[completely_funded_spp]

  ## determine which species receive indirect based on sharing actions with
  ## a funded project
  partially_funded_spp <- as.matrix(x[, tree$tip.label, drop = FALSE]) > 1e-15
  partially_funded_spp[!partially_funded_projects, ] <- 0.0
  partially_funded_spp[zero_cost_projects, ] <- 0.0
  partially_funded_spp <- colSums(partially_funded_spp) > 1e-15
  partially_funded_spp <- setdiff(tree$tip.label[partially_funded_spp],
                                  completely_funded_spp)

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(methods::as(spp_probs, "dgCMatrix"))

  ## pre-compute probabilities that each branch will persist
  branch_probs <- rcpp_branch_probabilities(spp_probs, branch_matrix(tree),
                                            methods::as(as.matrix(solution),
                                               "dgCMatrix"))

  # Main processing
  ## format tree data for plotting
  tree2 <- suppressMessages(suppressWarnings(tidytree::as_tibble(tree)))
  tree2$status <- NA_character_
  tree2$status[tree2$label %in% completely_funded_spp] <- "Funded"
  tree2$status[tree2$label %in% partially_funded_spp] <- "Partially Funded"
  tree2$prob <- c(branch_probs)[match(
    paste0(tree2$parent, "_", tree2$node),
    paste0(tree$edge[, 1], "_", tree$edge[, 2]))]
  tree2$label <- paste0("   ", tree2$label)
  tree2 <- suppressMessages(suppressWarnings(tidytree::as.treedata(tree2)))

  ## calculate padding for points
  point_padding <- max(rowSums(as.matrix(branch_matrix(tree)) *
                               matrix(tree$edge.length, ncol = nrow(tree$edge),
                                      nrow = length(tree$tip.label)))) *
                   symbol_hjust
  ## make plot
  p <- ggtree::ggtree(tree2, ggplot2::aes_string(color = "prob"), size = 1.1) +
       ggtree::geom_tippoint(
         ggplot2::aes_string(x = "x + point_padding", subset = "!is.na(status)",
                             shape = "status"), color = "black") +
       ggtree::geom_tiplab(color = "black", size = 2.5) +
       ggplot2::scale_color_gradientn(name = "Probability of\npersistence",
                                      colors = viridisLite::inferno(
                                        150, begin = 0, end = 0.9,
                                        direction = -1),
                                      limits = c(0, 1)) +
       ggplot2::scale_shape_manual(name = "Projects",
                                   values = c("Funded" = 8,
                                              "Partially Funded" = 1),
                                   na.translate = FALSE) +
       ggplot2::theme(legend.position = "right")

  # Exports
  # return plot
  p
}
