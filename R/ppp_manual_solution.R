#' @include internal.R
NULL

#' Solve the 'Project Prioritization Protocol' problem by manually specifying
#' a solution
#'
#' Manually specify funding schemes for conservation projects and calculate
#' their effectiveness.
#'
#' @inheritParams help
#'
#' @inherit help return
#'
#' @details The effectiveness of different funding schemes is calculated
#'   following the "expected phylogenetic diversity" metric (Faith 2008).
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem, see \code{\link{ppp_gurobi_solution}},
#'   \code{\link{ppp_heuristic_solution}}, and
#'   \code{\link{ppp_random_solution}}. To visualize the effectiveness of a
#'   particular solution, see \code{\link{ppp_plot}}.
#'
#' @references
#' Faith, D. P. (2008). Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}, 1461--1470.
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
#' # print the output
#' print(s1)
#'
#' # visualize the effectiveness of the different solutions
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success",
#'          n = 1) + ggtitle("solution 1")
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success",
#'          n = 2) + ggtitle("solution 2")
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success",
#'          n = 3) + ggtitle("solution 3")
#' @export
ppp_manual_solution <- function(x, tree, solution,
                                project_column_name,
                                cost_column_name,
                                success_column_name) {
  # assertions
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
                          assertthat::noNA(x[[success_column_name]]))
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

  # main processing
  ## format statistics for output
  s <- solution
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      solution = seq_len(nrow(solution)),
      objective = ppp_objective_value(x, tree, project_column_name,
                                      success_column_name, s),
      budget = NA_real_,
      cost = rowSums(matrix(x[[cost_column_name]], byrow = TRUE,
                            ncol = ncol(s), nrow = nrow(s)) *
                     as.matrix(s[, x[[project_column_name]], drop = FALSE])),
      optimal = NA,
      method = "manual"),
    s[, x[[project_column_name]], drop = FALSE]))

  # return result
  out
}
