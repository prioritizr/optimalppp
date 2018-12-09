#' @include internal.R
NULL

#' Manually prioritize conservation projects with phylogenetic data
#'
#' Manually specify funding schemes for conservation projects using the
#' 'Project Prioritization Protocol' (Joseph, Maloney & Possingham 2009), and
#' evaluate their effectiveness using 'expected phylogenetic diversity'
#' (Faith 2008).
#'
#' @inheritParams help
#'
#' @inherit help return
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem, see
#'   \code{\link{ppp_exact_phylo_solution}},
#'   \code{\link{ppp_heuristic_phylo_solution}}, and
#'   \code{\link{ppp_random_phylo_solution}}. To visualize the effectiveness of
#'   a particular solution, see \code{\link{ppp_phylo_plot}}.
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
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load built-in data
#' data(sim_project_data, sim_action_data, sim_tree)
#'
#' # load packages to help with plotting
#' library(ggplot2)
#'
#' # print simulated project data
#' print(sim_project_data)
#'
#' # print simulated action data
#' print(sim_action_data)
#'
#' # print simulated phylogenetic tree data
#' print(sim_tree)
#'
#' # plot the simulated phylogeny
#' plot(sim_tree, main = "simulated phylogeny")
#'
#' # now we will create three solutions
#' # first, we will initialize a data.frame with no actions funded
#' solutions <- as.data.frame(matrix(FALSE, ncol = nrow(sim_action_data),
#'                            nrow = 3))
#' names(solutions) <- sim_action_data$name
#'
#' # the first solution will have no actions funded except for the base line
#' # actions, so we will make the first value in that column TRUE
#' solutions$baseline_action[1] <- TRUE
#'
#' # the second solution will have all actions funded, so we will set those
#' # values to TRUE
#' solutions[2, ] <- TRUE
#'
#' # the third solution will have four randomly selected actions funded
#' solutions[3, sample.int(nrow(sim_action_data), 4)] <- TRUE
#'
#' # now we can evaluate the solutions
#' s1 <- ppp_manual_phylo_solution(sim_project_data, sim_action_data, sim_tree,
#'                                 solutions, "name", "cost", "success")
#'
#' # print the output
#' print(s1)
#'
#' # visualize the effectiveness of the different solutions
#' ppp_plot(sim_project_data, sim_action_data, sim_tree, s1, "name", "cost",
#'          "success", n = 1) + ggtitle("solution 1")
#' ppp_plot(sim_project_data, sim_action_data, sim_tree, s1, "name", "cost",
#'          "success", n = 2) + ggtitle("solution 2")
#' ppp_plot(sim_project_data, sim_action_data, sim_tree, s1, "name", "cost",
#'          "success", n = 2) + ggtitle("solution 3")
#' @export
ppp_manual_phylo_solution <- function(x, y, tree, solution,
                                      project_column_name,
                                      success_column_name,
                                      action_column_name,
                                      cost_column_name) {
  # assertions
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
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
                          all(y[[cost_column_name]] >= 0))
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
  assertthat::assert_that(is.logical(as.matrix(
    x[, y[[action_column_name]], drop = FALSE])))
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

  # main processing
  ## format statistics for output
  s <- solution
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      solution = seq_len(nrow(s)),
      method = "manual",
      epd = ppp_epd(x, y, tree, s, project_column_name,
                    success_column_name, action_column_name),
      er = ppp_epd(x, y, star_phylogeny(tree$tip.label), s,
                   project_column_name, success_column_name,
                   action_column_name),
      budget = NA_real_,
      cost = rowSums(matrix(y[[cost_column_name]], byrow = TRUE,
                            ncol = nrow(y), nrow = nrow(s)) *
                     as.matrix(s[, y[[action_column_name]], drop = FALSE])),
      optimal = NA),
    s[, y[[action_column_name]], drop = FALSE]))

  # return result
  out
}
