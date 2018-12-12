#' @include internal.R
NULL

#' Randomly prioritize conservation projects under expected phylogenetic
#' diversity
#'
#' Generate random solutions for the 'Project Prioritization Protocol'
#' problem (Joseph, Maloney & Possingham 2009) and evaluate them using
#' 'expected phylogenetic diversity' (Faith 2008).
#' Although conservation projects should, ideally, not be funded based on random
#' allocations, it can be useful to compare the effectiveness of solutions to
#' random decisions in order to evaluate their effectiveness.
#' \strong{When informing conservation actions, it is strongly recommended to
#' use the \code{\link{ppp_exact_phylo_solution}} method because it can identify
#' optimal funding schemes with a guarantee.}
#'
#' @inheritParams help
#'
#' @details Each random solution is generated using the following algorithm.
#'  Firstly, all actions are initially selected for funding (excepting actions
#'  which are locked out). Secondly, an action is randomly selected and
#'  defunded. Thirdly, the second step is
#'  repeated until the total cost of the remaining actions that are prioritized
#'  for funding is within the budget. Note that actions that have zero cost
#'  are never deselected for funding, and are always included in the solutions.
#'  Additionally, actions that are locked in are never deselected for funding.
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem using phylogenetic data, see
#'   \code{\link{ppp_heuristic_phylo_solution}}
#'   \code{\link{ppp_exact_phylo_solution}}, and
#'   \code{\link{ppp_manual_phylo_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot_phylo_solution}}.
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
#' # generate 10 random solutions that meet a budget of 300
#' s1 <- ppp_random_phylo_solution(sim_project_data, sim_action_data, sim_tree,
#'                                 300, "name", "success", "name", "cost",
#'                                 number_solutions = 10)
#'
#' # print solutions
#' print(s1)
#'
#' # plot first solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s1,
#'                         "name", "success", "name", "cost", n = 1)
#'
#' # view histogram of their expected phylogenetic diversity
#' hist(s1$obj, xlab = "Expected phylogenetic diversity")
#'
#' # view histogram of their costs
#' hist(s1$cost, xlab = "Solution cost ($)")
#' @export
ppp_random_phylo_solution <- function(x, y, tree, budget,
                                      project_column_name,
                                      success_column_name,
                                      action_column_name,
                                      cost_column_name,
                                      locked_in_column_name = NULL,
                                      locked_out_column_name = NULL,
                                      number_solutions = 1L) {
  # assertions
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
  ## coerce x to tibble if just a regular data.frame
  if (inherits(y, "data.frame") && !inherits(y, "tbl_df"))
    y <- tibble::as_tibble(y)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, "tbl_df"),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(y, "tbl_df"),
                          ncol(y) > 0, nrow(y) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.number(budget),
                          is.finite(budget),
                          isTRUE(budget >= 0),
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
                          assertthat::is.count(number_solutions),
                          assertthat::noNA(number_solutions))
  assertthat::assert_that(min(y[[cost_column_name]]) >= 0,
                          msg = "zero cost baseline project missing.")
  ## coerce factor project names to character
  if (is.factor(x[[project_column_name]]))
    x[[project_column_name]] <- as.character(x[[project_column_name]])
  ## coerce factor action names to character
  if (is.factor(y[[action_column_name]]))
    y[[action_column_name]] <- as.character(y[[action_column_name]])
  ## locked in checks
  if (!is.null(locked_in_column_name))
    assertthat::assert_that(assertthat::is.string(locked_in_column_name),
                            assertthat::has_name(y, locked_in_column_name),
                            is.logical(y[[locked_in_column_name]]),
                            assertthat::noNA(y[[locked_in_column_name]]))
  if (!is.null(locked_out_column_name))
    assertthat::assert_that(assertthat::is.string(locked_out_column_name),
                            assertthat::has_name(y, locked_out_column_name),
                            is.logical(y[[locked_out_column_name]]),
                            assertthat::noNA(y[[locked_out_column_name]]))
  if (!is.null(locked_in_column_name) && !is.null(locked_out_column_name)) {
    assertthat::assert_that(max(y[[locked_out_column_name]] +
                            y[[locked_in_column_name]]) <= 1,
                            msg = "some projects locked in and locked out.")
  }
  ## species name checks
  assertthat::assert_that(
    all(tree$tip.label %in% names(x)),
    msg = paste("argument to tree contains species that do not appear as",
                "column names in the argument to x:",
                paste(paste0("'", setdiff(tree$tip.label, names(x)), "'"),
                      collapse = ", ")))
  ## check that branches have lengths
  if (is.null(tree$edge.length)) {
    tree$edge.length <- rep(1, nrow(tree$edge))
    warning(paste("tree does not have branch length data,",
                  "all branches are assumed to have equal lengths"))
  }  else {
    assertthat::assert_that(nrow(tree$edge) == length(tree$edge.length))
  }
  ## additional column checks
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

  # preliminary data formatting
  ## determine which actions need to be locked
  locked_in <- integer(0)
  locked_out <- integer(0)
  if (!is.null(locked_in_column_name))
    locked_in <- which(y[[locked_in_column_name]])
  if (!is.null(locked_out_column_name))
    locked_out <- which(y[[locked_out_column_name]])
  assertthat::assert_that(sum(y[[cost_column_name]][locked_in]) <= budget,
                          msg = "locked in actions exceed budget.")

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(methods::as(round(spp_probs, 5), "dgCMatrix"))

  # solve the problem
  s <- rcpp_random_solution(
    spp = spp_probs,
    actions = methods::as(as.matrix(x[, y[[action_column_name]],
                                      drop = FALSE]), "dgCMatrix"),
    budget = budget,
    branch_matrix = branch_matrix(tree),
    branch_lengths = tree$edge.length,
    costs = y[[cost_column_name]],
    locked_in = locked_in,
    locked_out = locked_out,
    n_solutions = number_solutions)

  # prepare results for output
  ## add column name
  colnames(s) <- as.character(y[[action_column_name]])
  out <- tibble::as_tibble(s)

  ## format statistics for output
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      solution = seq_len(nrow(s)),
      method = "random",
      obj = ppp_epd(x, y, tree, out, project_column_name,
                    success_column_name, action_column_name),
      budget = budget,
      cost = rowSums(matrix(y[[cost_column_name]], byrow = TRUE,
                            ncol = nrow(y), nrow = nrow(s)) *
                     as.matrix(out)),
      optimal = NA),
    out))

  # return result
  out
}
