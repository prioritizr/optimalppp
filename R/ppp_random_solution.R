#' @include internal.R
NULL

#' Random solutions for the 'Project Prioritization Protocol' problem
#'
#' Generate random solutions for the 'Project Prioritization Protocol'
#' problem (Joseph, Maloney & Possingham 2009).
#' Although conservation projects should, ideally, not be funded based on random
#' allocations, it can be useful to compare the effectiveness of solutions to
#' random decisions in order to evaluate their effectiveness.
#' \strong{When informing conservation actions, it is strongly recommended to
#' use the \code{\link{ppp_exact_solution}} method because it can identify
#' optimal funding schemes with a guarantee.}
#'
#' @inheritParams help
#'
#' @details The random solutions are generated using the following algorithm.
#'  Firstly, all projects are initially selected for funding (excepting projects
#'  which are locked out). Secondly, a project is randomly selected and
#'  defunded. Thirdly, the second step is
#'  repeated until the total cost of the remaining projects that are selected
#'  for funding is within the budget. Note that projects that have zero cost
#'  are never deselected for funding, and are always included in the solutions.
#'  Additionally, projects that are locked in are never deselected for funding.
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem, see \code{\link{ppp_heuristic_solution}}
#'   \code{\link{ppp_exact_solution}}, and \code{\link{ppp_manual_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot}}.
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
#' # load built-in data
#' data(sim_project_data, sim_tree)
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
#' # generate 10 random solutions that meet a budget of 300
#' s1 <- ppp_random_solution(sim_project_data, sim_tree, 300,
#'                           "name", "cost", "success", number_solutions = 10)
#'
#' # print solutions
#' print(s1)
#'
#' # plot first solution
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success", n = 1)
#'
#' # view histogram of their objective values
#' hist(s1$objective, xlab = "solution objective")
#'
#' # view histogram of their costs
#' hist(s1$cost, xlab = "solution cost")
#'
#' @export
ppp_random_solution <- function(x, tree, budget,
                                project_column_name,
                                cost_column_name,
                                success_column_name,
                                locked_in_column_name = NULL,
                                locked_out_column_name = NULL,
                                number_solutions = 1L) {
  # assertions
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, "tbl_df"),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.number(budget),
                          is.finite(budget),
                          isTRUE(budget >= 0),
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
                          assertthat::is.count(number_solutions),
                          is.finite(number_solutions))
  assertthat::assert_that(min(x[[cost_column_name]]) >= 0,
                          msg = "zero cost baseline project missing.")
  if (!is.null(locked_in_column_name))
    assertthat::assert_that(assertthat::is.string(locked_in_column_name),
                            assertthat::has_name(x, locked_in_column_name),
                            is.logical(x[[locked_in_column_name]]),
                            assertthat::noNA(x[[locked_in_column_name]]))
  if (!is.null(locked_out_column_name))
    assertthat::assert_that(assertthat::is.string(locked_out_column_name),
                            assertthat::has_name(x, locked_out_column_name),
                            is.logical(x[[locked_out_column_name]]),
                            assertthat::noNA(x[[locked_out_column_name]]))
  if (!is.null(locked_in_column_name) && !is.null(locked_out_column_name)) {
    assertthat::assert_that(max(x[[locked_out_column_name]] +
                            x[[locked_in_column_name]]) <= 1,
                            msg = "some projects locked in and locked out.")
  }
  assertthat::assert_that(
    all(tree$tip.label %in% names(x)),
    msg = paste("argument to tree contains species that do not appear as",
                "column names in the argument to x:",
                paste(paste0("'", setdiff(tree$tip.label, names(x)), "'"),
                      collapse = ", ")))

  # preliminary data formatting
  ## coerce factor species names to character
  if (is.factor(x[[project_column_name]]))
    x[[project_column_name]] <- as.character(x[[project_column_name]])

  ## check that branches have lengths
  if (is.null(tree$edge.length)) {
    tree$edge.length <- rep(1, nrow(tree$edge))
    warning(paste("tree does not have branch length data,",
                  "all branches are assumed to have equal lengths"))
  }  else {
    assertthat::assert_that(nrow(tree$edge) == length(tree$edge.length))
  }

  ## determine which projects need to be locked
  locked_in <- integer(0)
  locked_out <- integer(0)
  if (!is.null(locked_in_column_name))
    locked_in <- which(x[[locked_in_column_name]])
  if (!is.null(locked_out_column_name))
    locked_out <- which(x[[locked_out_column_name]])
  assertthat::assert_that(sum(x[[cost_column_name]][locked_in]) <= budget,
                          msg = "locked in projects exceed budget.")

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(methods::as(round(spp_probs, 5), "dgCMatrix"))

  # solve the problem
  s <- rcpp_random_solution(spp = spp_probs,
                               budget = budget,
                               branch_matrix = branch_matrix(tree),
                               branch_lengths = tree$edge.length,
                               costs = x[[cost_column_name]],
                               locked_in = locked_in,
                               locked_out = locked_out,
                               n_solutions = number_solutions)

  # prepare results for output
  colnames(s) <- as.character(x[[project_column_name]])
  out <- tibble::as_tibble(s)

  ## format statistics for output
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      solution = seq_len(nrow(out)),
      objective = ppp_objective_value(x, tree, project_column_name,
                                      success_column_name, out),
      budget = budget,
      cost = rowSums(matrix(x[[cost_column_name]], byrow = TRUE,
                            ncol = ncol(out), nrow = nrow(out)) *
                     as.matrix(out)),
      optimal = NA,
      method = "random"), out))

  # return result
  out
}
