#' @include internal.R
NULL

#' Randomly prioritize conservation projects with species data
#'
#' Generate random solutions for the 'Project Prioritization Protocol'
#' problem (Joseph, Maloney & Possingham 2009) with species weights.
#' Although conservation projects should, ideally, not be funded based on random
#' allocations, it can be useful to compare the effectiveness of solutions to
#' random decisions in order to evaluate their effectiveness.
#' \strong{When informing conservation actions, it is strongly recommended to
#' use the \code{\link{ppp_exact_sppp_solution}} method because it can identify
#' optimal funding schemes with a guarantee.}
#'
#' @inheritParams help
#'
#' @details The random solutions are generated using the following algorithm.
#'  Firstly, all projects are initially selected for funding (excepting actions
#'  which are locked out). Secondly, an action is randomly selected and
#'  defunded. Thirdly, the second step is
#'  repeated until the total cost of the remaining projects that are selected
#'  for funding is within the budget. Note that actions that have zero cost
#'  are never deselected for funding, and are always included in the solutions.
#'  Additionally, actions that are locked in are never deselected for funding.
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem using phylogenetic data, see
#'   \code{\link{ppp_heuristic_spp_solution}}
#'   \code{\link{ppp_exact_spp_solution}}, and
#'   \code{\link{ppp_manual_spp_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_spp_plot}}.
#'
#' @references
#' Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
#' resources among threatened species: A project prioritization protocol.
#' \emph{Conservation Biology}, \strong{23}, 328--338.
#'
#' @examples
#' # set seed for reproducibility
#' set.seed(500)
#'
#' # load built-in data
#' data(sim_project_data, sim_action_data, sim_species_data)
#'
#' # print simulated project data
#' print(sim_project_data)
#'
#' # print simulated action data
#' print(sim_action_data)
#'
#' # print simulated species data
#' print(sim_species_data)
#'
#' # generate 10 random solutions that meet a budget of 300
#' s1 <- ppp_random_spp_solution(sim_project_data, sim_action_data,
#'                               sim_species_data, 300, "name", "success",
#'                               "name", "cost", "name", "weight",
#'                               number_solutions = 10)
#'
#' # print solutions
#' print(s1)
#'
#' # plot first solution
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       s1, "name", "success", "name", "cost", "name",
#'                       "weight", n = 1)
#'
#' # view histogram the objective value
#' hist(s1$obj, xlab = "sum weighted species probabilities")
#'
#' # view histogram of their costs
#' hist(s1$cost, xlab = "solution cost")
#' @export
ppp_random_spp_solution <- function(x, y, spp, budget,
                                    project_column_name,
                                    success_column_name,
                                    action_column_name,
                                    cost_column_name,
                                    species_column_name,
                                    weight_column_name = NULL,
                                    locked_in_column_name = NULL,
                                    locked_out_column_name = NULL,
                                    number_solutions = 1L) {
  # assertions
  ## coerce x to tibble if just a regular data.frame
  if (inherits(spp, "data.frame") && !inherits(spp, "tbl_df"))
    spp <- tibble::as_tibble(spp)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(spp, "tbl_df"),
                          ncol(spp) > 0, nrow(spp) > 0,
                          assertthat::is.string(species_column_name),
                          assertthat::has_name(spp, species_column_name),
                          assertthat::noNA(spp[[species_column_name]]),
                          inherits(spp[[species_column_name]],
                                   c("character", "factor")),
                          all(spp[[species_column_name]] %in% names(x)),
                          anyDuplicated(spp[[species_column_name]]) == 0)
  if (!is.null(weight_column_name)) {
    assertthat::assert_that(assertthat::is.string(weight_column_name),
                            assertthat::has_name(spp, weight_column_name),
                            is.numeric(spp[[weight_column_name]]),
                            assertthat::noNA(spp[[weight_column_name]]))
    w <- spp[[weight_column_name]]
  } else {
    w <- rep(1, nrow(spp))
  }

  ## coerce factor project names to character
  if (is.factor(spp[[species_column_name]]))
    spp[[species_column_name]] <- as.character(x[[species_column_name]])

  # generate random solutions
  out <- ppp_random_phylo_solution(
    x = x, y = y, tree = star_phylogeny(spp[[species_column_name]], w),
    budget = budget, project_column_name = project_column_name,
    success_column_name = success_column_name,
    action_column_name = action_column_name,
    cost_column_name = cost_column_name,
    locked_in_column_name = locked_in_column_name,
    locked_out_column_name = locked_out_column_name,
    number_solutions = number_solutions)

  # return output
  out
}
