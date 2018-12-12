#' @include internal.R
NULL

#' Prioritize conservation projects by maximizing expected weighted species
#' richness using heuristic algorithms
#'
#' Prioritize funding for conservation projects using the 'Project
#' Prioritization Protocol' (Joseph, Maloney & Possingham 2009) with
#' species weights and using a backwards heuristic algorithm (Bennett
#' \emph{et al}. 2014). \strong{Although this algorithm can deliver solutions
#' that perform better than random, it is extremely unlikely to identify
#' solutions that are optimal (Underhill 1994; Rodrigues & Gaston 2002).}
#'
#' @inheritParams help
#'
#' @inherit help return
#'
#' @details This algorithm aims to identify a set of conservation projects,
#' each associated with a set of conservation actions, that should be
#' funded to maximize the expected weighted species richness (i.e. sum of each
#' species' persisting into the future, multiplied by its weight). Briefly,
#' this algorithm works by starting off with all conservation actions selected
#' for funding and then begins iteratively defunding (removing) actions until
#' the budget is met (Joseph, Maloney & Possingham 2009; Bennett \emph{et al}.
#' 2014). In a given iteration, each action is evaluated in terms of the
#' expected weighted species riches when the action is not funded, and the
#' action associated with the lowest reduction in this metric is defunded. Since
#' projects are only considered funded when all of their associated actions are
#' also funded---and species only receive benefits from projects that are
#' funded, and not individual conservation actions---by iteratively removing
#' actions according to their expected utility, this algorithm may identify
#' cost-effective funding schemes. Note, however, that this algorithm is
#' extremely unlikely to identify optimal solutions.
#'
#' The calculations that underpin this algorithm can be expressed
#' mathematically. To calculate the utility for funding a given action (\eqn{L})
#' among a set of actions (\eqn{L}), let the expected weighted species
#' richness when all the actions are funded
#' be expressed as \eqn{A(L)}. Also, let the expected weighted species
#' richness when all the remaining actions are funded except for action \eqn{l}
#' be expressed as \eqn{A(L - l)}. Furthermore, allow the cost for funding
#' action \eqn{l} to be \eqn{C_l}. Given this, the relative benefit (or
#' utility) for funding action \eqn{l}
#' (\eqn{U_l}) in a given iteration can be expressed as:
#'
#' \deqn{U_l = \frac{A(L) - A(L - l)}{C_l}}{A_l = (A(L) - A(L - l)) / C_l}
#'
#' To calculate the weighted species richness that is expected to persist into
#' the future for a given set of funded actions, we will adopt a new set
#' of definitions to avoid confusion. Let \eqn{I} represent a given set of
#' funded actions (indexed by \eqn{i}). For example, \eqn{I} could denote all
#' of the actions in a given iteration (\eqn{A(L)}) or all of the actions in a
#' given iteration except for a specific action (\eqn{A(L - l)}).
#' Next, let \eqn{S} represent each species (e.g. species;
#  indexed by \eqn{s}). Additionally, let \eqn{J} denote the set of funded
#' conservation projects (indexed by
#' \eqn{j}) given the set of funded actions \eqn{I}. Let \eqn{P_j} represent
#' the probability of project \eqn{j} being successful if
#' it is funded. To represent the conservation outcome for
#' funding the projects \eqn{J}, let \eqn{B_{js}} denote the probability of
#' persistence for the species \eqn{s} if project \eqn{j} is funded and project
#' \eqn{j} is used to conserve that species (i.e. it is the best funded
#' funded project for that species). Furthermore, let \eqn{W_s} denote
#' the weight for each species. This weight may reflect cultural or
#' economic importance.
#'
#' The probability that each species will go extinct (\eqn{E_s}) when a given
#' set of projects are funded (\eqn{J}) can then be  expressed as as:
#'
#' \deqn{E_s = 1 - \mathrm{max}(P_1 \times B_{1s}, \ldots, P_J \times B_{Js})}{E_s = 1 - max(P_1 B_{1s}, ..., P_J B_{Js})}
#'
#' The weighted species richness that is expected to persist when a
#' given set of projects are funded can then be expressed as:
#'
#' \deqn{A(I) = \sum_{s = 0}^{S} (1 - E_s) W_s}{A(I) = sum_{s = 0}^{S} (1 - E_s) W_s}
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem using species weights, see
#'   \code{\link{ppp_exact_spp_solution}}
#'   \code{\link{ppp_manual_spp_solution}}, and
#'   \code{\link{ppp_random_spp_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot_spp_solution}}.
#
#' @references
#' Bennett JR, Elliott G, Mellish B, Joseph LN, Tulloch AI,
#' Probert WJ, ... & Maloney R (2014) Balancing phylogenetic diversity
#' and species numbers in conservation prioritization, using a case study of
#' threatened species in New Zealand. \emph{Biological Conservation},
#' \strong{174}: 47--54.
#'
#' Faith DP (2008) Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}: 1461--1470.
#'
#' Joseph LN, Maloney RF, & Possingham HP (2009) Optimal
#' allocation of resources among threatened species: a project prioritization
#' protocol. \emph{Conservation biology}, \strong{23}, 328--338.
#'
#' Rodrigues AS & Gaston KJ (2002) Optimisation in reserve selection
#' procedures---why not? \emph{Biological Conservation}, \strong{107}: 123-129.
#'
#' Underhill LG (1994) Optimal and suboptimal reserve selection
#' algorithms. \emph{Biological Conservation}, \strong{70}: 85--87.
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
#' \donttest{
#' # find a solution that meets a budget of 300
#' s1 <- ppp_heuristic_spp_solution(sim_project_data, sim_action_data,
#'                                  sim_species_data, 300, "name", "success",
#'                                  "name", "cost", "name", "weight")
#'
#' # print solution
#' print(s1)
#'
#' # print the names of which actions were funded
#' print(names(s1)[which(unlist(s1[1, sim_action_data$name]))])
#'
#' # plot solution
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       s1, "name", "success", "name", "cost", "name",
#'                       "weight")
#'
#' # find a solution that meets a budget of 300 and allocates
#' # funding for the "S1_action" action. For instance, species "S1" might
#' # be an iconic species that has cultural and economic importance.
#' sim_action_data2 <- sim_action_data
#' sim_action_data2$locked_in <- sim_action_data2$name == "S1_action"
#' s2 <- ppp_heuristic_spp_solution(sim_project_data, sim_action_data2,
#'                                  sim_species_data, 300, "name", "success",
#'                                  "name", "cost",
#'                                  locked_in_column_name = "locked_in")
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data2,
#'                         sim_species_data, s2, "name", "success", "name",
#'                         "cost", "name", "weight")
#'
#' # find a solution that meets a budget of 300 and does not allocate
#' # funding for the "S2_action" action. For instance, species "S2"
#' # might have very little cultural or economic importance. Broadly speaking,
#' # though, it is better to "lock in" "important" species rather than
#' # "lock out" unimportant species.
#' sim_action_data3 <- sim_action_data
#' sim_action_data3$locked_out <- sim_action_data3$name == "S2_action"
#' s3 <- ppp_heuristic_spp_solution(sim_project_data, sim_action_data3,
#'                                  sim_species_data, 300, "name", "success",
#'                                  "name", "cost", "name", "weight",
#'                                  locked_out_column_name = "locked_out")
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data3,
#'                         sim_species_data, s3, "name", "success", "name",
#'                         "cost", "name", "weight")
#'
#' # find all solutions from the heuristic algorithm
#' # note we can set the budget higher than the total cost of all the
#' # projects, and the number of solutions to the total number of
#' # projects to achieve this
#' s4 <- ppp_heuristic_spp_solution(sim_project_data, sim_action_data,
#'                                  sim_species_data,
#'                                  sum(sim_action_data$cost) * 1.1,
#'                                  "name", "success", "name", "cost",
#'                                  "name", "weight",
#'                                  number_solutions = nrow(sim_action_data))
#'
#' # print solutions
#' print(s4)
#'
#' # plot solution cost against expected phylogenetic diversity
#' plot(obj ~ cost, data = s4,
#'      main = "Heuristic solutions", xlab = "Cost ($)",
#'      ylab = "Expected weighted species richness")
#' }
#' @export
ppp_heuristic_spp_solution <- function(x, y, spp, budget,
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
                          isTRUE(all(spp[[species_column_name]] %in% names(x))),
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
  out <- ppp_heuristic_phylo_solution(
    x = x, y = y, tree = star_phylogeny(spp[[species_column_name]], w),
    budget = budget,
    project_column_name = project_column_name,
    success_column_name = success_column_name,
    action_column_name = action_column_name,
    cost_column_name = cost_column_name,
    locked_in_column_name = locked_in_column_name,
    locked_out_column_name = locked_out_column_name,
    number_solutions = number_solutions)

  # return output
  out
}
