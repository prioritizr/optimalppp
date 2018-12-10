#' @include internal.R
NULL

#' Prioritize conservation projects with species-level data using exact
#' algorithms
#'
#' Prioritize funding for conservation projects with species weights and
#' using exact algorithms. Unlike other algorithms for solving the
#' 'Project Prioritization Protocol'
#' (Joseph, Maloney & Possingham 2009), this method can identify
#' solutions that are guaranteed to be optimal (or within a pre-specified
#' optimality gap; see Underhill 1994; Rodrigues & Gaston 2002).
#' \strong{As a consequence, it is strongly recommended to use
#' this method for developing project prioritizations.}
#'
#' @inheritParams help
#'
#' @inherit help return
#'
#' @details This function works by formulating the 'Project Prioritization
#'   Protocol' as a mixed integer programming problem (MIP) and solving it
#'   using the
#'   \href{https://www.gurobi.com}{Gurobi optimization software suite}.
#'   Although \href{https://www.gurobi.com}{Gurobi} is a commercial software,
#'   academics can obtain a \href{https://user.gurobi.com/download/licenses/free-academic}{special license for no cost}.
#'    After downloading and installing the
#'   href{https://www.gurobi.com}{Gurobi} software suite, the
#'   \pkg{gurobi} package will also need to be installed (see instructions for
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/software_installation_guid.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/software_installation_guid.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/software_installation_guid.html}{Windows} operating systems).
#' Finally, the \pkg{gurobi} package will also need to be installed (see
#' instructions for
#'  \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/r_installing_the_r_package.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/r_installing_the_r_package.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/r_installing_the_r_package.html}{Windows} operating systems).
#'
#'   The mathematical formulation is described in
#'   \code{\link{ppp_exact_phylo_solution}}. Instead of using a "real"
#'   phylogeny, it simply uses a star phylogeny---constructed using the species'
#'   weights---to denote the relationships and importance for conserving
#'   different species.
#'
#' @seealso For other methods for solving the 'Project Prioritization Protocol'
#'   problem, see \code{\link{ppp_heuristic_phylo_solution}},
#'   \code{\link{ppp_manual_phylo_solution}}, and
#'   \code{\link{ppp_random_phylo_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot_phylo_solution}}.
#'
#' @references
#' Joseph LN, Maloney RF & Possingham HP (2009) Optimal allocation of
#' resources among threatened species: A project prioritization protocol.
#' \emph{Conservation Biology}, \strong{23}, 328--338.
#'
#' Rodrigues AS & Gaston KJ (2002) Optimisation in reserve selection
#' procedures---why not? \emph{Biological Conservation}, \strong{107}: 123-129.
#'
#' Underhill LG (1994) Optimal and suboptimal reserve selection
#' algorithms. \emph{Biological Conservation}, \strong{70}: 85--87.
#'
#' @examples
#' # load built-in data
#' data(sim_project_data, sim_action_data, sim_species_data)
#'
#' # print simulated project data set
#' print(sim_project_data)
#'
#' # print simulated action data
#' print(sim_action_data)
#'
#' # print simulated species data
#' print(sim_species_data)
#' \donttest{
#' # verify if guorbi package is installed
#' if (!require(gurobi, quietly = TRUE))
#'  stop("the gurobi R package is not installed.")
#'
#' # find a solution that meets a budget of 300
#' s1 <- ppp_exact_spp_solution(sim_project_data, sim_action_data,
#'                              sim_species_data, 300, "name", "success",
#'                              "name", "cost", "name", "weight")
#'
#' # print solution
#' print(s1)
#'
#' # print the names of which projects were funded
#' print(names(s1)[which(unlist(s1[1, sim_action_data$name]))])
#'
#' # plot solution
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       "name", "success", "name", "cost", "name", "weight")
#'
#' # find a solution that meets a budget of 300 and allocates
#' # funding for the "S1_project" project. For instance, species "S1" might
#' # be an iconic species that has cultural and economic importance.
#' sim_action_data2 <- sim_action_data
#' sim_action_data2$locked_in <- sim_action_data2$name == "S1_project"
#' s2 <- ppp_plot_spp_solution(sim_project_data, sim_action_data2,
#'                             sim_species_data, 300, "name", "success",
#'                             "name", "cost", "name", "weight",
#'                             locked_in_column_name = "locked_in")
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' ppp_plot_spp_solution(sim_project_data, sim_action_data2, sim_species_data,
#'                       s2, "name", "success", "name", "cost", "name",
#'                       "weight")
#'
#' # find a solution that meets a budget of 300 and does not allocate
#' # funding for the "S2_project" project. For instance, species "S2"
#' # might have very little cultural or economic importance. Broadly speaking,
#' # though, it is better to "lock in" "important" species rather than
#' # "lock out" unimportant species.
#' sim_action_data3 <- sim_action_data
#' sim_action_data3$locked_out <- sim_action_data3$name == "S2_project"
#' s3 <- ppp_plot_spp_solution(sim_project_data, sim_action_data3,
#'                             sim_species_data, 300, "name", "success",
#'                             "name", "cost", "name", "weight",
#'                             locked_out_column_name = "locked_out")
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' ppp_plot_spp_solution(sim_project_data, sim_action_data3, sim_tree, s3,
#'                       "name", "success", "name", "cost")
#' }
#' @export
ppp_exact_spp_solution <- function(x, y, spp, budget,
                                   project_column_name,
                                   success_column_name,
                                   action_column_name,
                                   cost_column_name,
                                   species_column_name,
                                   weight_column_name,
                                   locked_in_column_name = NULL,
                                   locked_out_column_name = NULL,
                                   gap = 0.000001, threads = 1L,
                                   number_solutions = 1L,
                                   time_limit = .Machine$integer.max,
                                   number_approx_points = 300,
                                   verbose = FALSE) {
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
  out <- ppp_exact_phylo_solution(
    x = x, y = y, tree = star_phylogeny(spp[[species_column_name]], w),
    budget = budget,
    project_column_name = project_column_name,
    success_column_name = success_column_name,
    action_column_name = action_column_name,
    cost_column_name = cost_column_name,
    locked_in_column_name = locked_in_column_name,
    locked_out_column_name = locked_out_column_name,
    gap = gap, threads = threads,
    number_solutions = number_solutions,
    time_limit = time_limit,
    number_approx_points = number_approx_points,
    verbose = verbose)

  # return output
  out
}
