#' @include internal.R
NULL

#' Plot a 'Project Prioritization Protocol' solution with species-level data
#'
#' Create a plot to visualize a solution to the 'Project Prioritization
#' Protocol' problem (Joseph, Maloney & Possingham 2009) with species
#' weights. This plot is essentially a phylogram with the length of each
#' branch corresponding to the species' weight.
#'
#' @inheritParams help
#' @inheritParams ppp_plot_phylo_solution
#'
#' @inherit ppp_plot_phylo_solution details return
#'
#' @seealso To generate solutions for the 'Project
#'   Prioritization Protocol' problem, see
#'   \code{\link{ppp_heuristic_spp_solution}}
#'   \code{\link{ppp_exact_spp_solution}},
#'   \code{\link{ppp_manual_spp_solution}},
#'   or \code{\link{ppp_random_spp_solution}}.
#'
#' @references
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
#' # create random some solutions with a budget of 700
#' s1 <- ppp_random_spp_solution(sim_project_data, sim_action_data,
#'                               sim_species_data, 700, "name", "success",
#'                               "name", "cost", "name", "weight",
#'                               number_solutions = 10)
#'
#' # print output
#' print(s1)
#'
#' # plot the first solution
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       s1, "name", "success", "name", "cost", "name",
#'                       "weight")
#'
#' # plot the second solution
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       s1, "name", "success", "name", "cost", "name",
#'                       "weight", n = 2)
#'
#' # since this function returns a ggplot2 plot object, we can customize the
#' # appearance of the plot using standard ggplot2 commands!
#' # for example, we can add a title
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       s1, "name", "success", "name", "cost", "name",
#'                       "weight") +
#' ggtitle("solution")
#'
#' # we could also also set the minimum and maximum values in the color ramp to
#' # correspond to those in the data, rather than being capped at 0 and 1
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       s1, "name", "success", "name", "cost", "name",
#'                       "weight") +
#' scale_color_gradientn(name = "Probability of\npersistence",
#'                       colors = viridisLite::inferno(150, begin = 0,
#'                                                     end = 0.9,
#'                                                     direction = -1)) +
#' ggtitle("solution")
#'
#' # we could also change the color ramp
#' ppp_plot_spp_solution(sim_project_data, sim_action_data, sim_species_data,
#'                       s1, "name", "success", "name", "cost", "name",
#'                       "weight") +
#' scale_color_gradient(name = "Probability of\npersistence",
#'                      low = "red", high = "black") +
#' ggtitle("solution")
#'
#' # we could even hide the legend if desired
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_species_data,
#'                         s1, "name", "success", "name", "cost", "name",
#'                         "weight") +
#' scale_color_gradient(name = "Probability of\npersistence",
#'                      low = "red", high = "black") +
#' theme(legend.position = "hide") +
#' ggtitle("solution")
#' @export
ppp_plot_spp_solution <- function(x, y, spp, solution, project_column_name,
                                  success_column_name, action_column_name,
                                  cost_column_name, species_column_name,
                                  weight_column_name = NULL, n = 1L,
                                  symbol_hjust = 0.007) {
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

  # Main processing
  ## make plot
  p <- ppp_plot_spp_solution(
    x = x, y = y, tree = star_phylogeny(spp[[species_column_name]], w),
    solution = solution, project_column_name = project_column_name,
    success_column_name = success_column_name,
    action_column_name = action_column_name,
    cost_column_name = cost_column_name,
    n = n, symbol_hjust = symbol_hjust)

  # Exports
  ## return plot
  p
}
