#' @include internal.R
NULL

#' Simulate conservation project data
#'
#' Simulate data to prioritize project funding schemes for species conservation.
#'
#' @param number_species \code{numeric} number of species.
#'
#' @param cost_mean \code{numeric} average cost for the projects. Defaults to
#'   \code{100}.
#'
#' @param cost_sd \code{numeric} standard deviation in project costs. Defaults
#'   to \code{5}.
#'
#' @param success_min_probability \code{numeric} minimum probability of the
#'   projects succeeding if they are funded. Defaults to \code{0.7}.
#'
#' @param success_max_probability \code{numeric} maximum probability of the
#'   projects succeeding if they are funded. Defaults to \code{0.99}.
#'
#' @param funded_min_persistence_probability \code{numeric} minimum probability
#'   of the species persisting if their projects are funded and successful.
#'   Defaults to \code{0.5}.
#'
#' @param funded_max_persistence_probability \code{numeric} maximum probability
#'   of the species persisting if their projects are funded and successful.
#'   Defaults to \code{0.9}.
#'
#' @param not_funded_min_persistence_probability \code{numeric} minimum
#'   probability of the species persisting if their projects are not funded.
#'   Defaults to \code{0.01}.
#'
#' @param not_funded_max_persistence_probability \code{numeric} maximum
#'   probability of the species persisting if their projects are not funded.
#'   Defaults to \code{0.4}.
#'
#' @param locked_in_proportion \code{numeric} of projects that are locked
#'   into the solution. Defaults to \code{0}.
#'
#' @param locked_out_proportion \code{numeric} of projects that are locked
#'   into the solution. Defaults to \code{0}.
#'
#' @details The simulated data set will contain one conservation project for
#'   each species and a baseline "do nothing" project to reflect species'
#'   persistence when their conservation projects are not funded. The
#'   data are simulated as follows:
#'
#'   \enumerate{
#'
#'     \item A conservation project is created for each species.
#'
#'     \item Cost data for each project are simulated using a normal
#'       distribution and the \code{cost_mean} and \code{cost_sd} arguments.
#'
#'     \item The probability of each project succeeding is simulated by
#'       drawing probabilities from a uniform distribution with the upper
#'       and lower bounds set as the \code{success_min_probability} and
#'       \code{success_max_probability} arguments.
#'
#'     \item The probability of each project succeeding is simulated by
#'       drawing probabilities from a uniform distribution with the upper
#'       and lower bounds set as the \code{success_min_probability} and
#'       \code{success_max_probability} arguments.
#'
#'     \item A set proportion of the projects are randomly set to be locked
#'       in and out of the solutions using the \code{locked_in_proportion} and
#'       \code{locked_out_proportion} arguments.

#'     \item The probability of each species persisting is simulated by
#'       drawing probabilities from a uniform distribution with the upper
#'       and lower bounds set as the \code{funded_min_persistence_probability}
#'       and \code{funded_max_persistence_probability} arguments.
#'
#'     \item An additional project is created which represents the baseline
#'       "do nothing" scenario. The probability of each species persisting
#'       when managed under this project is simulated by drawing probabilities
#'       from a uniform distribution with the upper and lower bounds
#'       set as the \code{not_funded_min_persistence_probability}
#'       and \code{not_funded_max_persistence_probability} arguments.
#'
#'     \item A phylogenetic tree is simulated for the species using
#'       \code{\link[ape]{rcoal}}.
#'
#'  }
#'
#' @return A \code{list} object containing the elements:
#'
#'   \describe{
#'
#'     \item{\code{"project_data"}}{A \code{\link[tibble]{tibble}} containing
#'       the data for the conservation projects. It contains the following
#'       columns:
#'
#'       \describe{
#'
#  '       \item{\code{"name"}}{\code{character} name for each project.}
#'
#'         \item{\code{"cost"}}{\code{numeric} cost for each project.}
#'
#'         \item{\code{"success"}}{\code{numeric} probability of each project
#'           succeeding if it is funded.}
#'
#'         \item{\code{"locked_in"}}{\code{logical} indicating if certain
#'           projects should be locked into the solution.}
#'
#'         \item{\code{"locked_out"}}{\code{logical} indicating if certain
#'           projects should be locked out of the solution.}
#'
#'         \item{\code{"S1"} ... \code{"SN"}}{\code{numeric} columns for each
#'           species, ranging from \code{"S1"} to \code{"SN"} where N is the
#'           number of species, indicating the enhanced probability that each
#'           species will survive if it funded.}
#'
#'     }}
#'
#'    \item{tree}{\code{\link[ape]{phylo}} phylogenetic tree for the species.}
#'
#'  }
#'
#' @export
ppp_simulate_data <- function(number_species, cost_mean = 100, cost_sd = 5,
                              success_min_probability = 0.7,
                              success_max_probability = 0.99,
                              funded_min_persistence_probability = 0.5,
                              funded_max_persistence_probability = 0.9,
                              not_funded_min_persistence_probability = 0.01,
                              not_funded_max_persistence_probability = 0.4,
                              locked_in_proportion = 0,
                              locked_out_proportion = 0) {
  # assert that arguments are valid
  assertthat::assert_that(
    assertthat::is.count(number_species),
    isTRUE(is.finite(number_species)),
    assertthat::is.number(cost_mean),
    isTRUE(cost_mean > 0),
    assertthat::is.number(cost_sd),
    isTRUE(cost_sd > 0),
    assertthat::is.number(success_min_probability),
    isTRUE(success_min_probability >= 0),
    isTRUE(success_min_probability <= 1),
    assertthat::is.number(success_max_probability),
    isTRUE(success_max_probability >= 0),
    isTRUE(success_max_probability <= 1),
    isTRUE(success_max_probability > success_min_probability),
    assertthat::is.number(funded_min_persistence_probability),
    isTRUE(funded_min_persistence_probability >= 0),
    isTRUE(funded_min_persistence_probability <= 1),
    assertthat::is.number(funded_max_persistence_probability),
    isTRUE(funded_max_persistence_probability >= 0),
    isTRUE(funded_max_persistence_probability <= 1),
    isTRUE(funded_max_persistence_probability >
           funded_min_persistence_probability),
    assertthat::is.number(not_funded_min_persistence_probability),
    isTRUE(not_funded_min_persistence_probability >= 0),
    isTRUE(not_funded_min_persistence_probability <= 1),
    assertthat::is.number(not_funded_max_persistence_probability),
    isTRUE(not_funded_max_persistence_probability >= 0),
    isTRUE(not_funded_max_persistence_probability <= 1),
    isTRUE(not_funded_max_persistence_probability >
           not_funded_min_persistence_probability),
    isTRUE(funded_min_persistence_probability >
           not_funded_max_persistence_probability),
    assertthat::is.number(locked_in_proportion),
    isTRUE(locked_in_proportion >= 0),
    isTRUE(locked_in_proportion <= 1),
    assertthat::is.number(locked_out_proportion),
    isTRUE(locked_out_proportion >= 0),
    isTRUE(locked_out_proportion <= 1))
    assertthat::assert_that(
      isTRUE(number_species >
             (ceiling(number_species * locked_in_proportion) +
             ceiling(number_species * locked_out_proportion))),
      msg = paste("combined number of locked in and locked out projects",
                  "exceeds the total number of projects."))

  # create project data
  project_data <- data.frame(
    name = c(paste0("S", seq_len(number_species), "_project"),
                    "baseline_project"),
    cost = c(rnorm(number_species, cost_mean, cost_sd), 0),
    success = c(runif(number_species, success_min_probability,
                      success_max_probability), 1),
    stringsAsFactors = FALSE)
  assertthat::assert_that(all(project_data$cost >= 0),
  msg = paste("some projects have subzero costs, increase the argument to",
              "cost_mean and try again"))

  # assign locked in projects
  project_data$locked_in <- FALSE
  if (locked_in_proportion > 1e-10) {
    l <- sample.int(number_species, ceiling(number_species *
                                            locked_in_proportion))
    project_data$locked_in[l] <- TRUE
  }

  # assign locked out projects
  project_data$locked_out <- FALSE
  if (locked_out_proportion > 1e-10) {
    l <- sample(which(!project_data$locked_in),
                ceiling(number_species * locked_out_proportion))
    project_data$locked_out[l] <- TRUE
  }

  # phylogenetic tree
  tree <- ape::rcoal(n = number_species,
                     tip.label = paste0("S", seq_len(number_species)))

  ## species persistence probabilities
  spp_prob_matrix <- matrix(0, ncol = number_species,
                            nrow = number_species + 1,
                            dimnames = list(NULL, sort(tree$tip.label)))
   diag(spp_prob_matrix) <- runif(number_species,
                                 funded_min_persistence_probability,
                                 funded_max_persistence_probability)
  spp_prob_matrix[nrow(spp_prob_matrix), ] <-
    runif(number_species, not_funded_min_persistence_probability,
          not_funded_max_persistence_probability)
  project_data <- cbind(project_data, as.data.frame(spp_prob_matrix))

  ## return result
  list(project_data = tibble::as_tibble(project_data), tree = tree)
}
