#' @include internal.R
NULL

#' Simulated project and phylogenetic data
#'
#' Simulated data for prioritizing conservations projects.
#'
#' @details
#' The data set contains the following objects:
#'
#' \describe{
#'
#'   \item{\code{sim_project_data}}{A \code{\link[base]{data.frame}} containing
#'     data for six simulated conservation projects. Each row corresponds to a
#'     different project and each column contains information about the
#'     projects. This table contains the following columns: \code{"name"} with
#'     the name of each project, \code{"cost"} with the cost of funding each
#'     project, \code{"success"} with the probability of the project succeeding
#'     if it is funded. It also contains columns that are named after each
#'     species (i.e. \code{"S1"}, \code{"S2"}, \code{"S3"}, \code{"S4"}, and
#'     \code{"S5"}) and these columns indicate the probability of each
#'     species persisting if the project is used to enhance its persistence.}
#'
#'   \item{\code{sim_tree}}{A \code{\link[ape]{phylo}} phylogenetic tree
#'     that describes the evolutionary relationships between five simulated
#'     species. These species are named: \code{"S1"}, \code{"S2"}, \code{"S3"},
#'     \code{"S4"}, and \code{"S5"}.}
#'
#' }
#'
#' @aliases sim_tree
#'
#' @usage data(sim_project_data)
#'
#' @usage data(sim_tree)
#'
#' @format \describe{
#'   \item{sim_project_data}{\code{\link[base]{data.frame}} object}
#'   \item{sim_tree}{\code{\link[ape]{phylo}} object.}
#' }
#'
#' @keywords datasets
#'
#' @examples
#' # load data
#' data(sim_project_data, sim_tree)
#'
#' # print project data
#' print(sim_project_data)
#
#' # print phylogenetic tree
#' print(sim_tree)
#'
#' # plot phylogenetic tree
#' plot(sim_tree)
#'
#' @name sim_data
NULL

#' @rdname sim_data
"sim_project_data"

#' @rdname sim_data
"sim_tree"
