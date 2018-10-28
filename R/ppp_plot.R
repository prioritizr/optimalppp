#' @include internal.R
NULL

#' Plot a 'Project Prioritization Protocol' solution
#'
#' Create a plot displaying a phylogenetic tree (phylogram) to visualize a
#' solution to the 'Project Prioritization Protocol' problem. In this plot, each
#' phylogenetic branch is colored according to probability
#' that it is expected to persist into the future. Additionally, species that
#' benefit from at least a single funded project with a non-zero cost are
#' denoted with large labels.
#'
#' @inheritParams help
#'
#' @details This function requires the \pkg{ggtree} package (). Since the
#'   \pkg{ggtree} package is distributed exclusively through
#'   \href{https://bioconductor.org}{Bioconductor}, please execute the
#'   following commands to install it:
#'   \code{source("https://bioconductor.org/biocLite.R");biocLite("ggtree")}.
#'   If the installation process fails, please consult the \pkg{ggtree}
#'   package's
#'   \href{online documentation}{https://bioconductor.org/packages/release/bioc/html/ggtree.html}.
#'   For more information on customizing the plot, please refer to the package's
#'   vignettes.
#'
#' @seealso To generate solutions for the 'Project
#'   Prioritization Protocol' problem, see \code{\link{ppp_heuristic_solution}}
#'   \code{\link{ppp_gurobi_solution}}, \code{\link{ppp_manual_solution}},
#'   or \code{\link{ppp_random_solution}}.
#'
#' @references
#' Yu, G., Smith, D. K., Zhu, H., Guan, Y., & Lam, T. T. Y. (2017). ggtree: An
#' R package for visualization and annotation of phylogenetic trees with their
#' covariates and other associated data. \emph{Methods in Ecology and
#' Evolution}, \strong{8}, 28--36.
#'
#' @export
ppp_plot <- function(x, tree, project_column_name, success_column_name,
                     solution, n = 1) {
  stop("TODO")
}
