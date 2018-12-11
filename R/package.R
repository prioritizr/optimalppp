#' @include internal.R
NULL

#' @useDynLib optimalppp, .registration = TRUE
NULL

#' @importFrom ape plot.phylo
#' @export
NULL

#' \pkg{optimalppp}
#'
#' @section Description:
#' The \pkg{optimalppp} \emph{R} package provides methods for prioritizing
#' funding of
#' conservation projects using the 'Protect Prioritization Protocol'. Projects
#' can be prioritized by maximizing expected species richness or expected
#' phylogenetic diversity. Prioritizations can be generated using a range of
#' methods, such as exact algorithms which can identify optimal solutions,
#' heuristic algorithms which have conventionally been used to identify
#' solutions, and by randomly funding actions. This package also provides the
#' functionality to visualize how well solutions maintain biodiversity.
#'
#' To make the most of this package, the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} and
#' \href{http://www.gurobi.com/documentation/8.1/refman/r_api_overview.html}{\pkg{gurobi}} R packages will need to be installed.
#' Since the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} package is exclusively available
#' at \href{https://bioconductor.org}{Bioconductor}---and is not available on
#' \href{https://cran.r-project.org/}{The Comprehensive R Archive Network}---please
#' execute the following command to install it:
#' \code{source("https://bioconductor.org/biocLite.R");biocLite("ggtree")}.
#' If the installation process fails, please consult the
#' \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{package's online documentation}. To install the \pkg{gurobi} package, the
#' \href{https://www.gurobi.com}{Gurobi} optimization suite will first need to
#' be installed (see instructions for \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/software_installation_guid.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/software_installation_guid.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/software_installation_guid.html}{Windows} operating systems). Although
#' \href{https://www.gurobi.com}{Gurobi} is a commercial software, academics
#' can obtain a
#' \href{https://user.gurobi.com/download/licenses/free-academic}{special license for no cost}. After installing the
#' \href{https://www.gurobi.com}{Gurobi} optimization suite, the \pkg{gurobi}
#'  package can then be installed (see instructions for \href{http://www.gurobi.com/documentation/8.1/quickstart_linux/r_installing_the_r_package.html}{Linux},
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_mac/r_installing_the_r_package.html}{Mac OSX}, and
#'   \href{http://www.gurobi.com/documentation/8.1/quickstart_windows/r_installing_the_r_package.html}{Windows} operating systems).
#'
#' @section Overview:
#' This package provides functions for generating, evaluating, and visualizing
#' prioritizations using 'expected weighted species richness' and 'expected
#' phylogenetic diversity' metrics of conservation benefit. For convenience,
#' the main functions in the package are listed below.
#'
#' \strong{Expected weighted species richness:}
#'
#'     \describe{
#'
#'        \item{\code{\link{ppp_exact_spp_solution}}}{
#'          Prioritize conservation projects by maximizing expected weighted
#'          species richness using exact algorithms.}
#'
#'        \item{\code{\link{ppp_heuristic_spp_solution}}}{
#'          Prioritize conservation projects by maximizing expected weighted
#'          species richness using heuristic algorithms.}
#'
#'        \item{\code{\link{ppp_manual_spp_solution}}}{
#'          Manually prioritize conservation projects under expected weighted
#'          species richness.}
#'
#'        \item{\code{\link{ppp_random_spp_solution}}}{Randomly prioritize
#'          conservation projects under expected weighted species
#'          richness.}
#'
#'     }
#'
#'   \strong{Expected phylogenetic diversity:}
#'
#'     \describe{
#'
#'        \item{\code{\link{ppp_exact_phylo_solution}}}{
#'          Prioritize conservation projects by maximizing expected phylogenetic
#'          diversity with exact algorithms.}
#'
#'        \item{\code{\link{ppp_heuristic_phylo_solution}}}{
#'          Prioritize conservation projects by maximizing expected phylogenetic
#'          diversity with heuristic algorithms.}
#'
#'        \item{\code{\link{ppp_manual_phylo_solution}}}{
#'          Manually prioritize conservation projects under expected
#'          phylogenetic diversity.}
#'
#'        \item{\code{\link{ppp_random_phylo_solution}}}{
#'           Randomly prioritize conservation projects under expected
#'           phylogenetic diversity.}
#'
#'     }
#'
#' @seealso Please refer to the package vignette for more information and worked
#'   examples. This can be accessed using the code
#'   \code{vignette("optimalppp")}.
#'
#' @name optimalppp
#'
#' @docType package
NULL
