#' @include internal.R
NULL

#' @useDynLib optimalppp, .registration = TRUE
NULL

#' \pkg{optimalppp}
#'
#' @description
#' Global biodiversity is declining (Ceballos, Ehrlich, & Dirzo 2017). Since
#' the resources available for preventing further extinctions are limited
#' (Balmford \emph{et al.} 2003; Bottrill \emph{et al.} 2009), there has been
#' increasing interest in understanding the best way to allocate funding for
#' conservation projects (Joseph, Maloney, & Possingham 2009; Bennett \emph{et
#' al.} 2014; Chadés \emph{et al.} 2015; Tulloch \emph{et al.} 2015). These
#' projects---such as baiting or trapping invasive species critical habitats,
#' or rearing captive individuals for reintroduction---are each associated with
#' (i) an economic cost, (ii) a probability that they will succeed, and
#' (iii) an enhanced probability that their target species' will persist
#' into the future if the project succeeds. The problem---coined the 'Project
#' Prioritization Protocol' (Joseph, Maloney, & Possingham 2009)---is then:
#' which projects should be prioritized for funding given a known budget? One
#' approach is to to prioritize the combination of conservation projects that
#' will maximize the total amount evolutionary history that is expected to
#' persist in the study area (Faith 2008).
#'
#' The \pkg{optimalppp} package provides methods for solving the 'Project
#' Prioritization Protocol' and visualizing the implications of different
#' funding schemes. Here, you can generate solutions by using a stingy
#' heuristic algorithm (\code{\link{ppp_heuristic_solution}}; similar to
#' Bennett \emph{et al.} 2014), or using exact algorithms
#' (\code{\link{ppp_exact_solution}}) which are
#' guaranteed to deliver optimal solutions. You can also generate solutions
#' by randomly selecting projects within a given budget
#' (\code{\link{ppp_random_solution}}), and evaluate solutions identified
#' through other means (e.g. expert elicitation;
#' \code{\link{ppp_manual_solution}}). To visualize how well a set of
#' priority projects will maintain local biodiversity, you can also
#' generate plots showing phylogenetic tree (\code{\link{ppp_plot}}).
#' Furthermore, execute the command \code{vignette("optimalppp")} to view a
#' short tutorial on using this software.
#'
#' To make the most of this package, the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}},
#' \href{https://bioconductor.org/packages/release/bioc/html/treeio.html}{\pkg{treeio}}, and
#' \href{http://www.gurobi.com/documentation/8.1/refman/r_api_overview.html}{\pkg{gurobi}} R packages will need to be installed.
#' Since the \href{https://bioconductor.org/packages/release/bioc/html/ggtree.html}{\pkg{ggtree}} and \href{https://bioconductor.org/packages/release/bioc/html/treeio.html}{\pkg{treeio}} packages are exclusively available
#' at \href{https://bioconductor.org}{Bioconductor}---and are not available on
#' \href{https://cran.r-project.org/}{Comprehensive R Archive Network}---please
#' execute the following commands to install them:
#' \code{source("https://bioconductor.org/biocLite.R");biocLite("ggtree")}.
#' If the installation process fails, please consult the
#' packages' online documentation. To install the \pkg{gurobi} package, the
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
#' @references
#' Balmford, A., Gaston, K. J., Blyth, S., James, A., & Kapos, V. (2003).
#' Global variation in terrestrial conservation costs, conservation benefits,
#' and unmet conservation needs. \emph{Proceedings of the National Academy of
#' Sciences}, \strong{100}, 1046--1050.
#'
#' Bennett, J. R., Elliott, G., Mellish, B., Joseph, L. N., Tulloch, A. I.,
#' Probert, W. J., ... & Maloney, R. (2014). Balancing phylogenetic diversity
#' and species numbers in conservation prioritization, using a case study of
#' threatened species in New Zealand. \emph{Biological Conservation},
#' \strong{174}, 47--54.
#'
#' Bottrill, M. C., Joseph, L. N., Carwardine, J., Bode, M., Cook, C., Game, E.
#' T., ... & Pressey, R. L. (2009). Finite conservation funds mean triage is
#' unavoidable. \emph{Trends in Ecology & Evolution}, \strong{24}, 183--184.
#'
#' Ceballos, G., Ehrlich, P. R., & Dirzo, R. (2017). Biological annihilation
#' via the ongoing sixth mass extinction signaled by vertebrate population
#' losses and declines. \emph{Proceedings of the National Academy of
#' Sciences}, \strong{114}, E6089--E6096.
#'
#' Chadés, I., Nicol, S., van Leeuwen, S., Walters, B., Firn, J., Reeson, A.,
#' ... & Carwardine, J. (2015). Benefits of integrating complementarity into
#' priority threat management. \emph{Conservation Biology}, \strong{29},
#' 525--536.
#'
#' Faith, D. P. (2008). Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}, 1461--1470.
#'
#' Joseph, L. N., Maloney, R. F., & Possingham, H. P. (2009). Optimal
#' allocation of resources among threatened species: a project prioritization
#' protocol. \emph{Conservation Biology}, \strong{23}, 328--338.
#'
#' Tulloch, A. I., Maloney, R. F., Joseph, L. N., Bennett, J. R., Di Fonzo, M.
#' M., Probert, W. J., ... & Possingham, H. P. (2015). Effect of risk aversion
#' on prioritizing conservation projects. \emph{Conservation Biology},
#' \strong{29}, 513--524.
#' @name optimalppp
#'
#' @docType package
NULL
