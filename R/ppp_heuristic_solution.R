#' @include internal.R
NULL

#' Solve the 'Project Prioritization Protocol' problem using heuristic
#' algorithms
#'
#' Prioritize funding for conservation projects using a stingy heuristic
#' algorithm (Bennett \emph{et al}. 2014; Joseph \emph{et al. 2009}).
#' \strong{Although this algorithm can deliver solutions that perform
#' better than random, it is extremely unlikely to identify solutions that
#' are optimal.}.
#'
#' @inheritParams ppp_gurobi_solution
#'
#' @inherit ppp_gurobi_solution return
#'
#' @details Briefly, this algorithm works by starting off with all
#' conservation projects selected for funding and then begins iteratively
#' defunding (removing) projects until the budget is met (Bennett \emph{et al}.
#' 2014; Joseph \emph{et al. 2009}). In a given iteration,
#' each project is evaluated in terms of the amount of evolutionary
#' history that is expected to be lost per unit cost when the project is not
#' funded. Although this algorithm may identify funding schemes that perform
#' better than random, it is worth noting that this algorithm is extremely
#' unlikely to identify optimal solutions. These calculations can be expressed
#' mathematically using the follow definitions.
#'
#' To calculate the relative benefit for funding a given project (\eqn{j})
#' among a set of projects (\eqn{J}), let the expected amount of evolutionary
#' history that will persist into the future when all the projects are funded
#' be expressed as \eqn{P(J)}. Also, let the expected amount of evolutionary
#' history that will persist into the future when all the remaining projects
#' are funded except for project \eqn{j} be expressed as \eqn{P(J - j)}.
#' Furthermore, allow the cost for funding project \eqn{j} to be \eqn{C_j}.
#' Given this, the benefit for funding project \eqn{j} (\eqn{B_j}) in a given
#' iteration can be expressed as:
#'
#' \deqn{B_j = \big(P(J) - P(J - j)\big) / C_j}{B_j = (P(J) - P(J - j)) / C_j}
#'
#' To calculate the expected amount of evolutionary history that will persist
#' into the future for a given set of funded projects, we will adopt a new set
#' of definitions to avoid confusion. Let \eqn{I} represent a given set of
#' funded projects (indexed by \eqn{i}). For example, \eqn{I} could denote all
#' of the projects in a given iteration (\eqn{P(J)}) or all of the project in a
#' given iteration except for a specific project (\eqn{P(J - j)}). Next, let
#' \eqn{P_i} represent the probability of project \eqn{i} being successful if
#' it is funded. Also, let $S$ represent each species (e.g. species; indexed by
#' \eqn{s}). To represent the conservation outcome for funding each project,
#' let $B_{is}$ denote the probability of persistence for the species \eqn{s} if
#' project \eqn{i} is funded and project $i$ is used to conserve that species.
#'
#' The probability that each species will go extinct (\eqn{E_s}) when a given
#' set of projects are funded (\eqn{I}) can then be  expressed as as:
#'
#' \deqn{E_s = 1 - \text{max}\(B_{1s}, \text{\ldots}, B_{Is}\)}{E_s = 1 - max(B_{1s}, ..., B_{Is})}
#'
#' To account for the phylogenetic contributions of funding a project,
#' consider a phylogenetic tree that contains species \eqn{s \in S}{s in S} and
#' contains branches with known lengths. To describe the tree using mathematical
#' notation, let \eqn{B} represent the branches (indexed by \eqn{b}) with
#' lengths \eqn{L_b} and let \eqn{T_{bs}} indicate which species
#' \eqn{s \in S}{s in S} are associated with which
#' phylogenetic branches \eqn{b \in B}{b in B} (using zeros and ones).
#'
#' The amount of evolutionary history that is expected to persist when a
#' given set of projects are funded can be expressed as:
#'
#' \deqn{P\(I\) = \sum_{b = 0}^{B} L_b \times \\ \(1 - \prod_{s = 0}^{S} ifelse\(T_{bs} == 1, E_s, 1\)\)}{P(I) = sum_{b = 0}^{B} L_b (1 - prod_{s = 0}^{S} ifelse(T_{bs} == 1, E_s, 1))}
#'
#' @seealso For other methods for solving the 'Project Prioritization Protocol'
#' problem, see \code{\link{ppp_heuristic_solution}} and
#' \code{\link{ppp_gurobi_solution}}. To visualize the effectiveness of a
#' particular solution, see \code{\link{ppp_plot}}.
#
#' @references
#' Bennett, J. R., Elliott, G., Mellish, B., Joseph, L. N., Tulloch, A. I.,
#' Probert, W. J., ... & Maloney, R. (2014). Balancing phylogenetic diversity
#' and species numbers in conservation prioritization, using a case study of
#' threatened species in New Zealand. \emph{Biological Conservation},
#' \strong{174}, 47-54.
#'
#' Joseph, L. N., Maloney, R. F., & Possingham, H. P. (2009). Optimal
#' allocation of resources among threatened species: a project prioritization
#' protocol. \emph{Conservation biology}, \strong{23}, 328--338.
#'
#' @export
ppp_heuristic_solution <- function() {
 stop("TODO")
}
