#' @include internal.R
NULL

#' Solve the 'Project Prioritization Protocol' problem using heuristic
#' algorithms
#'
#' Prioritize funding for conservation projects under the 'Project
#' Prioritization Protocol' (Joseph, Maloney & Possingham 2009) using a stingy
#' heuristic algorithm (Bennett \emph{et al}. 2014).
#' \strong{Although this algorithm can deliver solutions that perform
#' better than random, it is extremely unlikely to identify solutions that
#' are optimal (Underhill 1994; Rodrigues & Gaston 2002).}
#'
#' @inheritParams help
#'
#' @inherit help return
#'
#' @details Briefly, this algorithm works by starting off with all
#' conservation projects selected for funding and then begins iteratively
#' defunding (removing) projects until the budget is met
#' (Joseph, Maloney & Possingham 2009; Bennett \emph{et al}. 2014). In a given
#' iteration,
#' each project is evaluated in terms of the amount of evolutionary
#' history that is expected to be lost per unit cost when the project is not
#' funded (based on the 'expected phylogenetic diversity' metric; Faith 2008),
#' and the project associated with the lowest utility is defunded.
#' Although this algorithm may identify funding schemes that perform
#' better than random, it is worth noting that this algorithm is extremely
#' unlikely to identify optimal solutions. These calculations can be expressed
#' mathematically with the following definitions.
#'
#' To calculate the utility for funding a given project (\eqn{j})
#' among a set of projects (\eqn{J}), let the expected amount of evolutionary
#' history that will persist into the future when all the projects are funded
#' be expressed as \eqn{P(J)}. Also, let the expected amount of evolutionary
#' history that will persist into the future when all the remaining projects
#' are funded except for project \eqn{j} be expressed as \eqn{P(J - j)}.
#' Furthermore, allow the cost for funding project \eqn{j} to be \eqn{C_j}.
#' Given this, the relative benefit (or utility) for funding project \eqn{j}
#' (\eqn{U_j}) in a given iteration can be expressed as:
#'
#' \deqn{U_j = \frac{P(J) - P(J - j)}{C_j}}{U_j = (P(J) - P(J - j)) / C_j}
#'
#' To calculate the expected amount of evolutionary history that will persist
#' into the future for a given set of funded projects, we will adopt a new set
#' of definitions to avoid confusion. Let \eqn{I} represent a given set of
#' funded projects (indexed by \eqn{i}). For example, \eqn{I} could denote all
#' of the projects in a given iteration (\eqn{P(J)}) or all of the project in a
#' given iteration except for a specific project (\eqn{P(J - j)}). Next, let
#' \eqn{P_i} represent the probability of project \eqn{i} being successful if
#' it is funded. Also, let \eqn{S} represent each species (e.g. species;
#  indexed by \eqn{s}). To represent the conservation outcome for funding each
#' project, let \eqn{B_{is}} denote the probability of persistence for the
#' species \eqn{s} if project \eqn{i} is funded and project \eqn{i} is used to
#' conserve that species.
#'
#' The probability that each species will go extinct (\eqn{E_s}) when a given
#' set of projects are funded (\eqn{I}) can then be  expressed as as:
#'
#' \deqn{E_s = 1 - \text{max}(P_1 \times B_{1s}, \ldots, P_I \times B_{Is})}{E_s = 1 - max(P_1 B_{1s}, ..., P_I B_{Is})}
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
#' given set of projects are funded can then be expressed as:
#'
#' \deqn{P(I) = \sum_{b = 0}^{B} L_b \times \big(1 - \prod_{s = 0}^{S} ifelse(T_{bs} == 1, E_s, 1)\big)}{P(I) = sum_{b = 0}^{B} L_b (1 - prod_{s = 0}^{S} ifelse(T_{bs} == 1, E_s, 1))}
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem, see \code{\link{ppp_heuristic_solution}}
#'   \code{\link{ppp_exact_solution}}, and \code{\link{ppp_random_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot}}.
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
#' # load built-in data
#' data(sim_project_data, sim_tree)
#'
#' # print simulated project data set
#' print(sim_project_data)
#'
#' # print simulated phylogenetic tree data set
#' print(sim_tree)
#'
#' # plot the simulated phylogeny
#' plot(sim_tree, main = "simulated phylogeny")
#' \donttest{
#' # find a solution that meets a budget of 300
#' s1 <- ppp_heuristic_solution(sim_project_data, sim_tree, 300,
#'                              "name", "cost", "success")
#'
#' # print solution
#' print(s1)
#'
#' # print the names of which projects were funded
#' print(names(s1)[which(unlist(s1[1, sim_project_data$name]))])
#'
#' # plot solution
#' ppp_plot(sim_project_data, sim_tree, s1, "name", "cost", "success")
#'
#' # find a solution that meets a budget of 300 and allocates
#' # funding for the "S1_project" project. For instance, species "S1" might
#' # be an iconic species that has cultural and economic importance.
#' sim_project_data2 <- sim_project_data
#' sim_project_data2$locked_in <- sim_project_data2$name == "S1_project"
#' s2 <- ppp_heuristic_solution(sim_project_data2, sim_tree, 300,
#'                              "name", "cost", "success",
#'                               locked_in_column_name = "locked_in")
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' ppp_plot(sim_project_data2, sim_tree, s2, "name", "cost", "success")
#'
#' # find a solution that meets a budget of 300 and does not allocate
#' # funding for the "S2_project" project. For instance, species "S2"
#' # might have very little cultural or economic importance. Broadly speaking,
#' # though, it is better to "lock in" "important" species rather than
#' # "lock out" unimportant species.
#' sim_project_data3 <- sim_project_data
#' sim_project_data3$locked_out <- sim_project_data2$name == "S2_project"
#' s3 <- ppp_heuristic_solution(sim_project_data3, sim_tree, 300,
#'                              "name", "cost", "success",
#'                              locked_out_column_name = "locked_out")
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' ppp_plot(sim_project_data3, sim_tree, s3, "name", "cost", "success")
#'
#' # find all solutions from the heuristic algorithm
#' # note we can set the budget higher than the total cost of all the
#' # projects, and the number of solutions to the total number of
#' # projects to achieve this
#' s4 <- ppp_heuristic_solution(sim_project_data, sim_tree,
#'                              sum(sim_project_data$cost) * 1.1,
#'                              "name", "cost", "success",
#'                               number_solutions = nrow(sim_project_data))
#'
#' # print solutions
#' print(s4)
#'
#' # plot solution cost against objective
#' plot(objective ~ cost, data = s4,
#'      main = "Heuristic solutions", xlab = "Cost ($)",
#'      ylab = "Expected phylogenetic diversity")
#' }
#' @export
ppp_heuristic_solution <- function(x, tree, budget,
                                   project_column_name,
                                   cost_column_name,
                                   success_column_name,
                                   locked_in_column_name = NULL,
                                   locked_out_column_name = NULL,
                                   number_solutions = 1L) {
  # assertions
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, "tbl_df"),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.number(budget),
                          is.finite(budget),
                          isTRUE(budget >= 0),
                          assertthat::is.string(project_column_name),
                          assertthat::has_name(x, project_column_name),
                          assertthat::noNA(x[[project_column_name]]),
                          inherits(x[[project_column_name]],
                                   c("character", "factor")),
                          assertthat::is.string(cost_column_name),
                          assertthat::has_name(x, cost_column_name),
                          is.numeric(x[[cost_column_name]]),
                          assertthat::noNA(x[[cost_column_name]]),
                          assertthat::is.string(success_column_name),
                          assertthat::has_name(x, success_column_name),
                          is.numeric(x[[success_column_name]]),
                          assertthat::noNA(x[[success_column_name]]),
                          assertthat::is.count(number_solutions),
                          assertthat::noNA(number_solutions))
  assertthat::assert_that(min(x[[cost_column_name]]) >= 0,
                          msg = "zero cost baseline project missing.")
  if (!is.null(locked_in_column_name))
    assertthat::assert_that(assertthat::is.string(locked_in_column_name),
                            assertthat::has_name(x, locked_in_column_name),
                            is.logical(x[[locked_in_column_name]]),
                            assertthat::noNA(x[[locked_in_column_name]]))
  if (!is.null(locked_out_column_name))
    assertthat::assert_that(assertthat::is.string(locked_out_column_name),
                            assertthat::has_name(x, locked_out_column_name),
                            is.logical(x[[locked_out_column_name]]),
                            assertthat::noNA(x[[locked_out_column_name]]))
  if (!is.null(locked_in_column_name) && !is.null(locked_out_column_name)) {
    assertthat::assert_that(max(x[[locked_out_column_name]] +
                            x[[locked_in_column_name]]) <= 1,
                            msg = "some projects locked in and locked out.")
  }
  assertthat::assert_that(
    all(tree$tip.label %in% names(x)),
    msg = paste("argument to tree contains species that do not appear as",
                "column names in the argument to x:",
                paste(paste0("'", setdiff(tree$tip.label, names(x)), "'"),
                      collapse = ", ")))

  # preliminary data formatting
  ## coerce factor species names to character
  if (is.factor(x[[project_column_name]]))
    x[[project_column_name]] <- as.character(x[[project_column_name]])

  ## check that branches have lengths
  if (is.null(tree$edge.length)) {
    tree$edge.length <- rep(1, nrow(tree$edge))
    warning(paste("tree does not have branch length data,",
                  "all branches are assumed to have equal lengths"))
  }  else {
    assertthat::assert_that(nrow(tree$edge) == length(tree$edge.length))
  }

  ## determine which projects need to be locked
  locked_in <- integer(0)
  locked_out <- integer(0)
  if (!is.null(locked_in_column_name))
    locked_in <- which(x[[locked_in_column_name]])
  if (!is.null(locked_out_column_name))
    locked_out <- which(x[[locked_out_column_name]])
  assertthat::assert_that(sum(x[[cost_column_name]][locked_in]) <= budget,
                          msg = "locked in projects exceed budget.")

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(methods::as(round(spp_probs, 5), "dgCMatrix"))

  # solve the problem
  s <- rcpp_heuristic_solution(spp = spp_probs,
                               budget = budget,
                               branch_matrix = branch_matrix(tree),
                               branch_lengths = tree$edge.length,
                               costs = x[[cost_column_name]],
                               locked_in = locked_in,
                               locked_out = locked_out)
  # prepare results for output
  colnames(s) <- as.character(x[[project_column_name]])
  out <- tibble::as_tibble(s)

  ## format statistics for output
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      objective = ppp_objective_value(x, tree, project_column_name,
                                      success_column_name, out),
      budget = budget,
      cost = rowSums(matrix(x[[cost_column_name]], byrow = TRUE,
                            ncol = ncol(out), nrow = nrow(out)) *
                     as.matrix(out)),
      optimal = NA,
      method = "heuristic"), out))

  ## subset solutions with budget
  out <- out[out$cost <= budget, , drop = FALSE]

  ## sort by best objective
  out <- out[order(out$objective, decreasing = TRUE), , drop = FALSE]

  ## return n best solutions
  out <- out[seq_len(min(nrow(out), number_solutions)), , drop = FALSE]

  ## add solution column
  out$solution <- seq_len(nrow(out))

  ## throw warning if the number of output solutions is not equal to the number
  ## of the requested solution
  if (nrow(out) != number_solutions)
    warning(paste("although", number_solutions, "requested, only", nrow(out),
                  "solutions exist."))

  # return result
  out
}
