#' @include internal.R
NULL

#' Prioritize conservation projects with phylogenetic data using heuristic
#' algorithms
#'
#' Prioritize funding for conservation projects using the 'Project
#' Prioritization Protocol' (Joseph, Maloney & Possingham 2009) with
#' phylogenetic data and using a stingy heuristic algorithm (Bennett
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
#' funded to maximize the amount of evolutionary history that is expected
#' to persist into the future. Briefly, this algorithm works by starting off
#' with all conservation actions selected for funding and then begins
#' iteratively defunding (removing) actions until the budget is met
#' (Joseph, Maloney & Possingham 2009; Bennett \emph{et al}. 2014). In a given
#' iteration, each action is evaluated in terms of the amount of evolutionary
#' history that is expected to be lost per unit cost when the action is not
#' funded (based on the 'expected phylogenetic diversity' metric; Faith 2008),
#' and the action associated with the lowest utility is defunded. Since
#' projects are only considered funded when all of their associated actions are
#' also funded---and species only receive benefits from projects that are
#' funded, and not individual conservation actions---by iteratively removing
#' actions according to their expected utility, this algorithm may identify
#' cost-effective funding schemes. Note, however, that this algorithm is
#' extremely unlikely to identify optimal solutions.
#'
#' The calculations that underpin this algorithm can be expressed
#' mathematically. To calculate the utility for funding a given action (\eqn{L})
#' among a set of actions (\eqn{L}), let the expected amount of evolutionary
#' history that will persist into the future when all the actions are funded
#' be expressed as \eqn{A(L)}. Also, let the expected amount of evolutionary
#' history that will persist into the future when all the remaining actions
#' are funded except for action \eqn{l} be expressed as \eqn{A(L - l)}.
#' Furthermore, allow the cost for funding action \eqn{l} to be \eqn{C_l}.
#' Given this, the relative benefit (or utility) for funding action \eqn{l}
#' (\eqn{U_l}) in a given iteration can be expressed as:
#'
#' \deqn{U_l = \frac{A(L) - A(L - l)}{C_l}}{A_l = (A(L) - A(L - l)) / C_l}
#'
#' To calculate the expected amount of evolutionary history that will persist
#' into the future for a given set of funded actions, we will adopt a new set
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
#' funded project for that species).
#'
#' The probability that each species will go extinct (\eqn{E_s}) when a given
#' set of projects are funded (\eqn{J}) can then be  expressed as as:
#'
#' \deqn{E_s = 1 - \mathrm{max}(P_1 \times B_{1s}, \ldots, P_J \times B_{Js})}{E_s = 1 - max(P_1 B_{1s}, ..., P_J B_{Js})}
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
#' \deqn{A(I) = \sum_{b = 0}^{B} L_b \times \big(1 - \prod_{s = 0}^{S}
#'   ifelse(T_{bs} == 1, E_s, 1)\big)}{A(I) = sum_{b = 0}^{B} L_b (1 -
#'   prod_{s = 0}^{S} ifelse(T_{bs} == 1, E_s, 1))}
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem using phylogenetic data, see
#'   \code{\link{ppp_exact_phylo_solution}}
#'   \code{\link{ppp_manual_phylo_solution}}, and
#'   \code{\link{ppp_random_phylo_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot_phylo_solution}}.
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
#' data(sim_project_data, sim_action_data, sim_tree)
#'
#' # print simulated project data
#' print(sim_project_data)
#'
#' # print simulated action data
#' print(sim_action_data)
#'
#' # print simulated phylogenetic tree data
#' print(sim_tree)
#'
#' # plot the simulated phylogeny
#' plot(sim_tree, main = "simulated phylogeny")
#' \donttest{
#' # find a solution that meets a budget of 300
#' s1 <- ppp_heuristic_phylo_solution(sim_project_data, sim_action_data,
#'                                    sim_tree, 300, "name", "success", "name",
#'                                    "cost")
#'
#' # print solution
#' print(s1)
#'
#' # print the names of which actions were funded
#' print(names(s1)[which(unlist(s1[1, sim_action_data$name]))])
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s1,
#'                         "name", "success", "name", "cost")
#'
#' # find a solution that meets a budget of 300 and allocates
#' # funding for the "S1_action" action. For instance, species "S1" might
#' # be an iconic species that has cultural and economic importance.
#' sim_action_data2 <- sim_action_data
#' sim_action_data2$locked_in <- sim_action_data2$name == "S1_action"
#' s2 <- ppp_heuristic_phylo_solution(sim_project_data, sim_action_data2,
#'                                    sim_tree, 300, "name", "success", "name",
#'                                    "cost",
#'                                    locked_in_column_name = "locked_in")
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data2, sim_tree, s2,
#'                         "name", "success", "name", "cost")
#'
#' # find a solution that meets a budget of 300 and does not allocate
#' # funding for the "S2_action" action. For instance, species "S2"
#' # might have very little cultural or economic importance. Broadly speaking,
#' # though, it is better to "lock in" "important" species rather than
#' # "lock out" unimportant species.
#' sim_action_data3 <- sim_action_data
#' sim_action_data3$locked_out <- sim_action_data3$name == "S2_action"
#' s3 <- ppp_heuristic_phylo_solution(sim_project_data, sim_action_data3,
#'                                    sim_tree, 300, "name", "success", "name",
#'                                    "cost",
#'                                    locked_out_column_name = "locked_out")
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data3, sim_tree, s3,
#'                         "name", "success", "name", "cost")
#'
#' # find all solutions from the heuristic algorithm
#' # note we can set the budget higher than the total cost of all the
#' # projects, and the number of solutions to the total number of
#' # projects to achieve this
#' s4 <- ppp_heuristic_phylo_solution(sim_project_data, sim_action_data,
#'                                    sim_tree, sum(sim_action_data$cost) * 1.1,
#'                                    "name", "success", "name", "cost",
#'                                    number_solutions = nrow(sim_action_data))
#'
#' # print solutions
#' print(s4)
#'
#' # plot solution cost against expected phylogenetic diversity
#' plot(obj ~ cost, data = s4,
#'      main = "Heuristic solutions", xlab = "Cost ($)",
#'      ylab = "Expected phylogenetic diversity")
#' }
#' @export
ppp_heuristic_phylo_solution <- function(x, y, tree, budget,
                                         project_column_name,
                                         success_column_name,
                                         action_column_name,
                                         cost_column_name,
                                         locked_in_column_name = NULL,
                                         locked_out_column_name = NULL,
                                         number_solutions = 1L) {
  # assertions
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
  ## coerce x to tibble if just a regular data.frame
  if (inherits(y, "data.frame") && !inherits(y, "tbl_df"))
    y <- tibble::as_tibble(y)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, "tbl_df"),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(y, "tbl_df"),
                          ncol(y) > 0, nrow(y) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.number(budget),
                          is.finite(budget),
                          isTRUE(budget >= 0),
                          assertthat::is.string(project_column_name),
                          assertthat::has_name(x, project_column_name),
                          assertthat::noNA(x[[project_column_name]]),
                          inherits(x[[project_column_name]],
                                   c("character", "factor")),
                          assertthat::is.string(success_column_name),
                          assertthat::has_name(x, success_column_name),
                          is.numeric(x[[success_column_name]]),
                          assertthat::noNA(x[[success_column_name]]),
                          all(x[[success_column_name]] >= 0),
                          all(x[[success_column_name]] <= 1),
                          assertthat::is.string(action_column_name),
                          assertthat::has_name(y, action_column_name),
                          assertthat::noNA(y[[action_column_name]]),
                          inherits(y[[action_column_name]],
                                   c("character", "factor")),
                          all(as.character(y[[action_column_name]]) %in%
                              names(x)),
                          assertthat::is.string(cost_column_name),
                          assertthat::has_name(y, cost_column_name),
                          is.numeric(y[[cost_column_name]]),
                          assertthat::noNA(y[[cost_column_name]]),
                          all(y[[cost_column_name]] >= 0),
                          assertthat::is.count(number_solutions),
                          assertthat::noNA(number_solutions))
  assertthat::assert_that(min(y[[cost_column_name]]) >= 0,
                          msg = "zero cost baseline project missing.")
  ## coerce factor project names to character
  if (is.factor(x[[project_column_name]]))
    x[[project_column_name]] <- as.character(x[[project_column_name]])
  ## coerce factor action names to character
  if (is.factor(y[[action_column_name]]))
    y[[action_column_name]] <- as.character(y[[action_column_name]])
  ## locked in checks
  if (!is.null(locked_in_column_name))
    assertthat::assert_that(assertthat::is.string(locked_in_column_name),
                            assertthat::has_name(y, locked_in_column_name),
                            is.logical(y[[locked_in_column_name]]),
                            assertthat::noNA(y[[locked_in_column_name]]))
  if (!is.null(locked_out_column_name))
    assertthat::assert_that(assertthat::is.string(locked_out_column_name),
                            assertthat::has_name(y, locked_out_column_name),
                            is.logical(y[[locked_out_column_name]]),
                            assertthat::noNA(y[[locked_out_column_name]]))
  if (!is.null(locked_in_column_name) && !is.null(locked_out_column_name)) {
    assertthat::assert_that(max(y[[locked_out_column_name]] +
                            y[[locked_in_column_name]]) <= 1,
                            msg = "some projects locked in and locked out.")
  }
  ## species name checks
  assertthat::assert_that(
    all(tree$tip.label %in% names(x)),
    msg = paste("argument to tree contains species that do not appear as",
                "column names in the argument to x:",
                paste(paste0("'", setdiff(tree$tip.label, names(x)), "'"),
                      collapse = ", ")))
  ## check that branches have lengths
  if (is.null(tree$edge.length)) {
    tree$edge.length <- rep(1, nrow(tree$edge))
    warning(paste("tree does not have branch length data,",
                  "all branches are assumed to have equal lengths"))
  }  else {
    assertthat::assert_that(nrow(tree$edge) == length(tree$edge.length))
  }
  ## additional column checks
  assertthat::assert_that(is.logical(as.matrix(
    x[, y[[action_column_name]], drop = FALSE])))
  assertthat::assert_that(all(!is.na(as.matrix(
    x[, y[[action_column_name]], drop = FALSE]))))
  assertthat::assert_that(is.numeric(as.matrix(
    x[, tree$tip.label, drop = FALSE])))
  assertthat::assert_that(assertthat::noNA(
    as.matrix(x[, tree$tip.label, drop = FALSE])))
  assertthat::assert_that(min(as.matrix(x[, tree$tip.label,
                                          drop = FALSE])) >= 0)
  assertthat::assert_that(max(as.matrix(x[, tree$tip.label,
                                          drop = FALSE])) <= 1)
  # preliminary data formatting
  ## determine which actions need to be locked
  locked_in <- integer(0)
  locked_out <- integer(0)
  if (!is.null(locked_in_column_name))
    locked_in <- which(y[[locked_in_column_name]])
  if (!is.null(locked_out_column_name))
    locked_out <- which(y[[locked_out_column_name]])
  assertthat::assert_that(sum(y[[cost_column_name]][locked_in]) <= budget,
                          msg = "locked in actions exceed budget.")

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(methods::as(round(spp_probs, 5), "dgCMatrix"))

  # solve the problem
  s <- rcpp_heuristic_phylo_solution(
    spp = spp_probs,
    actions = as(as.matrix(x[, y[[action_column_name]], drop = FALSE]),
                 "dgCMatrix"),
    budget = budget,
    branch_matrix = branch_matrix(tree),
    branch_lengths = tree$edge.length,
    costs = y[[cost_column_name]],
    locked_in = locked_in,
    locked_out = locked_out)

  # prepare results for output
  ## add column name
  colnames(s) <- as.character(y[[action_column_name]])
  out <- tibble::as_tibble(s)

  ## format statistics for output
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      method = "heuristic",
      obj = ppp_epd(x, y, tree, out, project_column_name,
                    success_column_name, action_column_name),
      budget = budget,
      cost = rowSums(matrix(y[[cost_column_name]], byrow = TRUE,
                            ncol = nrow(y), nrow = nrow(s)) *
                     as.matrix(out)),
      optimal = NA),
    out))

  ## subset solutions with budget
  out <- out[out$cost <= budget, , drop = FALSE]

  ## sort by best objective
  out <- out[order(out$obj, decreasing = TRUE), , drop = FALSE]

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
  out[, c(ncol(out), seq(1, ncol(out) - 1)), drop = FALSE]
}
