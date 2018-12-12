#' @include internal.R
NULL

#' Prioritize conservation projects by maximizing expected phylogenetic
#' diversity with exact algorithms
#'
#' Prioritize funding for conservation projects with phylogenetic data and
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
#'   \href{https://www.gurobi.com}{Gurobi} software suite, the
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
#' The objective of this problem is to maximize the amount of evolutionary
#' history that is expected to remain within a specified period of time (e.g.
#' 100 years; i.e. 'expected phylogenetic diversity'; Faith 2008).
#' Let \eqn{I} represent the set of conservation actions (indexed by \eqn{i}).
#' Let \eqn{C_i} denote the cost for funding action \eqn{i}, and let \eqn{m}
#' define the maximum expenditure (budget) for funding all the actions. Also,
#' let \eqn{S} represent each species (indexed by \eqn{s}). To describe the
#' evolutionary relationships between the species \eqn{s \in S}{s in S},
#' consider a phylogenetic tree that contains species \eqn{s \in S}{s in S}
#' with branches of known lengths. This tree can be described using
#' mathematical notation by letting \eqn{B} represent the branches (indexed by
#' \eqn{b}) with lengths \eqn{L_b} and letting \eqn{T_{bs}} indicate which
#' species \eqn{s \in S}{s in S} are associated with which phylogenetic
#' branches \eqn{b \in B}{b in B} using zeros and ones. Ideally, the set of
#' species \eqn{S} would contain all of the species in the study
#' area---including non-threatened species---to fully account for the benefits
#' for funding different actions.
#'
#' To guide the prioritization, the conservation actions are organized into
#' conservation projects. Let \eqn{J} denote the set of conservation projects
#' (indexed by \eqn{j}), and let \eqn{A_{ij}} denote which actions
#' \eqn{i \in I}{i in I} comprise each conservation project
#' \eqn{j \in J}{j in J} using zeros and ones. Next, let \eqn{P_j} represent
#' the probability of project \eqn{j} being successful if it is funded. Also,
#' let \eqn{B_{sj}} denote the enhanced probability that each species'
#' \eqn{s \in S}{s in S} associated with the project \eqn{j \in J}{j in J}
#' will persist if all of the actions that comprise project \eqn{j} are funded
#' and that project is allocated to species \eqn{s}.
#'
#' The binary control variables \eqn{X_i} in this problem indicate whether each
#' project \eqn{i \in I}{i in I} is funded (variable equal to one) or not
#' (variable equal to zero). The decision variables in this problem are the
#' \eqn{Y_{j}}, \eqn{Z_{sj}}, \eqn{E_s}, and \eqn{R_b} variables.
#' Specifically, the binary \eqn{Y_{j}} variables indicate if project \eqn{j}
#' is funded (variable equal to one) or not (variable equal to zero) based on
#' which actions are funded; the binary \eqn{Z_{sj}} variables indicate
#' if project \eqn{j} is used to manage species \eqn{s} (variable equal to one)
#' or not (variable equal to zero); the semi-continuous \eqn{E_s} variables
#' denote the probability that species \eqn{s} will go extinct; and the
#' semi-continuous \eqn{R_b} variables denote the probability that phylogenetic
#' branch \eqn{b} will remain in the future.
#'
#' Now that we have defined all the data and variables, we can formulate
#' the problem. For convenience, let the symbol used to denote each set also
#' represent its cardinality (e.g. if there are ten species, let \eqn{S}
#' represent the set of ten species and also the number ten).
#'
#' \deqn{
#'   \mathrm{Maximize} \space \sum_{b = 0}^{B} L_b R_b \space
#'   \mathrm{(eqn \space 1a)} \\
#'   \mathrm{Subject \space to} \space R_b = 1 - \prod_{s = 0}^{S}
#'   ifelse(T_{bs} == 1, \space E_s, \space
#'   1) \space \forall \space b \in B \space \mathrm{(eqn \space 1b)} \\
#'   E_s = 1 - \sum_{j = 0}^{J} Z_{sj} P_j B_{sj} \space \forall \space s \in S
#'   \space \mathrm{(eqn \space 1c)} \\
#'   \sum_{i = 0}^{I} C_i \leq m \space \mathrm{(eqn \space 1d)} \\
#'   Z_{sj} \leq Y_{j} \space \forall \space j \in J \space \mathrm{(eqn \space
#'   1e)} \\
#'   \sum_{j = 0}^{J} Z_{sj} = 1 \space \forall \space s \in s \space
#'   \mathrm{(eqn \space 1f)} \\
#'   A_{ij} Y_{j} \leq X_{i} \space \forall \space i \in I, j \in J \space
#'   \mathrm{(eqn \space 1g)} \\
#'   R_{b} \geq 0, R_{b} \leq 1 \space \forall \space b \in B \space
#'   \mathrm{(eqn \space 1h)} \\
#'   E_{s} \geq 0, E_{s} \leq 1 \space \forall \space b \in B \space
#'   \mathrm{(eqn \space 1i)} \\
#'   X_{i}, Y_{j}, Z_{sj} \in [0, 1] \space \forall \space i \in I, j \in J, s
#'   \in S \space \mathrm{(eqn \space 1j)}
#'   }{
#'   Maximize sum_b^B L_b R_b (eqn 1a); Subject to:
#'   R_b = 1 - prod_s^S ifelse(T_{bs} == 1, E_s, 1) for all b in B (eqn 1b),
#'   E_s = 1 - sum_j^J Y_{sj} P_j B_{sj} for all s in S (eqn 1c),
#'   sum_i^I C_i <= m (eqn 1d),
#'   Z_{sj} <= Y_j for all j in J (eqn 1e),
#'   sum_j^J Z_{sj} = 1 for all s in s (eqn 1f),
#'   A_{ij} Y_{j} <= X_{i} for all i I, j in J (eqn 1g),
#'   R_b >= 0, R_b >= 1 for all b in B (eqn 1h),
#'   E_s >= 0, E_s >= 1 for all s in S (eqn 1i),
#'   X_i, Y_j, Z_{sj} in [0, 1] for all i in I, j in J, s in S (eqn 1j)
#'   }
#'
#' The objective (eqn 1a) is to maximize the amount of expected phylogenetic
#' history that will remain in the future. This is expressed as the sum of
#' branch lengths (\eqn{L_b}) weighted by the probability that at least one of
#' the species connected to this branch will not go extinct (\eqn{R_b}).
#' Constraints (eqn 1b) state that the probability that a branch will remain
#' (\eqn{R_b}) is equal to one minus the probability that all species connected
#' to the branch will go extinct. Constraints (eqn 1c) calculate the
#' probability that each species
#' will go extinct according to their allocated project. Constraint (eqn 1d)
#' ensures that the cost of all the funded actions do not exceed the budget.
#' Constraints (eqn 1e) ensure that species can only be allocated to projects
#' that have all of their actions funded. Constraints (eqn 1f) state that each
#' species can only be allocated to a single project. Constraints (eqn 1g)
#' ensure that a project cannot be funded unless all of its actions are funded.
#' Constraints (eqns 1h, 1i) ensure that the probability variables (\eqn{R_b},
#' \eqn{E_s}) are bounded between zero and one. Constraints (eqns 1j) ensure
#' that the action funding (\eqn{X_j}), project funding (\eqn{Y_j}), and project
#' allocation (\eqn{Z_{sj}}) variables are binary.
#'
#' Although this formulation is a mixed integer quadratically constrained
#' programming problem (due to eqn 1b), it can be linearized and then solved
#' using commercial mixed integer programming solvers (e.g. Gurobi). This can
#' be achieved by substituting the product of the species' extinction
#' probabilities (eqn 1b) with the sum of the log species' extinction
#' probabilities and using piecewise linear approximations (described in
#' Hillier & Price 2005 pp. 390--392) to approximate the exponent of this term.
#' Although this means the problem can only be solved to a pre-specified level
#' of precision (controlled via the argument to \code{number_approx_points}),
#' advances in exact algorithm solvers mean that the problem can be solved to a
#' sufficient degree of precision (e.g. \eqn{1 \times 10^{-5}}{1e-5}) in a
#' trivial period of time.
#'
#' @seealso For other methods for solving the 'Project Prioritization Protocol'
#'   problem, see \code{\link{ppp_heuristic_phylo_solution}},
#'   \code{\link{ppp_manual_phylo_solution}}, and
#'   \code{\link{ppp_random_phylo_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot_phylo_solution}}.
#'
#' @references
#' Faith DP (2008) Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}: 1461--1470.
#'
#' Hillier FS & Price CC (2005) \emph{International series in operations
#' research & management science}. Springer.
#'
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
#' data(sim_project_data, sim_action_data, sim_tree)
#'
#' # print simulated project data set
#' print(sim_project_data)
#'
#' # print simulated action data
#' print(sim_action_data)
#'
#' # print simulated phylogenetic tree data set
#' print(sim_tree)
#'
#' # plot the simulated phylogeny
#' plot(sim_tree, main = "simulated phylogeny")
#' \donttest{
#' # verify if guorbi package is installed
#' if (!require(gurobi, quietly = TRUE))
#'  stop("the gurobi R package is not installed.")
#'
#' # find a solution that meets a budget of 300
#' s1 <- ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree,
#'                                300, "name", "success", "name", "cost")
#'
#' # print solution
#' print(s1)
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data, sim_tree, s1,
#'                         "name", "success", "name", "cost")
#'
#' # find a solution that meets a budget of 300 and allocates
#' # funding for the "S3_action" action. For instance, species "S3" might
#' # be an iconic species that has cultural and economic importance.
#' sim_action_data2 <- sim_action_data
#' sim_action_data2$locked_in <- sim_action_data2$name == "S3_action"
#' s2 <- ppp_exact_phylo_solution(sim_project_data, sim_action_data2, sim_tree,
#'                                300, "name", "success", "name", "cost",
#'                                locked_in_column_name = "locked_in")
#'
#' # print solution
#' print(s2)
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data2, sim_tree, s2,
#'                         "name", "success", "name", "cost")
#'
#' # find a solution that meets a budget of 300 and does not allocate
#' # funding for the "S2_action" project. For instance, species "S2"
#' # might have very little cultural or economic importance. Broadly speaking,
#' # though, it is better to "lock in" "important" species rather than
#' # "lock out" unimportant species.
#' sim_action_data3 <- sim_action_data
#' sim_action_data3$locked_out <- sim_action_data3$name == "S2_action"
#' s3 <- ppp_exact_phylo_solution(sim_project_data, sim_action_data3, sim_tree,
#'                                300, "name", "success", "name", "cost",
#'                                locked_out_column_name = "locked_out")
#'
#' # print solution
#' print(s3)
#'
#' # plot solution
#' ppp_plot_phylo_solution(sim_project_data, sim_action_data3, sim_tree, s3,
#'                         "name", "success", "name", "cost")
#'
#' # find the top solutions
#' s4 <- ppp_exact_phylo_solution(sim_project_data, sim_action_data, sim_tree,
#'                                300, "name", "success", "name", "cost",
#'                                number_solutions = 1000)
#'
#' # print solution
#' print(s4)
#' }
#' @export
ppp_exact_phylo_solution <- function(x, y, tree, budget,
                               project_column_name,
                               success_column_name,
                               action_column_name,
                               cost_column_name,
                               locked_in_column_name = NULL,
                               locked_out_column_name = NULL,
                               gap = 0.000001, threads = 1L,
                               number_solutions = 1L,
                               time_limit = .Machine$integer.max,
                               number_approx_points = 300,
                               verbose = FALSE) {
  # assertions
  ## assert that gurobi R package is installed
  assertthat::assert_that(requireNamespace("gurobi", quietly = TRUE),
                          msg = "gurobi R package not installed.")
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
  ## coerce x to tibble if just a regular data.frame
  if (inherits(y, "data.frame") && !inherits(y, "tbl_df"))
    y <- tibble::as_tibble(y)
  ## assert that parameters are valid
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
                          assertthat::is.number(gap),
                          is.finite(gap),
                          isTRUE(gap >= 0),
                          assertthat::is.count(time_limit),
                          is.finite(time_limit),
                          assertthat::is.count(threads),
                          assertthat::is.count(number_solutions),
                          is.finite(number_solutions),
                          assertthat::is.count(number_approx_points),
                          assertthat::is.flag(verbose),
                          !is.na(verbose),
                          !is.na(number_approx_points),
                          !is.na(number_solutions),
                          !is.na(threads),
                          !is.na(time_limit))
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

  # formulate the problem
  f <- rcpp_mip_formulation(
    spp = spp_probs,
    actions = methods::as(as.matrix(x[, y[[action_column_name]],
                                      drop = FALSE]), "dgCMatrix"),
    budget = budget,
    branch_matrix = branch_matrix(tree),
    branch_lengths = tree$edge.length,
    costs = y[[cost_column_name]],
    locked_in = locked_in,
    locked_out = locked_out,
    n_approx_points = number_approx_points)
  # convert constraint matrix to sparse representation
  f$A <- Matrix::sparseMatrix(i = f$Ai, j = f$Aj, x = f$Ax, index1 = FALSE)

  # solve the problem
  s <- gurobi::gurobi(f, list(Presolve = 2,
                              LogToConsole = as.integer(verbose),
                              MIPGap = gap,
                              Threads = threads,
                              TimeLimit = time_limit,
                              LogFile = "",
                              PoolSearchMode = 2,
                              PoolSolutions = number_solutions))

  # verify that solution is feasible
  if (s$status == "INFEASIBLE")
    stop(paste0("problem is infeasible. This should not happen. ",
                "Please file an issue at:\n",
                "  https://github.com/prioritizr/optimalppp/issues"))

  # prepare results for output
  out <- sapply(s$pool, `[[`, "xn", simplify = TRUE)
  if (!is.matrix(out))
    out <- matrix(out, ncol = 1)
  out <- out[seq_len(nrow(y)), , drop = FALSE]
  out <- t(out) > 0.5
  colnames(out) <- y[[action_column_name]]
  out <- tibble::as_tibble(out)

  ## throw warning if the number of output solutions is not equal to the number
  ## of the requested solution
  if (nrow(out) != number_solutions)
    warning(paste("although", number_solutions, "requested, only", nrow(out),
                  "solutions exist."))

  ## format statistics for output
  out2 <- tibble::tibble(
    solution = seq_along(s$pool),
    method = "exact",
    obj = ppp_epd(x, y, tree, out, project_column_name,
                  success_column_name, action_column_name),
    budget = budget,
    cost = rowSums(matrix(y[[cost_column_name]], byrow = TRUE,
                          ncol = nrow(y), nrow = nrow(out)) *
                   as.matrix(out)))
  if (s$status == "OPTIMAL") {
    out2$optimal <- (abs(out2$obj[1] - out2$obj) < 1.0e-5)
  } else {
    out2$optimal <- NA
  }
  out <- tibble::as_tibble(cbind(out2, out))

  # remove duplicates (this is caused by species being assigned to different
  # projects given the same set of funded actions)
  out <- tibble::as_tibble(out[!duplicated(out[, y[[action_column_name]]]), ])

  # reset solution
  out$solution <- seq_along(out$solution)

  ## throw warning if the number of output solutions is not equal to the number
  ## of the requested solution
  if (nrow(out) != number_solutions)
    warning(paste("although", number_solutions, "requested, only", nrow(out),
                  "solutions exist."))

  # return result
  out
}
