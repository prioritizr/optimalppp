#' @include internal.R
NULL

#' Solve the 'Project Prioritization Protocol' problem using Gurobi
#'
#' Prioritize funding for conservation projects using the
#' \href{https://www.gurobi.com}{Gurobi optimization software suite}. Unlike
#' other methods for generating prioritizations, this method can identify
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
#'   Specifically, the problem aims to maximize
#'   the amount of evolutionary history that is expected to
#'   persist (i.e. the 'expected phylogenetic diversity' metric; Faith 2008).
#'   This is achieved by funding different conservation projects, with
#'   known costs, that have a known effect on species' survival. Please refer
#'   to the package vignette for the complete formulation of this problem.
#'
#'   Although \href{https://www.gurobi.com}{Gurobi} is a commercial software,
#'   academics can obtain a \href{https://user.gurobi.com/download/licenses/free-academic}{special license for no cost}.
#'    After downloading and installing the
#'   href{https://www.gurobi.com}{Gurobi} software suite, the
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
#' @seealso For other methods for solving the 'Project Prioritization Protocol'
#'   problem, see \code{\link{ppp_heuristic_solution}},
#'   \code{\link{ppp_manual_solution}}, and \code{\link{ppp_random_solution}}.
#'   To visualize the effectiveness of a particular solution, see
#'   \code{\link{ppp_plot}}.
#'
#' @references
#' Faith, D. P. (2008). Threatened species and the potential loss of
#' phylogenetic diversity: conservation scenarios based on estimated extinction
#' probabilities and phylogenetic risk analysis. \emph{Conservation Biology},
#' \strong{22}, 1461--1470.
#'
#' Rodrigues, A. S., & Gaston, K. J. (2002). Optimisation in reserve selection
#' procedures---why not?. \emph{Biological Conservation}, \strong{107}, 123-129.
#'
#' Underhill, L. G. (1994). Optimal and suboptimal reserve selection
#' algorithms. \emph{Biological Conservation}, \strong{70}, 85--87.
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
#' # verify if guorbi package is installed
#' if (!require(gurobi, quietly = TRUE))
#'  stop("the gurobi R package is not installed.")
#'
#' # find a solution that meets a budget of 300
#' s1 <- ppp_gurobi_solution(sim_project_data, sim_tree, 300,
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
#' }
#' @export
ppp_gurobi_solution <- function(x, tree, budget,
                                project_column_name,
                                cost_column_name,
                                success_column_name,
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
                          assertthat::is.number(gap),
                          is.finite(gap),
                          isTRUE(gap >= 0),
                          assertthat::is.count(time_limit),
                          is.finite(time_limit),
                          assertthat::is.count(threads),
                          assertthat::is.count(number_solutions),
                          is.finite(number_solutions),
                          assertthat::is.count(number_approx_points),
                          assertthat::is.flag(verbose))
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

  # formulate the problem
  f <- rcpp_mip_formulation(spp = spp_probs,
                            budget = budget,
                            branch_matrix = branch_matrix(tree),
                            branch_lengths = tree$edge.length,
                            costs = x[[cost_column_name]],
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
  out <- out[seq_len(nrow(x)), , drop = FALSE]
  out <- t(out) > 0.5
  colnames(out) <- x[[project_column_name]]
  out <- tibble::as_tibble(out)

  ## throw warning if the number of output solutions is not equal to the number
  ## of the requested solution
  if (nrow(out) != number_solutions)
    warning(paste("although", number_solutions, "requested, only", nrow(out),
                  "solutions exist."))

  ## format statistics for output
  out2 <- tibble::tibble(solution = seq_along(s$pool))
  out2$objective <- ppp_objective_value(x, tree, project_column_name,
                                       success_column_name, out)
  out2$budget <- budget
  out2$cost <- rowSums(matrix(x[[cost_column_name]], byrow = TRUE,
                              ncol = ncol(out), nrow = nrow(out)) *
                       as.matrix(out))
  if (s$status == "OPTIMAL") {
    out2$optimal <- (abs(out2$objective[1] - out2$objective) < 1.0e-5)
  } else {
    out2$optimal <- NA
  }
  out2$method <- "gurobi"
  out <- tibble::as_tibble(cbind(out2, out))

  # return result
  out
}
