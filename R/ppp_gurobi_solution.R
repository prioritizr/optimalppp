#' @include internal.R
NULL

#' Project prioritization protocol
#'
#' Prioritize funding for conservation projects. To identify optimal funding
#' schemes, the Gurobi optimization software suite and the
#' \pkg{gurobi} package need to be installed.
#'
#' @param x \code{\link[base]{data.frame}} or \code{\link[tibble]{tbl_df}}
#'   table containing project data. Here, each row should correspond to
#'   a different project and columns should contain that correspond to
#'   each project. See the Details section below for more information.
#'
#' @param tree \code{\link[ape]{phylo}} phylogenetic tree describing the
#'   evolutionary history of the species affected by the conservation
#'   projects that could potentially be funded. Note that every single
#'   species that is affected by the various conservation projects should
#'   be represented in this tree.
#'
#' @param budget \code{numeric} value that represents the total budget available
#'   for funding conservation projects.
#'
#' @param cost_column_name \code{character} name of column that
#'   indicates the cost for funding each project. This column must have
#'   \code{numeric} values which are equal to or greater than zero. No missing
#'   values are permitted.
#'
#' @param success_column_name \code{character} name of column that
#'   denotes the probability that each project will succeed if it is funded.
#'   This column must have \code{numeric} values which lay between zero and one.
#'   No missing values are permitted.
#'
#' @param locked_in_column_name \code{character} name of column that
#'   indicates which projects should be locked into the funding scheme. For
#'   example, it may be desirable to mandate that projects for
#'   iconic species are funded in the prioritization. This column should
#'   contain \code{logical} values, and projects associated with \code{TRUE}
#'   values are locked into the solution. No missing values are permitted.
#'   Defaults to \code{NULL} such that no projects are locked into the
#'   solution.
#'
#' @param locked_out_column_name \code{character} name of column that
#'   indicates which projects should be locked out of the funding scheme. For
#'   example, it may be desirable to lock out projects for certain species
#'   that are expected to have little support from the public. This column
#'   should contain \code{logical} values, and projects associated with
#'   \code{TRUE} values are locked out of the solution. No missing values are
#'   permitted. Defaults to \code{NULL} such that no projects are locked out of
#'   the solution.
#'
#' @param gap \code{numeric} optimality gap. This gap should be expressed as
#'   a proportion. For example, to find a solution that is within 10 % of
#'   optimality, then \code{0.1} should be supplied. No missing values are
#'   permitted. Defaults to \code{0}, so that the optimal solution will be
#'   returned.
#'
#' @param threads \code{numeric} number of threads for computational processing.
#'   No missing values are permitted. Defaults to \code{1}.
#'
#' @param number_solutions \code{numeric} number of solutions to return. If
#'   the argument is greater than \code{1}, then the output will contain the
#'   set number of solutions that are closest to optimality. No missing values
#'   are permitted. Defaults to \code{1}.
#'
#' @param time_limit \code{numeric} maximum number of seconds that should be
#'   spent searching for a solution after formatting the data. Effectively,
#'   defaults to no time limit (but specifically is
#'   \code{.Machine$integer.max}). No missing values are permitted.
#'
#' @param number_approx_points \code{numeric} number of points to use for
#'   approximating the probability that branches will go extent. Larger
#'   values increase the precision of these calculations.
#'   No missing values are permitted. Defaults to \code{300}.
#'
#' @param verbose \code{logical} should information be printed while solving
#'   the problem? No missing values are permitted. Defaults to \code{FALSE}.
#'
#' @details TODO
#'
#' @return A \code{\link[tibble]{tbl_df}} object that contains a column
#'   for each solution. Each row corresponds to a different project,
#'   (following the same order as the data in the argument to \code{x}), and
#'   each column corresponds to a different solution. Cell values are
#'   \code{logical} (i.e. \code{TRUE}/\code{FALSE} values) indicating if each
#'   project is selected for funding in a given solution. The column names
#'   are formatted where the name \code{"solution_N"} contains the
#'   the n'th solution. For example, if only one solution is returned
#'   (as specified in the \code{number_solutions} argument) then the
#'   output object will only contain a single column called
#'   \code{"solution_1"}. The output object is also associated with the
#'   following attributes that describe the solution(s).
#'
#'   \describe{
#'
#'     \item{\code{status}}{\code{character} description of the
#'       solver (e.g. \code{"OPTIMAL"} indicates that optimal solution(s)
#'       were found.)}
#'
#'     \item{\code{runtime}}{\code{numeric} number of seconds that elapsed
#'       while solving the problem.}
#'
#'     \item{\code{objective}}{\code{numeric} objective value associated with
#'       each of the solution(s).}
#'
#'     \item{\code{optimality}}{\code{logical} indicating if each solution
#'       is known to be optimal or not.)}
#'
#'  }
#'
#' @export
ppp_gurobi_solution <- function(x, tree, budget,
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
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, c("data.frame", "tbl_df")),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.scalar(budget),
                          is.finite(budget),
                          isTRUE(budget >= 0),
                          assertthat::is.string(cost_column_name),
                          assertthat::has_name(x, cost_column_name),
                          is.numeric(x[[cost_column_name]]),
                          assertthat::noNA(x[[cost_column_name]]),
                          assertthat::is.string(success_column_name),
                          assertthat::has_name(x, success_column_name),
                          is.numeric(x[[success_column_name]]),
                          assertthat::noNA(x[[success_column_name]]),
                          assertthat::is.scalar(gap),
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
  if (!is.null(time_limit))
    assertthat::assert_that(assertthat::is.count(time_limit))
  assertthat::assert_that(
    all(tree$tip.label %in% names(x)),
    msg = paste("argument to tree contains species that do not appear as",
                "column names in the argument to x:",
                paste(paste0("'", setdiff(tree$tip.label, names(x)), "'"),
                      collapse = ", ")))

  # preliminary data formatting
  ## check that branches have lengths
  if (is.null(tree$edge.length)) {
    tree$edge.length <- rep(1, nrow(tree$edge))
    warning(paste("tree does not have branch length data,",
                  "all branches are assumed to have equal lengths"))
  }

  ## determine which projects need to be locked
  locked_in <- integer(0)
  locked_out <- integer(0)
  if (!is.null(locked_in_column_name))
    locked_in <- which(x[[locked_in_column_name]])
  if (!is.null(locked_out_column_name))
    locked_out <- which(x[[locked_out_column_name]])

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(as(round(spp_probs, 5), "dgCMatrix"))

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
  # format result for output
  obj <- sapply(s$pool, `[[`, "objval", simplify = TRUE)
  out <- sapply(s$pool, `[[`, "xn", simplify = TRUE)
  if (!is.matrix(out))
    out <- matrix(out, ncol = 1)
  out <- out > 0.5
  colnames(out) <- paste0("solution_", seq_len(ncol(out)))
  out <- tibble::as_tibble(out[seq_len(nrow(x)), , drop = FALSE])
  attr(out, "status") <- s$status
  attr(out, "runtime") <- s$runtime
  attr(out, "objective") <- obj
  attr(out, "optimal") <- (s$status == "OPTIMAL") & (abs(obj[1] - obj) < 1.0e-5)

  # throw warning if less solutions returned then requested
  if (ncol(out) < number_solutions)
    warning(paste0("although ", number_solutions, "requested, only ",
                   ncol(number_solutions), "solutions exist."))

  # return result
  out
}
