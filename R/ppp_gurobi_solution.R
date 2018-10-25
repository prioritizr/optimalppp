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
#' @param gap \code{numeric} optimality gap. Defaults to 0.000001, so that
#'   solutions will be within 0.0001 % of optimality. No missing values are
#'   permitted.
#'
#' @param threads \code{numeric} number of threads for computational processing.
#'   Defaults to \code{1}.
#'
#' @param time_limit \code{numeric} maximum number of seconds that should be
#'   spent searching for a solution after formatting the data. Effectively,
#'   defaults to no time limit (but specifically is
#'   \code{.Machine$integer.max}).
#'
#' @param number_approx_points \code{numeric} number of points to use for
#'   approximating the probability that branches will go extent. Larger
#'   values increase the precision of these calculations. Defaults to
#'   \code{300}.
#'
#' @param verbose \code{logical} should information be printed while solving
#'   the problem? Defaults to \code{FALSE}.
#'
#' @details TODO
#'
#' @return A \code{\link[tibble]{tibble}} object that contains the data in
#'   the argument to \code{x}, with an additional column named
#'   \code{"solution"} that contains \code{logical} (i.e.
#'   \code{TRUE}/\code{FALSE} values) which indicate if each project is
#'   selected for funding in the solution. This object also has the
#'   following attributes:
#'
#'   \describe{
#'
#'     \item{\code{objective}}{\code{numeric} objective value associated with
#'       the solution.}
#'
#'     \item{\code{runtime}}{\code{numeric} number of seconds that elapsed
#'       while solving the problem.}
#'
#'     \item{\code{status}}{\code{character} description of the state of the
#'       solver (e.g. \code{"OPTIMAL"} indicates that the
#'       optimal solution was found.}
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
                          assertthat::is.count(number_approx_points),
                          assertthat::is.flag(verbose))
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
    assertthat::see_if(max(x[[locked_out_column_name]] +
                           x[[locked_in_column_name]]) == 1)
    stop("some projects locked in and locked out.")
  }
  if (!is.null(time_limit))
    assertthat::assert_that(assertthat::is.count(time_limit))
  asserthat::assert_that(
    all(tree$tip.label %in% names(x)),
    msg = paste("argument to tree contains species that do not appear as",
                "column names in the argument to x:",
                paste(paste0("'", setdiff(tree$tip.label, names(x)), "'"),
                      collapse = ", ")))

  # preliminary data formatting
  ## determine which projects need to be locked
  locked_in <- integer(0)
  locked_out <- integer(0)
  if (!is.null(locked_in_column_name))
    locked_in <- which(x[[locked_in_column_name]])
  if (!is.null(locked_out_column_name))
    locked_out <- which(x[[locked_in_column_name]])

  ## pre-compute conditional probabilities of species persistance
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
                            branch_lengths = tree$edge.lengths,
                            costs = x[[cost_column_name]],
                            locked_in = locked_in,
                            locked_out = locked_out,
                            n_approx_points = number_approx_points)

  # solve the problem
  s <- gurobu::gurobi(f, params = list(Presolve = 2,
                                       LogToConsole = as.integer(verbose),
                                       MIPGap = gap,
                                       Threads = threads,
                                       TimeLimit = time_limit,
                                       LogFile = ""))

  # format result for output
  x$solution <- as.logical(s$x[seq_len(nrow(x))])
  attr(x, "objective") <- s$objval
  attr(x, "status") <- s$status
  attr(x, "runtime") <- s$runtime

  # return result
  x
}
