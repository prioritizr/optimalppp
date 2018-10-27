#' @include internal.R
NULL

#' Solve the 'Project Prioritization Protocol' problem using Gurobi
#'
#' Prioritize funding for conservation projects using the Gurobi optimization
#' software suite. Unlike other methods for generating prioritizations,
#' this method can identify optimal solutions (or solutions within a
#' pre-specified optimality gap). \strong{As a consequence, it is strongly
#' recommended to use this method for developing project prioritizations.}
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
#' @param project_column_name \code{character} name of column that contains
#'   the name for each conservation project. Note that the project names
#'   must not contain any duplicates or missing values.
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
#' @inherit ppp_results_class return seealso
#'
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
                          assertthat::is.scalar(budget),
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

  ## create branch matrix
  bm <- branch_matrix(tree)

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
                            branch_matrix = bm,
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
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      solution = seq_along(s$pool),
      objective = ppp_objective_value(x, tree, project_column_name,
                                      success_column_name, out),
      cost = rowSums(matrix(x[[cost_column_name]], byrow = TRUE,
                            ncol = ncol(out), nrow = nrow(out)) *
                     as.matrix(out)),
      optimal = if (s$status == "OPTIMAL") {
                  (abs(objective[1] - objective) < 1.0e-5)
                } else {
                  NA_logical_
                },
      method = "gurobi"), out))

  # return result
  out
}
