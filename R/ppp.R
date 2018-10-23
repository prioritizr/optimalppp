#' @include internal.R
NULL

#' Project prioritization protocol
#'
#' Prioritize funding for conservation projects. To identify optimal funding
#' schemes, the IBM ILOG CPLEX optimization software suite and the
#' \pkg{cplexAPI} package need to be installed.
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
#'   values are permitted. Defaults to \code{"cost"}.
#'
#' @param success_column_name \code{character} name of column that
#'   denotes the probability that each project will succeed if it is funded.
#'   This column must have \code{numeric} values which lay between zero and one.
#'   No missing values are permitted. Defaults to \code{"success"}.
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
#' @param time_limit \code{numeric} maximum number of seconds that should be
#'   spent searching for a solution after formatting the data. Defaults
#'   to \code{NULL} such that no time limit is imposed.
#'
#' @details TODO
#'
#' @return A \code{\link[tibble]{tibble}} object that contains the data in
#'   the argument to \code{x}, with an additional column named
#'   \code{"solution"} that contains \code{logical} (i.e.
#'   \code{TRUE}/\code{FALSE} values) which indicate if each project is
#'   selected for funding in the solution. This object also has the
#'   following attributes: \code{"objective"} containing the solution's
#'   objective, \code{"runtime"} denoting the number of seconds that elapsed
#'   while solving the problem, and \code{"status"} describing the status
#'   of the solver (e.g. \code{"TODO"} indicates that the
#'   optimal solution was found, and \code{"TODO"} indicates that the solution
#'   meets the optimality gap.
ppp <- function(x, tree, budget,
                cost_column_name = "cost",
                success_column_name = "success",
                locked_in_column_name = NULL,
                locked_out_column_name = NULL,
                gap = 0.000001, time_limit = NULL, threads = 1L,
                verbose = TRUE) {
  # assertions
  ## assert that cplexAPI is installed
  assertthat::assert_that(requireNamespace("cplexAPI", quietly = TRUE),
                          msg = "cplexAPI package not installed.")
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
                          assertthat::is.count(threads),
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
    stop("some projects are specified as locked in and locked out.")
  }
  if (!is.null(time_limit))
    assertthat::assert_that(assertthat::is.count(time_limit))
  asserthat::assert_that(
    all(tree$tip.label %in% names(x)),
    msg = paste("argument to tree contains species that do not appear as",
                "column names in the argument to x:",
                paste(paste0("'", setdiff(tree$tip.label, names(x)), "'"),
                      collapse = ", ")))

  # prepare data for cplex
  ## prepare problem formulation
  ## apply locked constraints
  locked_in <- integer(0)
  locked_out <- integer(0)
  if (!is.null(locked_in_column_name))
    locked_in <- which(x[[locked_in_column_name]])
  if (!is.null(locked_out_column_name))
    locked_out <- which(x[[locked_in_column_name]])
  f <- rcpp_miqp_formulation(spp = round(as.matrix(x[, tree$tip.label,
                                                     drop = FALSE]), 5),
                             budget = budget,
                             branch_matrix = branch_matrix(tree),
                             branch_lengths = tree$edge.lengths,
                             costs = x[[cost_column_name]],
                             success_probabilities = x[[success_column_name]],
                             locked_in = locked_in,
                             locked_out = locked_out)
  ## convert linear constraint matrix to array representation for cplexAPI
  f$A <- Matrix::sparseMatrix(i = f$Ai, j = f$Aj, x = f$Ax,
                              dims = c(max(f$Aj), f$n_variables),
                              index1 = FALSE)
  f$A <- as(f$A, "dgCMatrix")
  f$Amatbeg <- f$A@p
  f$Amatcnt <- diff(c(f$A@p, length(f$A@x)))
  f$Amatind <- f$A@i
  f$Amatval <- f$A@x
  ## convert quadratic objective matrix to array representation for cplexAPI
  f$Q <- Matrix::sparseMatrix(i = f$Qi, j = f$Qj, x = f$Qx,
                              dims = rep(f$n_variables, 2),
                              index1 = FALSE)
  f$Q <- as(f$Q, "dgCMatrix")
  f$Qmatbeg <- f$Q@p
  f$Qmatcnt <- diff(c(f$Q@p, length(f$Q@x)))
  f$Qmatind <- f$Q@i
  f$Qmatval <- f$Q@x

  # create cplex model
  ## initialize cplex environment
  env <- cplexAPI::openEnvCPLEX()
  ## set parameters for solving problem
  cplexAPI::setIntParmCPLEX(env, cplexAPI::CPX_PARAM_SCRIND,
                            as.integer(verbose))
  cplexAPI::setIntParmCPLEX(env, cplexAPI::CPXPARAM_SolutionTarget, 3L)
  cplexAPI::setIntParmCPLEX(env, cplexAPI::CPX_PARAM_THREADS,
                            as.integer(threads))
  cplexAPI::setDblParmCPLEX(env, cplexAPI::CPXPARAM_MIP_Tolerances_MIPGap, gap)
  if (!is.null(time_limit))
    cplexAPI::setIntParmCPLEX(env, cplexAPI::CPXPARAM_TimeLimit,
                              as.intger(time_limit))
  ## construct model
  p <- cplexAPI::initProbCPLEX(env, "ppp")
  cplexAPI::copyLpwNamesCPLEX(env, p, ncol(f$A), nrow(f$A),
                    cplexAPI::CPX_MIN, f$obj, f$rhs, f$sense, f$Amatbeg,
                    f$Amatcnt, f$Amatind, f$Amatval, f$lb, f$ub)
  cplexAPI::copyQuadCPLEX(env, p, f$Qmatbeg, f$Qmatcnt,
                          f$Qmatind, f$Qmatval)
  cplexAPI::copyColTypeCPLEX(env, p, f$vtype)
  ## solve model
  cplexAPI::mipoptCPLEX(f, p)
  ## extract solution
  sol_path <- tempfile(fileext = ".sol")
  cplexAPI::solWriteCPLEX(env, p, sol_path)
  l <- xml2::as_list(xml2::read_xml(sol_path))

  # return output
  x$solution <- as.logical(as.numeric(vapply(
    l$CPLEXSolution$variables[seq_len(nrow(x))], attr, character(1), "value")))
  attr(x, "objval") <- as.numeric(attr(l$CPLEXSolution$header,
                                       "objectiveValue"))
  attr(x, "status") <- attr(l$CPLEXSolution$header, "solutionStatusString")
  x
}
