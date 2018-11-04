#' @include internal.R
NULL

#' Help document
#'
#' This manual page documents function arguments and outputs.
#'
#' @name help
#'
#' @param x \code{\link[base]{data.frame}} or \code{\link[tibble]{tibble}}
#'   table containing project data. Here, each row should correspond to
#'   a different project and columns should contain data that correspond to
#'   each project. This object should contain data that denote (i)
#'   the name of each project (specified in the argument to
#'   \code{project_column_name}), (ii) the cost of each project
#'   (specified in the argument to \code{cost_column_name}), (iii) the
#'   probability that each project will succeed if it is funded
#'   (specified in the argument to \code{success}),
#'   and (iv) the enhanced probability that each species will persist if it
#'   is funded (specified as the tip labels in the argument to \code{tree}).
#'   To account for the combined benefits of multiple actions (e.g. baiting
#'   and trapping different invasive species in the same area), additional
#'   projects should be created that indicate the combined cost and
#'   corresponding species' persistence probabilities. Furthermore, this object
#'   must have a baseline project, with a zero cost, that represents the
#'   probability that each species will persist if no other conservation
#'   project is funded.
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
#' @param solution \code{\link[tibble]{tibble}} object containing the solution
#'   data. Here, each row corresponds to a different solution. This object
#'   should contain a column for each project---with the column name matching
#'   the project name---and the column should contain \code{logical} values
#'   that indicate if the project is funded (\code{TRUE}) or not (\code{FALSE}).
#'   Although additional columns can also be included, they will be ignored.
#'
#' @keywords internal
#'
#' @return A \code{\link[tibble]{tibble}} object containing the
#'   solution(s) data. Each row corresponds to a different solution, and
#'   each column describes a different property of the solution. The object
#'   contains a column for each project (based on the argument to
#'   \code{project_column_name}) which contains \code{logical} values indicating
#'   if the project was prioritized for funded (\code{TRUE}) or not
#'   (\code{FALSE}) in a given solution. Additionally, the object also contains
#'   the following columns:
#'
#'  \describe{
#'
#'    \item{\code{"objective"}}{\code{numeric} objective value associated with
#'      each of the solution(s). This corresponds to the expected amount of
#'      evolutionary history that is retained by each solution (Faith 2008).}
#'
#'    \item{\code{"budget"}}{\code{numeric} budget used for generating each of
#'      the of the solution(s).}
#'
#'    \item{\code{"cost"}}{\code{numeric} total cost associated with each of
#'      of the solution(s).}
#'
#'    \item{\code{"optimal"}}{\code{logical} indicating if each of the
#'      solution(s) is known to be optimal (\code{TRUE}) or not (\code{FALSE}.
#'      Missing values (\code{NA}) indicate that optimality is unknown
#'      (i.e. because the method used to produce the solution(s) does not
#'      provide any bounds on their quality).}
#'
#'    \item{\code{"method"}}{\code{character} name of method used to produce the
#'      solution(s).)}
#'
#'  }
#'
NULL
