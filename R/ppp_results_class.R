#' @include internal.R
NULL

#' 'Project Prioritization Protocol' solution class
#'
#' This manual page documents information about the
#' \code{\link[tibble]{tbl_df}} object output from the solver functions.
#'
#' @name ppp_results_class
#'
#' @keywords internal
#'
#' @seealso The following methods can be used to extract, inspect, and
#'   visualize the solution(s): TODO
#'
#' @return A \code{\code{link[tibble]{tbl_df}} object containing the
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
#'    \item{\code{objective}}{\code{numeric} objective value associated with
#'      each of the solution(s). This corresponds to the expected amount of
#'      evolutionary history that is retained by each solution.}
#'
#'    \item{\code{cost}}{\code{numeric} total cost associated with each of
#'      of the solution(s).}
#'
#'    \item{\code{optimal}}{\code{logical} indicating if each of the
#'      solution(s) is known to be optimal or not.)}
#'
#'  }}
#'
#'  Finally, the object also contains following attributes
#'  (which can be accessed, for example, using \code{attr(output, "status")}):
#'
#'  \describe{
#'
#'    \item{\code{runtime}}{\code{numeric} number of seconds that elapsed
#'      while solving the problem.}
#'
#'    \item{\code{status}}{\code{character} description of the
#'      solver status (e.g. \code{"OPTIMAL"} indicates that optimal solution(s)
#'      were found.)}
#'
#'  }
NULL
