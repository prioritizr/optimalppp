#' @include internal.R
NULL

#' Random solutions for the 'Project Prioritization Protocol' problem
#'
#' Generate random solutions for the 'Project Prioritization Protocol' problem.
#' Although conservation projects should, ideally, not be funded based on random
#' allocations, it can be useful to compare the effectiveness of solutions to
#' random decision in order to evaluate their effectiveness.
#' \strong{When informing conservation actions, it is strongly recommended to
#'  use the \code{\link{ppp_gurobi_solutions}} method because it can identify
#'  optimal funding schemes with a guarantee.}
#'
#' @inheritParams ppp_gurobi_solution
#'
#' @details The random solutions are generated using the following algorithm.
#'  All projects are initially selected for funding. Next,
#'  a project is randomly selected to be defunded (removed), and this step
#'  is repeated until the cost of the remaining projects is within the
#'  budget.
#'
#' @seealso For other methods for generating solutions for the 'Project
#'   Prioritization Protocol' problem, see \code{\link{ppp_heuristic_solution}}
#' \code{\link{ppp_gurobi_solution}}, and \code{\link{ppp_manual_solution}}. To
#' visualize the effectiveness of a particular solution, see
#' \code{\link{ppp_plot}}.
#'
#' @export
ppp_random_solution <- function(x, tree, budget,
                                project_column_name,
                                cost_column_name,
                                success_column_name,
                                locked_in_column_name = NULL,
                                locked_out_column_name = NULL) {
  stop("TODO")
}
