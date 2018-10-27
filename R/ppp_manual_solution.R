#' @include internal.R
NULL

#' Solve the 'Project Prioritization Protocol' problem by manually specifying
#' a solution
#'
#' Manually specify funding schemes for conservation projects and examine
#' their effectiveness.
#'
#' @inheritParams ppp_gurobi_solution
#'
#' @details TODO
#'
#' @inherit ppp_results_class return seealso
#'
#' @export
ppp_manual_solution <- function(x, tree, budget, solution,
                                project_column_name,
                                cost_column_name,
                                success_column_name,
                                verbose = FALSE) {
  # assertions
  ## coerce x to tibble if just a regular data.frame
  if (inherits(x, "data.frame") && !inherits(x, "tbl_df"))
    x <- tibble::as_tibble(x)
  ## coerce solution to tibble if just a regular data.frame
  if (inherits(solution, "data.frame") && !inherits(solution, "tbl_df"))
    solution <- tibble::as_tibble(solution)
  ## assert that parameters are valid
  assertthat::assert_that(inherits(x, "tbl_df"),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(solution, "tbl_df"),
                          ncol(solution) > 0, nrow(solution) > 0,
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
                          assertthat::noNA(x[[success_column_name]]))
  ## coerce factor species names to character
  if (is.factor(x[[project_column_name]]))
    x[[project_column_name]] <- as.character(x[[project_column_name]])
  ## additional checks
  assertthat::assert_that(all(assertthat::has_name(solution,
                                                   x[[project_column_name]])))
  assertthat::assert_that(all(vapply(solution[, x[[project_column_name]],
                                              drop = FALSE],
                                     class, character(1)) == "logical"))

  # preliminary data processing
  ## check that branches have lengths
  if (is.null(tree$edge.length)) {
    tree$edge.length <- rep(1, nrow(tree$edge))
    warning(paste("tree does not have branch length data,",
                  "all branches are assumed to have equal lengths"))
  }  else {
    assertthat::assert_that(nrow(tree$edge) == length(tree$edge.length))
  }

  # main processing
  ## format statistics for output
  s <- solution
  out <- tibble::as_tibble(cbind(
    tibble::tibble(
      solution = seq_len(nrow(solution)),
      objective = ppp_objective_value(x, tree, project_column_name,
                                      success_column_name, s),
      cost = rowSums(matrix(x[[cost_column_name]], byrow = TRUE,
                            ncol = ncol(s), nrow = nrow(s)) *
                     as.matrix(s[, x[[project_column_name]], drop = FALSE])),
      optimal = NA,
      method = "manual"),
    s[, x[[project_column_name]], drop = FALSE]))

  # return result
  out
}
