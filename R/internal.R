#' 'Project Prioritization Protocol' objective value
#'
#' Calculate the objective value for a set of solutions to a Project
#' Prioritization Protocol problem.
#'
#' @inheritParams ppp_gurobi_solution
#'
#' @param solution \code{\link[tibble]{tibble}} indicating if each project
#'   is funded or not. Here, each column corresponds to a different project and
#'   each row corresponds to a different solution.
#'
#' @return \code{numeric} vector with an objective value for each solution
#'  in the argument to \code{solutions}.
#'
#' @keywords internal
ppp_objective_value <- function(x, tree, project_column_name,
                                success_column_name, solution) {
  # assert that parameters are valid
  assertthat::assert_that(inherits(x, c("data.frame", "tbl_df")),
                          ncol(x) > 0, nrow(x) > 0,
                          inherits(tree, "phylo"),
                          assertthat::is.string(project_column_name),
                          assertthat::has_name(x, project_column_name),
                          assertthat::is.string(success_column_name),
                          assertthat::has_name(x, success_column_name),
                          is.numeric(x[[success_column_name]]),
                          assertthat::noNA(x[[success_column_name]]),
                          inherits(solution, c("data.frame", "tbl_df")),
                          all(as.character(x[[project_column_name]]) %in%
                              names(solution)),
                          all(vapply(solution[, x[[project_column_name]],
                                              drop = FALSE],
                                     class, character(1)) ==
                                     "logical"))
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
  } else {
    assertthat::assert_that(nrow(tree$edge) == length(tree$edge.length))
  }

  ## pre-compute conditional probabilities of species persistence
  ## and project success
  spp_probs <- as.matrix(x[, tree$tip.label, drop = FALSE])
  spp_probs <- spp_probs * matrix(x[[success_column_name]],
                                  ncol = ncol(spp_probs),
                                  nrow = nrow(spp_probs))
  spp_probs <- Matrix::drop0(as(round(spp_probs, 5), "dgCMatrix"))

  # Exports
  rcpp_ppp_objective(spp_probs, branch_matrix(tree), tree$edge.length,
                     as(as.matrix(solution[, x$name]), "dgCMatrix"))
}
