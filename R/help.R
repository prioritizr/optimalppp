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
#'   \code{project_column_name}), (ii) the
#'   probability that each project will succeed if all of its actions are funded
#'   (specified in the argument to \code{success_column_name}), (iii)
#'   the enhanced probability that each species will persist if it
#'   is funded,
#'   and (iv) and which actions are associated with which projects
#'   (specified in the action names in the argument to \code{y}).
#'   To account for the combined benefits of multiple actions (e.g. baiting
#'   and trapping different invasive species in the same area), additional
#'   projects should be created that indicate the combined cost and
#'   corresponding species' persistence probabilities. Furthermore, this object
#'   must have a baseline project, with a zero cost, that represents the
#'   probability that each species will persist if no other conservation
#'   project is funded.
#'
#' @param y \code{\link[base]{data.frame}} or \code{\link[tibble]{tibble}}
#'   table containing the action data. Here, each row should correspond to a
#'   different action and columns should contain data that correspond to
#'   each action. This object should contain data that denote (i)
#'   the name of each action (specified in the argument to
#'   \code{action_column_name}), (ii) the cost of each action (specified in the
#'   argument to \code{cost_column_name}). If certain actions should be locked
#'   in or out of the solution, then this object should also contain data
#'   that denote (iii) which actions should be locked in (specified using
#'   the argument to \code{locked_in_column_name} if relevant) and (iv) which
#'   actions should be locked out (specified using the argument to
#'   \code{locked_out_column_name} if relevant).
#'
#' @param tree \code{\link[ape]{phylo}} phylogenetic tree describing the
#'   evolutionary history of the species affected by the conservation
#'   projects that could potentially be funded. Note that every single
#'   species that is affected by the various conservation projects should
#'   be represented in this tree.
#'
#' @param spp \code{\link[base]{data.frame}} or \code{\link[tibble]{tibble}}
#'   table containing the species data. Here, each row should correspond
#'   to a different species and columns should contain data that correspond
#'   to each species. This object should contain data that denote
#'   (i) the name of each species (specified in the argument to
#'   \code{species_column_name}). It may also contain (ii) the weight for each
#'   species (specified in the argument to \code{weight_column_name} if
#'   relevant).
#'
#' @param species_column_name \code{character} name of the column that contains
#'   the name for each species. This argument corresponds to the argument to
#'   \code{spp}.
#'
#' @param weight_column_name \code{character} name of the column that contains
#'   the weight for each species. This argument corresponds to the argument to
#'   \code{spp}. This argument defaults to \code{NULL}, such that all species
#'   are assigned an equal weighting.
#'
#' @param budget \code{numeric} value that represents the total budget available
#'   for funding conservation actions.
#'
#' @param project_column_name \code{character} name of column that contains
#'   the name for each conservation project. This argument corresponds to the
#'   argument to \code{x}. Note that the project names must not contain any
#'   duplicates or missing values.
#'
#' @param action_column_name \code{character} name of column that contains
#'   the name for each conservation action. This argument corresponds to the
#'   argument to \code{y}. Note that the project names must not contain any
#'   duplicates or missing values.
#'
#' @param cost_column_name \code{character} name of column that
#'   indicates the cost for funding each action. This argument corresponds
#'   to the argument to \code{y}. This column must have \code{numeric} values
#'   which are equal to or greater than zero. No missing values are permitted.
#'
#' @param success_column_name \code{character} name of column that
#'   denotes the probability that each project will succeed. This argument
#'   corresponds to the argument to \code{x}. This column must have
#'   \code{numeric} values which lay between zero and one. No missing values
#'   are permitted.
#'
#' @param locked_in_column_name \code{character} name of column that
#'   indicates which actions should be locked into the funding scheme.
#'   This argument corresponds to the argument to \code{y}. For
#'   example, it may be desirable to mandate that projects for
#'   iconic species are funded in the prioritization. This column should
#'   contain \code{logical} values, and projects associated with \code{TRUE}
#'   values are locked into the solution. No missing values are permitted.
#'   Defaults to \code{NULL} such that no projects are locked into the
#'   solution.
#'
#' @param locked_out_column_name \code{character} name of column that
#'   indicates which actions should be locked out of the funding scheme.
#'   This argument corresponds to the argument to \code{y}. For
#'   example, it may be desirable to lock out projects for certain species
#'   that are expected to have little support from the public. This column
#'   should contain \code{logical} values, and projects associated with
#'   \code{TRUE} values are locked out of the solution. No missing values are
#'   permitted. Defaults to \code{NULL} such that no projects are locked out of
#'   the solution.
#'
#' @param gap \code{numeric} optimality gap. This gap should be expressed as
#'   a proportion. For example, to find a solution that is within 10 \% of
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
#'   should contain a column for each action---with the column names matching
#'   the project names---and the column should contain \code{logical} values
#'   that indicate if the action is funded (\code{TRUE}) or not (\code{FALSE}).
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
#'    \item{\code{"solution"}}{\code{integer} solution identifier.}
#'
#'    \item{\code{"method"}}{\code{character} name of method used to produce the
#'      solution(s).)}
#'
#'    \item{\code{"budget"}}{\code{numeric} budget used for generating each of
#'      the of the solution(s).}
#'
#'    \item{\code{"obj"}}{\code{numeric} objective value. If phylogenetic data
#'      were input, then this column contains the expected phylogenetic
#'      diversity (Faith 2008) associated with each of the solutions.
#'      Otherwise, this column contains the expected weighted species richness
#'      (i.e. the sum of the product between the species' persistence
#'      probabilities and their weights.}
#'
#'    \item{\code{"cost"}}{\code{numeric} total cost associated with each of
#'      of the solution(s).}
#'
#'    \item{\code{"optimal"}}{\code{logical} indicating if each of the
#'      solution(s) is known to be optimal (\code{TRUE}) or not (\code{FALSE}).
#'      Missing values (\code{NA}) indicate that optimality is unknown
#'      (i.e. because the method used to produce the solution(s) does not
#'      provide any bounds on their quality).}
#'
#'  }
#'
NULL
