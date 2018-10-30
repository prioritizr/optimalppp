# Initialization
## set parameters
number_species <- 100
cost_mean <- 100
cost_sd <- 5
success_min_probability <- 0.7
success_max_probability <- 0.99
funded_min_persistence_probability <- 0.5
funded_max_persistence_probability <- 0.9
not_funded_min_persistence_probability <- 0.01
not_funded_max_persistence_probability <- 0.4
budget_increments  <- 500
number_random_solutions_per_budget_increment <- 100

## load packages
library(optimalppp)

# Preliminary processing
## simulate data
sim_data <- ppp_simulate_data(number_species,
                              cost_mean,
                              cost_sd,
                              success_min_probability,
                              success_max_probability,
                              funded_min_persistence_probability,
                              funded_max_persistence_probability,
                              not_funded_min_persistence_probability,
                              not_funded_max_persistence_probability,
                              0, 0)
sim_project_data <- sim_data$project_data
sim_tree <- sim_data$tree

# Exports
## create budget increments
budgets <- seq(0, plyr::round_any(sum(sim_project_data$cost),
                                  budget_increments, ceiling),
               budget_increments)

# Main processing
## create heuristic prioritizations
heuristic_data <- plyr::ldply(budgets, ppp_heuristic_solution,
                              x = sim_project_data, tree = sim_tree,
                              project_column_name = "name",
                              cost_column_name = "cost",
                              success_column_name = "success")

## create optimal prioritizations
exact_data <- plyr::ldply(budgets, ppp_exact_solution,
                          x = sim_project_data, tree = sim_tree,
                          project_column_name = "name",
                          cost_column_name = "cost",
                          success_column_name = "success")

## create random prioritizations
random_raw_data <- plyr::ldply(budgets, ppp_random_solution,
                               x = sim_project_data, tree = sim_tree,
                               project_column_name = "name",
                               cost_column_name = "cost",
                               success_column_name = "success",
                               number_solutions =
                                 number_random_solutions_per_budget_increment)

# Exports
## calculate a summary for each budget increment
random_data <-
  random_raw_data %>%
  dplyr::group_by(budget) %>%
  dplyr::summarize(
   lower = suppressWarnings(gmodels::ci(objective)[[2]]),
   upper = suppressWarnings(gmodels::ci(objective)[[3]]),
   objective = median(objective)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(method = "random")

## create complete data set
sim_data <- rbind(dplyr::select(heuristic_data, budget, method,
                                objective),
                  dplyr::select(exact_data, budget, method, objective),
                  dplyr::select(random_data, budget, method, objective))
