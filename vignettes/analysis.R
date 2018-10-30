# Initialization
## set parameters
number_species <- 100
budget_increments  <- 500
number_random_solutions_per_budget_increment <- 500

success_min_probability <- 0.7
success_max_probability <- 0.99

not_funded_min_persistence_probability <- 0.01
not_funded_max_persistence_probability <- 0.2

single_spp_cost_mean <- 100
single_spp_cost_sd <- 5
single_spp_funded_min_persistence_probability <- 0.5
single_spp_funded_max_persistence_probability <- 0.9

number_multi_species_projects <- 40
number_species_per_multi_species_project <- 15
multi_spp_funded_min_persistence_probability <- 0.65
multi_spp_funded_max_persistence_probability <- 0.85
multi_spp_cost_mean <- 700
multi_spp_cost_sd <- 5

## load packages
library(magrittr)

# Preliminary processing
## simulate data
### specifically, the projects baseline project and the single species projects
simulated_data <-
  ppp_simulate_data(number_species,
                    single_spp_cost_mean,
                    single_spp_cost_sd,
                    success_min_probability,
                    success_max_probability,
                    single_spp_funded_min_persistence_probability,
                    single_spp_funded_max_persistence_probability,
                    not_funded_min_persistence_probability,
                    not_funded_max_persistence_probability,
                    0, 0)
sim_project_data <- simulated_data$project_data
sim_tree <- simulated_data$tree

### next add the projects that affect multiple species
multi_spp_projects <- matrix(0, ncol = number_species,
                             nrow = number_multi_species_projects)
rows <- rep(seq_len(number_multi_species_projects),
            each = number_species_per_multi_species_project)
cols <- sample.int(number_species, length(rows), replace = TRUE)
values <- runif(length(rows),
                multi_spp_funded_min_persistence_probability,
                multi_spp_funded_max_persistence_probability)
multi_spp_projects[matrix(c(rows, cols), ncol = 2)] <- values
colnames(multi_spp_projects) <-
  names(sim_project_data)[startsWith(names(sim_project_data), "S")]
multi_spp_projects <- tibble::as_tibble(multi_spp_projects)
multi_spp_projects$locked_in <- FALSE
multi_spp_projects$locked_out <- FALSE
multi_spp_projects$name <- paste0("multispecies_",
                                  seq_len(nrow(multi_spp_projects)))
multi_spp_projects$cost <-
  rnorm(nrow(multi_spp_projects),
        multi_spp_cost_mean, multi_spp_cost_sd)
multi_spp_projects$success <- runif(nrow(multi_spp_projects),
                                    success_min_probability,
                                    success_max_probability)

### merge project data togeather to get full project data set
sim_project_data <- rbind(sim_project_data,
                          multi_spp_projects[, names(sim_project_data)])

# Exports
## create budget increments
budgets <- seq(0, plyr::round_any(sum(sim_project_data$cost),
                                  budget_increments, ceiling),
               budget_increments)

# Main processing
## create heuristic prioritizations
### create solutions by extracting all solutions
heuristic_data <- ppp_heuristic_solution(x = sim_project_data, tree = sim_tree,
                                         max(budgets),
                                         project_column_name = "name",
                                         cost_column_name = "cost",
                                         success_column_name = "success",
                                         number_solutions =
                                           nrow(sim_project_data) + 1)

### assign each solution to nearest budget increment
heuristic_data$budget <- vapply(heuristic_data$cost,
                                function(x) min(budgets[budgets > x]),
                                numeric(1))

## create optimal prioritizations
exact_data <- plyr::ldply(budgets, ppp_exact_solution,
                          x = sim_project_data, tree = sim_tree,
                          project_column_name = "name",
                          cost_column_name = "cost",
                          success_column_name = "success", gap = 0.01,
                          verbose = TRUE)

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
   lower = quantile(objective, probs = 0.05, names = FALSE),
   upper = quantile(objective, probs = 0.95, names = FALSE),
   objective = mean(objective)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(method = "random")

## create complete data set
sim_data <- rbind(heuristic_data %>%
                    dplyr::select(budget, method, objective) %>%
                    dplyr::mutate(method = "Heuristic algorithm"),
                  exact_data %>%
                    dplyr::select(budget, method, objective) %>%
                    dplyr::mutate(method = "Exact algorithm"),
                  random_data %>%
                    dplyr::select(budget, method, objective) %>%
                    dplyr::mutate(method = "Random")) %>%
            dplyr::arrange(method, budget, objective)
