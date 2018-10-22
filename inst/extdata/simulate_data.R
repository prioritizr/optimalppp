# Initialization
## set seed for reproducibility
set.seed(500)

## set simulation parameters
n_spp <- 5 # number of species for simulation
budget_fraction <- 0.25 # fraction of total project cost available for funding
project_cost_distribution_parameters <- c(100, 5) # mean/sd for project cost
project_success_distribution_parameters <- c(-0.3, 1) # mean/sd for project
                                                      # success
spp_persistence_parameters <- c(0.5, 1) # mean/sd for persistence
spp_do_nothing_persistence_scaling_parameter <- 0.1 # spp persistence for do
                                                    # nothing project expressed
                                                    # as proportion of project
                                                    # persistence

# Main processing
## create project data
sim_project_data <- data.frame(
  name = c(paste0("project_spp_", seq_len(n_spp)), "project_do_nothing"),
  cost = c(rnorm(n_spp, project_cost_distribution_parameters[1],
              project_cost_distribution_parameters[2]), 0),
  project_success = c(plogis(rnorm(n_spp,
                      project_success_distribution_parameters[1],
                      project_success_distribution_parameters[2])), 1))

## phylogenetic tree
sim_phylogeny <- ape::rtree(n = n_spp, tip.label = paste0("S", seq_len(n_spp)))

## species persistence probabilities
species_names <- sim_phylogeny$tip.label
for (i in seq_along(species_names))
  sim_project_data[[species_names[i]]] <- replace(
    rep(0, nrow(sim_project_data)), i,
    plogis(rnorm(1, spp_persistence_parameters[1],
                 spp_persistence_parameters[2])))

## set persistence probabilities for species when the "do nothing" project is
## their only project
sim_project_data[nrow(sim_project_data), species_names] <-
  colSums(sim_project_data[, species_names]) *
  spp_do_nothing_persistence_scaling_parameter

# Exports
save(sim_project_data, file = "data/sim_sim_project_data.rda", compress = "xz")
save(sim_phylogeny, file = "data/sim_phylogeny.rda", compress = "xz")
