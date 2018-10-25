r_miqp_formulation <- function(project_data, tree, budget) {
  # Initialization
  ## calculate numbers
  n_projects <- nrow(project_data)
  species_names <- tree$tip.label
  n_spp <- length(species_names)

  ## convert tree to branch matrix + vector of lengths
  T_bs <- branch_matrix(tree)
  T_bs <- as(T_bs, "dgTMatrix")
  L_b <- tree$edge.length
  n_branches <- ncol(T_bs)

  ## create variable names
  variable_names <- c(paste0("X_", seq_len(n_projects)),
                      paste0("Y_", outer(seq_len(n_projects), species_names,
                                         paste0)),
                      paste0("E_", species_names),
                      paste0("D_", which(Matrix::colSums(T_bs) > 1)))

  ## calculate number of variables
  n_v <- length(variable_names)

  # Build model
  ## initialize model
  model <- list(modelsense = "min")

  ## linear component of objective function
  ### here, the branches that correspond to a single species are represented in
  ### the linear component of the objective function
  model$obj <- setNames(rep(0, n_v), variable_names)
  for (b in which(Matrix::colSums(T_bs) == 1)) {
    model$obj[match(paste0("E_", species_names[which(T_bs[, b] > 0.5)]),
                    names(model$obj))] <- L_b[b]
  }

  ## quadratic component of objective function
  ### here, the branches that correspond to multiple species are represented in
  ### the quadratic component of the objective function
  model$Q <- Matrix::sparseMatrix(i = 1, j = 1, x = 0,
                                  dims = c(n_v, n_v),
                                  dimnames = list(variable_names,
                                                  variable_names))
  model$Q <- Matrix::drop0(model$Q)
  for (b in which(Matrix::colSums(T_bs) > 1)) {
    curr_spp <- species_names[which(T_bs[, b] > 0.5)]
    m <- matrix(c(rep(paste0("E_", curr_spp[1]), length(curr_spp) - 1),
                  rep(paste0("E_", curr_spp[-1]), length(curr_spp) - 1)),
                ncol = 2)
    m2 <- matrix(0, ncol = 2, nrow = nrow(m))
    m2[, 1] <- match(m[, 1], variable_names)
    m2[, 2] <- match(m[, 2], variable_names)
    model$Q[m2] <- L_b[b] * 0.5
    model$Q[m2[, c(2, 1), drop = FALSE]] <- L_b[b] * 0.5
  }

  # set variable bounds
  model$lb <- replace(rep(0, n_v), grep("D_", variable_names, fixed = TRUE), 1)
  model$ub <- rep(1, n_v)

  # set variable types
  model$vtype <- rep("S", n_v)
  model$vtype[seq_len(n_projects)] <- "B"
  model$vtype[grep("^D\\_.*$", variable_names)] <- "B"

  ## set linear constraints
  ### initialize constraints
  model$A <- Matrix::sparseMatrix(i = 1, j = 1, x = 0,
                                  dims = c(1 + (n_spp * n_projects) + n_spp +
                                           n_spp, n_v),
                                  dimnames = list(NULL, variable_names))
  model$A <- Matrix::drop0(model$A)
  model$rhs <- rep(NA_real_, nrow(model$A))
  model$sense <- rep(NA_character_, nrow(model$A))

  ### budget constraint
  model$A[1, paste0("X_", seq_len(n_projects))] <- project_data$cost
  model$sense[1] <- "L"
  model$rhs[1] <- budget

  ### project allocation constraints
  curr_row <- 1
  for (i in seq_len(n_projects)) {
    for (s in species_names) {
      curr_row <- curr_row + 1
      model$A[curr_row, paste0("X_", i)] <- 1
      model$A[curr_row, paste0("Y_", i, s)] <- -1
      model$sense[curr_row] <- "G"
      model$rhs[curr_row] <- 0
    }
  }

  ### species allocation constraints
  for (s in species_names) {
    curr_row <- curr_row + 1
    model$A[curr_row, paste0("Y_", seq_len(n_projects), s)] <- 1
    model$sense[curr_row] <- "E"
    model$rhs[curr_row] <- 1
  }

  ### species extinction probability constraints
  for (s in species_names) {
    curr_row <- curr_row + 1
    curr_projects <- paste0("Y_", seq_len(n_projects), s)
    curr_projects_nz <- which(project_data[[s]] > 1.0e-10)
    model$A[curr_row, curr_projects] <-
      round(project_data$success * project_data[[s]][curr_projects_nz], 5)
    model$A[curr_row, paste0("E_", s)] <- 1
    model$sense[curr_row] <- "E"
    model$rhs[curr_row] <- 1
  }

  ### convert sparse matrices to vector representation
  model$A <- as(model$A, "dgTMatrix")
  model$Ai <- model$A@i - 1
  model$Aj <- model$A@j - 1
  model$Ax <- model$A@x
  model$Q <- as(model$Q, "dgTMatrix")
  model$Qi <- model$Q@i - 1
  model$Qj <- model$Q@j - 1
  model$Qx <- model$Q@x
  model$n_variables <- n_v

  # Exports
  model
}
