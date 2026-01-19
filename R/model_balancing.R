# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Balance predictions ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

balance_predictions <- function(data,
                                nodes,
                                balancing_grouping_variable,
                                nodes_to_balance = "complete_nodes",
                                heavy_vehicle = FALSE,
                                year){

  aadt_colname <- if(heavy_vehicle) "heavyAadt" else "aadt"
  suffix <- if(heavy_vehicle) "_heavy" else ""
  pred_colname <- paste0("inla_pred", suffix)
  sd_colname <- paste0("inla_sd", suffix)

  # Assign measurement error to observed values
  sigma_error <- calculate_measurement_error(
    data = data,
    colname_aadt = aadt_colname,
    source_col = "traffic_volume_source",
    year_col = "traffic_volume_year",
    coverage_col = "coverage",
    current_year = year
  )

  data$sigma_error <- sigma_error

  # Here we need to assign the balancing groups.
  # When joining the balancing groups to the data, this will create duplicates.

  if(is.character(balancing_grouping_variable) &&
     length(balancing_grouping_variable) == 1){
    if(balancing_grouping_variable == "run_clustering"){
      balancing_grouping_variable <- strategic_network_clustering(data)
    }else if(balancing_grouping_variable == "no_clustering"){
      data$cluster_id <- 1
      balancing_grouping_variable <- "cluster_id"}
  }
  # "balancing_grouping_variable" can be either a column name (then no duplicates)
  # or a data frame with the group mapping. In the latter case, the cluster id column will be "cluster_id".
  if(is.data.frame(balancing_grouping_variable)){
    data <- dplyr::full_join(data, balancing_grouping_variable,
                             by = dplyr::join_by(parentTrafficLinkId == id),
                             relationship = "many-to-many")
    balancing_grouping_variable <- "cluster_id"
  }


  groups <- sort(unique(data[[balancing_grouping_variable]]))


  # Initialize result list
  group_data_list <- list()
  group_diagnostics <- list()

  cat("Balancing predictions for all groups... --------------\n")
  # Loop through groups calling fit_and_balance_for_group()
  for(group in groups){
    cat("  Balancing predictions for group: ", group, "\n")
    group_data <- dplyr::filter(data, .data[[balancing_grouping_variable]] == group)

    balanced_group_results <- balance_group_predictions(
      data = group_data,
      nodes = nodes,
      colname_aadt = aadt_colname,
      colname_measurement_sd = "sigma_error",
      pred = group_data[[pred_colname]],
      sd = group_data[[sd_colname]],
      nodes_to_balance = nodes_to_balance)

    group_data$balanced_pred <- balanced_group_results$results$balanced_pred
    group_data$balanced_sd <- balanced_group_results$results$balanced_sd
    group_data_list[[group]] <- dplyr::select(group_data, id,
                                              balanced_pred, balanced_sd)

    group_diagnostics[[group]] <- list(
      diagnostics = balanced_group_results$diagnostics,
      incidence_matrix = balanced_group_results$matrices$A1)
  }


  # Determine suffix based on response variable
  suffix <- if(heavy_vehicle) "_heavy" else ""

  # Combine results across all groups
  predictions <- dplyr::bind_rows(group_data_list, .id = "grouping")
  predictions_by_link <- predictions |>
    dplyr::group_by(id) |> # Here we need to handle the possible duplicates.
    dplyr::summarise(n_duplicates = dplyr::n(), # Number of duplicates
              balanced_pred = mean(balanced_pred), # prediction is average of predictions
              balanced_sd = 1/n_duplicates*(sqrt(sum(balanced_sd^2))) # sd is 1/n_dup*sqrt(sum(sd^2))
    ) |>
    setNames(c("id", "nduplicates", paste0("balanced_pred", suffix), paste0("balanced_sd", suffix)))

  # Add the measurement error
  group_diagnostics$measurement_sd <- dplyr::select(data, id, sigma_error)

  group_diagnostics$predictions_by_group <- predictions


  return(list(balanced_res = predictions_by_link,
              diagnostics = group_diagnostics))
}

balance_group_predictions <- function(data, nodes, model = NULL, pred = NULL, sd = NULL,
                                      constraint_matrix = NULL,
                                      colname_aadt = "heavyAadt", colname_measurement_sd = "sigma_error",
                                      lambda = 1e-10, nodes_to_balance){
  start_time <- Sys.time()
  one_node_flag <- FALSE   # Flagging if a group has only one node

  if(!is.null(model)){
    mu_v <- round(model$summary.fitted.values[, "0.5quant"])
    marginal_sds <- model$summary.fitted.values[, "sd"]
  } else {
    mu_v <- pred
    marginal_sds <- sd
  }


  # Step 0: Set up data and constraint matrices
  # Flow constraints
  cat("    Building incidence matrix...\n")
  if(is.null(constraint_matrix)){
    A1 <- build_incidence_matrix(nodes, data,
                                 nodes_to_balance = nodes_to_balance)
  }else{
    A1 <- constraint_matrix
  }

  n_p <- sum(!is.na(data[[colname_aadt]])) # No. of AADT values
  n_e <- nrow(data) # No. of traffic links (edges)
  n_n <- nrow(A1) # No. of traffic nodes (with entering and exiting traffic)

  if(n_p == 0 & n_n == 0){ # No data and no nodes to balance, both A1 and A2 will be meaningless
    result_data <- data |>
      dplyr::mutate(inla_pred = mu_v,
             inla_sd = marginal_sds,
             balanced_pred = mu_v,
             balanced_sd = marginal_sds)

    end_time <- Sys.time()

    return(list(
      results = result_data,
      diagnostics = list(
        method_used = NA,
        rank_deficit = NA,
        condition_number = NA,
        n_measurements = n_p,
        n_links = n_e,
        n_constraints = n_n,
        underdetermined = NA,
        runtime = end_time - start_time
      ),
      matrices = list(A1 = NULL, A2 = NULL)
    ))
  }

  if(is.null(n_n)){ # This happens if there is only one row/node
    n_n <- 1
    one_node_flag <- TRUE
    cat("THIS GROUP HAS ONLY ONE NODE!\n")
  }

  cat("    Building measurement matrix...\n")
  if(n_p == 0){
    cat("This cluster has no measured points. \n")
  }else{
    A2 <- as.matrix(build_measurement_matrix(data, colname_aadt = colname_aadt))
    d <- na.omit(data[[colname_aadt]])
    Sigma_epsilon_mark <- diag(as.vector(na.omit(data[[colname_measurement_sd]])^2),
                               nrow = length(na.omit(data[[colname_measurement_sd]])))
  }


  Sigma_v <- diag(as.vector(marginal_sds^2), nrow = length(marginal_sds))


  d <- if (n_p == 0) numeric(0) else d
  b <- c(rep(0, n_n), d)

  capped_count <- 0

  # Step 1: Handle extreme variances
  if (max(diag(Sigma_v)) > 1e10 || kappa(Sigma_v) > 1e12) {
    #normal_vars <- min(diag(Sigma_v)[diag(Sigma_v) < 1e6], 1e10)
    #max_reasonable <- max(normal_vars) * 10
    max_reasonable <- 1e10
    capped_count <- sum(diag(Sigma_v) > max_reasonable)
    diag(Sigma_v)[diag(Sigma_v) > max_reasonable] <- max_reasonable
  }

  # Step 2: Convert to dense matrices
  A1 <- as.matrix(A1)
  if(one_node_flag){ # If only one node the matrix will be wrong
    A1 <- t(A1)
  }
  A2 <- if (n_p == 0) matrix(numeric(0), nrow = 0, ncol = ncol(A1)) else A2

  A <- rbind(A1, A2)


  cat("    Creating Sigma_vb...\n")
  Sigma_vb <- Sigma_v %*% t(A)


  # Step 3: Check system properties
  rank_A1 <- qr(A1)$rank
  rank_A2 <- qr(A2)$rank
  rank_A <- qr(A)$rank
  rank_deficit <- nrow(A) - rank_A

  # Step 4: Build covariance matrix
  Sigma_A1 <- matrix(0, nrow = n_n, ncol = n_n)

  Sigma_epsilon_mark <- if (n_p == 0)
    matrix(numeric(0), 0, 0) else Sigma_epsilon_mark

  Sigma_epsilon <- as.matrix(Matrix::bdiag(
    list(Sigma_A1, Sigma_epsilon_mark)
  ))
  Sigma_b <- A %*% Sigma_v %*% t(A) + Sigma_epsilon
  Sigma_b <- (Sigma_b + t(Sigma_b))/2  # Ensure symmetry

  cat("    Inverting Sigma_b...\n")
  # Step 5: Robust inversion
  if (rank_deficit > 0) {
    # Use pseudoinverse for rank-deficient systems
    Sigma_b_inv <- MASS::ginv(Sigma_b)
    method <- "pseudoinverse"
  } else if (kappa(Sigma_b) > 1e12) {
    # Use regularization for ill-conditioned systems
    Sigma_b_reg <- Sigma_b + lambda * diag(nrow(Sigma_b))
    Sigma_b_inv <- solve(Sigma_b_reg)
    method <- "regularized"
  } else {
    # Standard inversion
    Sigma_b_inv <- solve(Sigma_b)
    method <- "standard"
  }

  # Step 6: Calculate posterior mean and variance
  mu_v_given_b <- mu_v + Sigma_vb %*% Sigma_b_inv %*% (b - A %*% mu_v)
  check <- Sigma_vb %*% Sigma_b_inv %*% t(Sigma_vb)
  Sigma_v_given_b <- Sigma_v - Sigma_vb %*% Sigma_b_inv %*% t(Sigma_vb)

  # Some elements may be less than 0, set them to small number
  below_zero_count <- sum(mu_v_given_b <= 0)
  mu_v_given_b[mu_v_given_b <= 0] <- 0

  # The same applies to the uncertainty
  posterior_var <- pmax(diag(Sigma_v_given_b), 1e-6)  # Ensure positivity

  end_time <- Sys.time()

  # Step 7: Return results with diagnostics

  # Return the data frame with added columns for inla model and balanced results.
  result_data <- data |>
    dplyr::mutate(inla_pred = mu_v,
           inla_sd = marginal_sds,
           balanced_pred = as.vector(mu_v_given_b),
           balanced_sd = sqrt(posterior_var))

  return(list(
    results = result_data,
    diagnostics = list(
      method_used = method,
      rank_deficit = rank_deficit,
      condition_number = kappa(Sigma_b),
      n_measurements = nrow(A2),
      n_links = ncol(A),
      n_constraints = nrow(A1),
      underdetermined = ncol(A2) > nrow(A2),
      capped_count = capped_count,
      below_zero_count = below_zero_count,
      runtime = end_time - start_time
    ),
    matrices = list(A1 = A1, A2 = A2)
  ))
}

