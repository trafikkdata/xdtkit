# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fit INLA traffic model ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Fit INLA model for traffic volume prediction
#'
#' @param data Data frame containing traffic data with required columns: id, aadt
#' @param adjacency_matrix Adjacency matrix for spatial Besag term (single matrix, for backward compatibility)
#' @param adjacency_matrices List of adjacency matrices for multiple spatial terms (optional)
#' @param spatial_ids Character vector of spatial ID column names corresponding to adjacency_matrices (optional)
#' @param fixed_effects Formula for fixed effects (default: ~ 1 for intercept only)
#' @param iid_effects Character vector of variable names for IID random effects (default: "roadSystem")
#' @param spatial_term Formula for spatial random effect (default: Besag proper with constraints)
#'   Note: spatial_term is only used when adjacency_matrix is provided (single spatial effect)
#' @param heavy_vehicle Logical. Whether or not the model is for heavy vehicles (using the column "heavyAadt" as response) or all vehicles (using column "aadt"). Default FALSE.
#' @param family INLA family, either "poisson" or "nbinomial" (default: "poisson")
#'
#' @return Object of class "inla_traffic_model" containing:
#'   \item{predictions}{Data frame with id, pred (posterior median), sd (posterior SD)}
#'   \item{model_summary}{Summary of INLA model}
#'   \item{inla_model}{Full INLA model object}
#'
#' @examples
#' \dontrun{
#' # Single spatial effect (backward compatible)
#' fit_inla_model(
#'   data = prepared_traffic_links,
#'   adjacency_matrix,
#'   iid_effects = "roadSystem",
#'   family = "poisson")
#'
#'
#'
#'
#'
#' }
#' @export
fit_inla_model <- function(data,
                           adjacency_matrix = NULL,
                           adjacency_matrices = NULL,
                           spatial_ids = NULL,
                           fixed_effects = ~ 1,
                           iid_effects = "roadSystem",
                           spatial_term = ~ f(spatial.idx,
                                              model = "besagproper",
                                              graph = adjacency_matrix,
                                              adjust.for.con.comp = FALSE,
                                              constr = TRUE),
                           heavy_vehicle = FALSE,
                           family = "poisson") {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Input validation ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  response <- if(heavy_vehicle) "heavyAadt" else "aadt"

  # Check required columns
  required_cols <- c("id", response)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing required columns in data: ", paste(missing_cols, collapse = ", "))
  }

  # Check IID effect variables exist
  if (!is.null(iid_effects)) {
    missing_iid <- setdiff(iid_effects, names(data))
    if (length(missing_iid) > 0) {
      stop("IID effect variables not found in data: ", paste(missing_iid, collapse = ", "))
    }
  }

  # Validate family
  family <- match.arg(family, choices = c("poisson", "nbinomial"))

  # Determine which spatial specification to use
  use_multiple_spatial <- !is.null(adjacency_matrices) && !is.null(spatial_ids)
  use_single_spatial <- !is.null(adjacency_matrix)

  if (use_multiple_spatial && use_single_spatial) {
    stop("Provide either 'adjacency_matrix' (single) OR 'adjacency_matrices + spatial_ids' (multiple), not both")
  }

  if (!use_multiple_spatial && !use_single_spatial) {
    stop("Must provide either 'adjacency_matrix' or 'adjacency_matrices + spatial_ids'")
  }

  # Validate multiple spatial effects setup
  if (use_multiple_spatial) {
    if (length(adjacency_matrices) != length(spatial_ids)) {
      stop("adjacency_matrices and spatial_ids must have the same length")
    }

    # Check that spatial_id columns exist in data
    missing_spatial_cols <- setdiff(spatial_ids, names(data))
    if (length(missing_spatial_cols) > 0) {
      stop("Spatial ID columns not found in data: ", paste(missing_spatial_cols, collapse = ", "))
    }

    # Check dimensions of each adjacency matrix
    for (i in seq_along(adjacency_matrices)) {
      adj_mat <- adjacency_matrices[[i]]
      # Count non-NA values in corresponding spatial_id column
      n_valid <- sum(!is.na(data[[spatial_ids[i]]]))

      if (nrow(adj_mat) != n_valid || ncol(adj_mat) != n_valid) {
        warning("Adjacency matrix ", i, " has dimensions ", nrow(adj_mat), "x", ncol(adj_mat),
                " but spatial ID '", spatial_ids[i], "' has ", n_valid, " non-NA values. ",
                "Make sure adjacency matrix was built from the filtered subset.")
      }
    }
  }

  # Validate single spatial effect setup
  if (use_single_spatial) {
    if (nrow(data) != nrow(adjacency_matrix) || nrow(data) != ncol(adjacency_matrix)) {
      stop("Dimensions of adjacency_matrix (", nrow(adjacency_matrix), "x", ncol(adjacency_matrix),
           ") do not match number of rows in data (", nrow(data), ")")
    }

    # Check spatial_term is a formula
    if (!inherits(spatial_term, "formula")) {
      stop("spatial_term must be a formula (e.g., ~ f(spatial.idx, model = 'besagproper', ...))")
    }
  }

  # Check fixed_effects is a formula
  if (!inherits(fixed_effects, "formula")) {
    stop("fixed_effects must be a formula (e.g., ~ speed_limit + lanes)")
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare data and formula ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  message("Preparing data for INLA model...")

  # Start with response variable
  formula_full <- stats::as.formula(paste(response, "~ 1"))

  # Add spatial term(s)
  if (use_single_spatial) {
    # Create spatial index (needed for default spatial term)
    data$spatial.idx <- 1:nrow(data)

    # Add single spatial term
    formula_full <- stats::update(formula_full, spatial_term)

  } else if (use_multiple_spatial) {
    # Add multiple spatial terms
    for (i in seq_along(spatial_ids)) {
      spatial_id <- spatial_ids[i]
      adj_mat <- adjacency_matrices[[i]]

      # Build spatial term for this effect
      spatial_formula <- stats::as.formula(
        paste0("~ . + f(", spatial_id,
               ", model = 'besagproper'",
               ", graph = adjacency_matrices[[", i, "]]",
               ", adjust.for.con.comp = FALSE",
               ", constr = TRUE)")
      )

      formula_full <- stats::update(formula_full, spatial_formula)
    }
  }

  # Add IID random effects
  if (!is.null(iid_effects) && length(iid_effects) > 0) {
    for (var in iid_effects) {
      formula_full <- stats::update(
        formula_full,
        stats::as.formula(paste("~ . + f(", var, ', model = "iid")')))
    }
  }

  # Add fixed effects
  if (length(all.vars(fixed_effects)) > 0) {
    # Extract right-hand side of fixed_effects formula as a character string
    fixed_terms <- as.character(fixed_effects)[2]  # [2] gets the RHS of the formula
    formula_full <- stats::update(formula_full,
                           stats::as.formula(paste("~ . + ", fixed_terms)))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Fit INLA model ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  message("Fitting INLA model with family = ", family, "...")

  model <- INLA::inla(formula_full,
                      family = family,
                      data = data,
                      control.predictor = list(link = 1))

  message("Model fitting complete.")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract predictions ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Determine suffix based on response variable
  suffix <- if(heavy_vehicle) "_heavy" else ""

  predictions <- data.frame(
    id = data$id,
    pred = round(model$summary.fitted.values[, "0.5quant"]),
    sd = round(model$summary.fitted.values[, "sd"])
  ) |>
    stats::setNames(c("id", paste0("inla_pred", suffix), paste0("inla_sd", suffix)))


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create return object ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  result <- list(
    predictions = predictions,
    model_summary = summary(model),
    inla_model = model,
    formula = formula_full,
    family = family
  )

  class(result) <- "inla_traffic_model"

  return(result)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# S3 print method ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @export
print.inla_traffic_model <- function(x, ...) {
  cat("INLA Traffic Model\n")
  cat("==================\n\n")
  cat("Number of predictions:", nrow(x$predictions), "\n")
  cat("Family: ", x$family, "\n")
  cat("Formula: ")
  print(x$formula, showEnv = FALSE)
  cat("\n")
  cat("Use $summary for model details\n")
  cat("Use $predictions to access predictions data frame\n")
  invisible(x)
}
