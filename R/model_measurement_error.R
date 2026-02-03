# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Build measurement matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build measurement matrix
#'
#' @param data Data frame containing the row "aadt"
#' @param colname_aadt Column name of column containing AADT
#'
#' @returns A matrix with each row corresponding to a traffic link with a measured traffic volume, and columns corresponding to all traffic links. For each row, the entry corresponding to the given traffic link has value 1.
#' @export
#'
build_measurement_matrix <- function(data, colname_aadt = "aadt") {
  n_e <- nrow(data)
  measured_links <- which(!is.na(data[[colname_aadt]]))
  n_p <- length(measured_links)

  # Create sparse matrix directly
  A_2 <- Matrix::sparseMatrix(
    i = 1:n_p,           # row indices
    j = measured_links,   # column indices
    x = 1,               # values (all 1s)
    dims = c(n_p, n_e)
  )

  return(as.matrix(A_2))
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Calculate Measurement Error Variances for Traffic Data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Calculate measurement error variance for traffic volume data
#'
#' This function computes measurement error variances based on:
#' 1. Data source type (continuous, periodic, ferry, etc.)
#' 2. Temporal coverage (for continuous and periodic measurements)
#' 3. Age of data (temporal decay)
#' 4. For derived values: error propagation from contributing segments
#'
#' @param data Data frame containing traffic measurements
#' @param colname_aadt Name of column containing AADT values
#' @param source_col Name of column containing source type
#' @param year_col Name of column containing measurement year
#' @param coverage_col Name of column containing temporal coverage (0-1),
#'        required for "Trafikkdata_continuous", "Trafikkdata_periodic", and "AutoPASS"
#' @param current_year Current year for temporal decay calculation (default: 2024)
#' @param params List of parameters (see details)
#' @param return_cv Logical, if TRUE the function returns a data frame with the CV values. Useful for debugging.
#'
#' @details
#' Parameters list should contain:
#' - cv_base_continuous: Base CV for continuous measurements
#' - cv_base_autopass: Base CV for AutoPASS
#' - k_missing_continuous: Factor for continuous missing data uncertainty
#' - k_missing_periodic: Factor for periodic missing data uncertainty
#' - cv_ferry: CV for ferry data
#' - cv_external: CV for external municipal data
#' - cv_bus: CV for bus data
#' - cv_annual: Annual CV for temporal decay
#' - cv_max_periodic: Maximum CV for periodic measurements (default: NULL for no cap)
#'
#' @return Vector of measurement error standard deviations (same length as input data)
#'
calculate_measurement_error <- function(
    data,
    colname_aadt = "aadt",
    source_col = "traffic_volume_source",
    year_col = "traffic_volume_year",
    coverage_col = "coverage",
    current_year = 2024,
    params = list(),
    return_cv = FALSE
) {

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set default parameters ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  default_params <- list(
    cv_base_continuous = 0.01,    # Base error for continuous TRP
    cv_base_periodic = 0.02,       # Base error for continuous TRP
    cv_base_autopass = 0.01,      # Base error for AutoPASS
    cv_base_derived = 0.02,
    cv_base_derived_heavy = 0.1,
    k_missing_continuous = 0.56,     # Factor for missing data (0.5 = trust factor curves moderately)
    k_missing_periodic = 0.45,     # Factor for missing data (0.5 = trust factor curves moderately)
    cv_ferry = 0.03,               # Base error for ferry data
    cv_external = 0.4,             # 20% for external municipal data
    cv_bus = 0.4,                 # 15% for bus data
    cv_annual = 0.02,              # 2% annual temporal decay
    cv_max_periodic = NULL         # No cap on periodic CV by default
  )

  # Merge user params with defaults
  params <- utils::modifyList(default_params, params)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract data columns ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  aadt <- data[[colname_aadt]]
  source <- data[[source_col]]
  year <- data[[year_col]]
  coverage <- data[[coverage_col]]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate temporal uncertainty component ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  delta_t <- pmax(0, current_year - year)  # Years since measurement
  sigma_temporal <- aadt * params$cv_annual * delta_t

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate measurement uncertainty by source type ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  sigma_measurement <- numeric(nrow(data))

  for (i in seq_len(nrow(data))) {

    src <- source[i]
    aadt_val <- aadt[i]
    cov <- coverage[i]

    if (is.na(src)){

      sigma_measurement[i] <- NA_real_

    } else if (src == "Trafikkdata_continuous") {

      if (is.na(cov)) {
        stop("Coverage required for Trafikkdata_continuous at row ", i)
      }

      cv_base <- params$cv_base_continuous
      cv_missing <- params$k_missing_continuous * (1 - cov)
      cv_total <- sqrt(cv_base^2 + cv_missing^2)
      sigma_measurement[i] <- aadt_val * cv_total

    } else if (src == "Trafikkdata_periodic") {

      if (is.na(cov)) {
        stop("Coverage required for Trafikkdata_periodic at row ", i)
      }

      # Use same formula as continuous, but coverage is much lower
      cv_base <- params$cv_base_periodic
      cv_missing <- params$k_missing_periodic * (1 - cov)
      cv_total <- sqrt(cv_base^2 + cv_missing^2)

      # Apply cap if specified
      if (!is.null(params$cv_max_periodic)) {
        cv_total <- min(cv_total, params$cv_max_periodic)
      }

      sigma_measurement[i] <- aadt_val * cv_total

    } else if (src == "AutoPASS") {

      if (is.na(cov)) {
        stop("Coverage required for AutoPASS at row ", i)
      }

      cv_base <- params$cv_base_autopass
      cv_missing <- params$k_missing_continuous * (1 - cov)
      cv_total <- sqrt(cv_base^2 + cv_missing^2)
      sigma_measurement[i] <- aadt_val * cv_total

    } else if (src == "Derived") {

      if (is.na(cov)) {
        stop("Coverage required for Trafikkdata_derived at row ", i)
      }

      if(colname_aadt == "heavyAadt"){
        cv_base <- params$cv_base_derived_heavy
      }else{
        cv_base <- params$cv_base_derived
      }

      cv_missing <- params$k_missing_continuous * (1 - cov)
      cv_total <- sqrt(cv_base^2 + cv_missing^2)
      sigma_measurement[i] <- aadt_val * cv_total

    } else if (src == "Ferry") {

      sigma_measurement[i] <- aadt_val * params$cv_ferry

    } else if (src == "External_municipal") {

      sigma_measurement[i] <- aadt_val * params$cv_external

    } else if (src == "Bus") {

      sigma_measurement[i] <- aadt_val * params$cv_bus

    } else {
      warning("Unknown source type: ", src, " at row ", i)
      sigma_measurement[i] <- NA_real_
    }
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Combine measurement and temporal uncertainty ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # For non-derived values, combine measurement and temporal components
  sigma_total <- sqrt(sigma_measurement^2 + sigma_temporal^2)

  # For derived values, temporal uncertainty still applies
  # (measurement uncertainty will be added later based on contributing segments)
  #sigma_total[source == "Derived"] <- sigma_temporal[source == "Derived"]

  if(return_cv){
    cv_data <- data.frame(id = data$id, aadt = aadt,
                          traffic_volume_source = source,
                          year = year,
                          sigma_measurement = sigma_measurement,
                          sigma_temporal = sigma_temporal,
                          sigma_total = sigma_total) |>
      dplyr::mutate(cv_measurement = round(sigma_measurement/aadt, 3),
                    cv_temporal = round(sigma_temporal/aadt, 3),
                    cv_total = round(sigma_total/aadt, 3)) |>
      tidyr::drop_na(aadt)
    return(cv_data)
  }

  return(sigma_total)
}
