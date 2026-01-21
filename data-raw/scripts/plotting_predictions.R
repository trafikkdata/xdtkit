# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
predictions_total <- readRDS("data-raw/predictions_total.rds")
predictions_heavy <- readRDS("data-raw/predictions_heavy.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examining total AADT results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predictions compared to last year's predictions
plot_predictions_against_last_year(
  predictions_total, log10_axis = TRUE, color_by = "traffic_volume_source")

# Relative uncertainty from INLA
plot_prediction_relative_uncertainty_histogram(
  predictions_total, cap_cv = 1, balanced = FALSE,
  reference_lines = c(0.125, 0.3))

# Relative uncertainty from balancing
plot_prediction_relative_uncertainty_histogram(
  predictions_total, cap_cv = 1, reference_lines = c(0.125, 0.3))

# Relative uncertainty (from balancing) by data source
plot_prediction_relative_uncertainty_histogram(
  predictions_total, cap_cv = 1, facet_by = "traffic_volume_source",
  reference_lines = c(0.125, 0.3))

# Standard deviation from INLA
plot_prediction_uncertainty(predictions_total, cap_sd = 25000, balanced = FALSE,
                            log10_x = TRUE, log10_y = TRUE)
# Standard deviation from balancing
plot_prediction_uncertainty(predictions_total, cap_sd = 25000000,
                            log10_x = TRUE, log10_y = TRUE,
                            color_by = "functionClass")

predictions_total <- predictions_total |>
  dplyr::mutate(
    uncertainty_total = dplyr::case_when(
      balanced_sd/balanced_pred <= 0.2 ~ "low",
      balanced_sd/balanced_pred <= 0.5 ~ "medium",
      TRUE ~ "high"
    ),
    not_measured_low = is.na(aadt) & uncertainty_total == "low"
  )

plot_traffic_links_map(
  predictions_total,
  color_by = "not_measured_low"
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Examining heavy AADT results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predictions compared to last year's predictions
plot_predictions_against_last_year(
  predictions_heavy, log10_axis = TRUE, color_by = "traffic_volume_source",
  heavy_vehicle = TRUE)

# Relative uncertainty from INLA
plot_prediction_relative_uncertainty_histogram(
  predictions_heavy, cap_cv = 4, balanced = FALSE, heavy_vehicle = TRUE,
  reference_lines = c(0.125, 0.3))

# Relative uncertainty from balancing
plot_prediction_relative_uncertainty_histogram(
  predictions_heavy, cap_cv = 4, heavy_vehicle = TRUE,
  reference_lines = c(0.125, 0.3))

