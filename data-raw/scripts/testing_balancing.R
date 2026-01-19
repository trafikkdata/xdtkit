# Testing balancing
year <- 2024
raw_path <- "data-raw/"

prepared_traffic_links <- readRDS(paste0(raw_path, "prepared/prepared_traffic_links", year, ".rds"))

predictions_inla <- readRDS("data-raw/results/predictions_total.rds") |>
  #dplyr::select(-balanced_pred, -balanced_sd) %>%
  dplyr::filter(county == "Telemark")

nodes <- readRDS(paste0(raw_path, "prepared/prepared_nodes", year, ".rds"))


predictions_balanced <- balance_predictions(data = predictions_inla,
                                            nodes = nodes,
                                            balancing_grouping_variable = "no_clustering",
                                            nodes_to_balance = "complete_nodes",
                                            year = year)

predictions <- dplyr::right_join(prepared_traffic_links, predictions_balanced$balanced_res)


plot_prediction_relative_uncertainty_histogram(predictions, cap_cv = 100)
summary(predictions$balanced_sd)




sigma_error <- calculate_measurement_error(
  data = prepared_traffic_links,
  colname_aadt = "aadt",
  source_col = "traffic_volume_source",
  year_col = "traffic_volume_year",
  coverage_col = "coverage",
  current_year = year,
  return_cv = TRUE
)

library(ggplot2)

ggplot(sigma_error, aes(x = cv_measurement, y = ggplot2::after_stat(density))) +
  geom_histogram() +
  facet_wrap(~ traffic_volume_source)
ggplot(sigma_error, aes(x = cv_temporal, y = ggplot2::after_stat(density))) +
  geom_histogram() +
  facet_wrap(~ traffic_volume_source)

ggplot(sigma_error, aes(x = cv_total, y = ggplot2::after_stat(density))) +
  geom_histogram() +
  facet_wrap(~ traffic_volume_source)


sigma_error <- calculate_measurement_error(
  data = prepared_traffic_links,
  colname_aadt = "heavyAadt",
  source_col = "traffic_volume_source",
  year_col = "traffic_volume_year",
  coverage_col = "coverage",
  current_year = year,
  return_cv = TRUE
)

ggplot(sigma_error, aes(x = cv_total, y = ggplot2::after_stat(density))) +
  geom_histogram() +
  facet_wrap(~ traffic_volume_source)

sum(is.nan(sigma_error$sigma_total))
