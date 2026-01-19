# Testing Poisson vs negative binomial
library(dplyr)
year <- 2024
raw_path <- "data-raw/"

# Load data
prepared_traffic_links <- readRDS(paste0(raw_path, "prepared_traffic_links", year, ".rds"))

missing_fc <- directed_traffic_links %>% filter(functionClass == "unknown")
table(missing_fc$roadCategory)

#nodes <- readRDS(paste0(raw_path, "prepared_nodes", year, ".rds"))
adjacency_matrix <- readRDS(paste0(raw_path, "adjacency_matrix", year, ".rds"))
#clusters <- readRDS(paste0(raw_path, "clusters", year, ".rds"))

prepared_traffic_links$lastYearAadt_logAadt <- log(prepared_traffic_links$lastYearAadt_aadt + 1)

covariates <- ~ functionalRoadClass:maxLanes +
  functionalRoadClass:roadCategory +
  minLanes:roadCategory + functionalRoadClass +
  maxLanes + roadCategory +
  #hasOnlyPublicTransportLanes + #isFerryRoute + isNorwegianScenicRoute +
  functionalRoadClass*isRamp


# Fit negative binomial model
inla_nb <- fit_inla_model(
  data = prepared_traffic_links,
  adjacency_matrix,
  fixed_effects = covariates,
  iid_effects = "roadSystem",
  family = "nbinomial")
predictions_nb <- dplyr::full_join(prepared_traffic_links, inla_nb$predictions)

# Fit Poisson model
inla_poisson <- fit_inla_model(
  data = prepared_traffic_links,
  adjacency_matrix,
  fixed_effects = covariates,
  iid_effects = "roadSystem",
  family = "poisson")
predictions_poisson <- dplyr::full_join(prepared_traffic_links, inla_poisson$predictions)

# With last year's AADT
inla_poisson2 <- fit_inla_model(
  data = prepared_traffic_links,
  adjacency_matrix,
  fixed_effects = update(covariates, ~ . + lastYearAadt_logAadt),
  iid_effects = "roadSystem",
  family = "poisson")
predictions_poisson2 <- dplyr::full_join(prepared_traffic_links, inla_poisson2$predictions)


plot_prediction_uncertainty(predictions_nb, log10_y = TRUE, cap_sd = 100000,
                            balanced = FALSE, color_by = "traffic_volume_source")

plot_prediction_uncertainty(predictions_poisson, log10_y = TRUE, cap_sd = 100000,
                            balanced = FALSE, color_by = "traffic_volume_source")


plot_prediction_uncertainty(predictions_poisson2, log10_y = TRUE, cap_sd = 100000,
                            balanced = FALSE, color_by = "traffic_volume_source")

# Usikkerhet for modell UTEN fjorårets ÅDT
# Relativ usikkerhet-histogram fordelt på vegkategori og funksjonsklasse
plot_prediction_relative_uncertainty_histogram(
  directed_predictions = predictions_poisson, balanced = FALSE,
  cap_cv = 3, facet_by = "roadCategory", facet_by_2 = "functionClass",
  show_density = TRUE, reference_lines = c(0.5, 1, 2))

# Relativ usikkerhet-histogram fordelt på vegkategori
plot_prediction_relative_uncertainty_histogram(
  directed_predictions = predictions_poisson, balanced = FALSE,
  cap_cv = 3, facet_by = "roadCategory",
  reference_lines = c(0.2, 0.5))
ggplot2::ggsave(paste0(raw_path, "uncertainty_histograms_WITHOUT_last_year.png"),
                height = 6, width = 8)

# Usikkerhet for modell MED fjorårets ÅDT
# Relativ usikkerhet-histogram fordelt på vegkategori og funksjonsklasse
plot_prediction_relative_uncertainty_histogram(
  directed_predictions = predictions_poisson2, balanced = FALSE,
  cap_cv = 3, facet_by = "roadCategory", facet_by_2 = "functionClass",
  show_density = TRUE, reference_lines = c(0.5, 1, 2))

# Relativ usikkerhet-histogram fordelt på vegkategori
plot_prediction_relative_uncertainty_histogram(
  directed_predictions = predictions_poisson2, balanced = FALSE,
  cap_cv = 3, facet_by = "roadCategory",
  reference_lines = c(0.2, 0.5))
ggplot2::ggsave(paste0(raw_path, "uncertainty_histograms_WITH_last_year.png"),
                height = 6, width = 8)



# Kart
#
predictions_poisson <- predictions_poisson |>
mutate(relative_uncertainty = inla_sd/inla_pred,
       uncertainty_category = case_when(
         relative_uncertainty <= 0.2 ~ "low",
         relative_uncertainty <= 0.5 ~ "medium",
         TRUE ~ "high"))
table(predictions_poisson$uncertainty_category)

# First, create uncertainty categories
predictions_poisson2 <- predictions_poisson2 |>
  mutate(relative_uncertainty = inla_sd/inla_pred,
         uncertainty_category = case_when(
           relative_uncertainty <= 0.2 ~ "low",
           relative_uncertainty <= 0.5 ~ "medium",
           TRUE ~ "high"))

table(predictions_poisson2$uncertainty_category)

# Map with uncertainty categories
plot_traffic_links_map(
  directed_predictions = predictions_poisson2,
  color_by = "uncertainty_category",
  balanced = FALSE
)

# Prediksjoner mot fjorårets
plot_predictions_against_last_year(
  predictions_poisson, balanced = FALSE, log10_axis = TRUE,
  color_by = "traffic_volume_source", facet_by = "roadCategory")

plot_predictions_against_last_year(
  predictions_poisson, balanced = FALSE, log10_axis = TRUE,
  color_by = "traffic_volume_source", facet_by = "functionClass")
