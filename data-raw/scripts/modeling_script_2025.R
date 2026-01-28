# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Preprocess data and create everything that takes time.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
year <- 2025
raw_path <- "data-raw/"

# Traffic links: Load and preprocess
directed_traffic_links <- jsonlite::fromJSON(paste0(raw_path, "raw/directed-traffic-links-", year, ".json"))
preprocessed_traffic_links <- preprocess_traffic_links(directed_traffic_links, year = year)

missing_counts <- colSums(is.na(preprocessed_traffic_links))
missing_counts[missing_counts > 0]

# Bus data: Load and preprocess
stops_on_traffic_links <- read.csv(paste0(raw_path, "raw/Trafikklenker med holdeplasser ", year, ".csv"))
bus_counts <- read.csv(paste0(raw_path, "raw/holdeplasspasseringer_entur_", year, ".csv"))

bus_aadt <- calculate_bus_aadt(stops_on_traffic_links, bus_counts, year = year)

# Fill missing values and add bus data
prepared_traffic_links <- fill_missing_values(
  df = preprocessed_traffic_links,
  unknown_impute_columns = c("functionClass", "highestSpeedLimit", "lowestSpeedLimit","maxLanes", "minLanes"),
  mode_impute_columns = c("hasOnlyPublicTransportLanes"),
  median_impute_columns = c("lastYearAadt_aadt", "lastYearAadt_heavyRatio")) |>
  remove_negative_aadt() |>
  add_logLastYear() |>
  join_bus_to_traffic(bus_aadt)

missing_counts <- colSums(is.na(prepared_traffic_links))
missing_counts[missing_counts > 0]


# Nodes: Load and preprocess
raw_nodes_geo <- sf::st_read(paste0(raw_path, "raw/traffic-nodes-", year, ".geojson"))
nodes <- identify_unbalanceable_nodes(raw_nodes_geo, prepared_traffic_links) # This may take a while to run

# Adjacency matrix
adjacency_matrix <- build_adjacency_matrix(prepared_traffic_links,
                                           exclude_public_transport = TRUE)

# Balancing clusters
clusters <- strategic_network_clustering(
  data = prepared_traffic_links,
  boundary_links = c("Trafikkdata_continuous", "AutoPASS"),
  extra_boundary_links = c("0.47813092@181362-0.69434556@181186", # TL vest for Oslo
                           "0.64310018@971787-0.44481682@971788",# TL'er øst for Oslo
                           "0.59497974@971566-0.82908906@444258",
                           "0.31944922@443497-0.40997724@971559",
                           "0.51111532@971558-0.31944922@443497",
                           "0.78140309@971504-0.63295502@443465",
                           "0.65120232-0.91722091@705187", # TL'er nord-øst for Oslo
                           "0.69089123@704623-0.49119695@705214",
                           "0.0@2472765-0.55746674@2472766", # Tl'er nord for Oslo
                           "0.9628257@1060294-0.68755836@1060295"
                           ),
  year = year)

data_with_clusters <- dplyr::left_join(prepared_traffic_links, clusters,
                                       by = join_by(parentTrafficLinkId == id))
table(data_with_clusters$cluster_id)
plot_traffic_links_simple_map(dplyr::filter(data_with_clusters, cluster_id == 18),
                              color_by = "traffic_volume_source")

# Save everything (prepared_traffic_links, nodes, adjacency matrix, clusters)
saveRDS(prepared_traffic_links, paste0(raw_path, "prepared_data/prepared_traffic_links", year, ".rds"))
saveRDS(nodes, paste0(raw_path, "prepared_data/prepared_nodes", year, ".rds"))
saveRDS(adjacency_matrix, paste0(raw_path, "prepared_data/adjacency_matrix", year, ".rds"))
saveRDS(clusters, paste0(raw_path, "prepared_data/clusters", year, ".rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model setup
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load data
prepared_traffic_links <- readRDS(paste0(raw_path, "prepared_data/prepared_traffic_links", year, ".rds"))
nodes <- readRDS(paste0(raw_path, "prepared_data/prepared_nodes", year, ".rds"))
adjacency_matrix <- readRDS(paste0(raw_path, "prepared_data/adjacency_matrix", year, ".rds"))
clusters <- readRDS(paste0(raw_path, "prepared_data/clusters", year, ".rds"))

covariates <- ~ functionalRoadClass:maxLanes +
  functionalRoadClass:roadCategory +
  minLanes:roadCategory + functionalRoadClass +
  maxLanes + roadCategory +
  hasOnlyPublicTransportLanes + #isFerryRoute + isNorwegianScenicRoute +
  functionalRoadClass*isRamp

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.a Run INLA model for total AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

covariates_total <- update(covariates, ~ . + lastYearAadt_logAadt)

inla_model_total <- fit_inla_model(
  data = prepared_traffic_links,
  adjacency_matrix,
  fixed_effects = covariates_total,
  iid_effects = "roadSystem",
  family = "poisson")

inla_model_total

predictions_total <- dplyr::full_join(prepared_traffic_links, inla_model_total$predictions)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.b Run balancing for total AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

balanced_model_total <- balance_predictions(data = predictions_total,
                                      nodes = nodes,
                                      balancing_grouping_variable = clusters,
                                      nodes_to_balance = "complete nodes",
                                      year = year)

predictions_total <- dplyr::full_join(predictions_total, balanced_model_total$balanced_res)

saveRDS(predictions_total, "data-raw/results/predictions_total.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.a Run INLA model for heavy AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

covariates_heavy <- update(covariates, ~ . + lastYearAadt_logHeavyAadt)

inla_model_heavy <- fit_inla_model(
  data = prepared_traffic_links,
  adjacency_matrix,
  fixed_effects = covariates_heavy,
  iid_effects = "roadSystem",
  family = "poisson",
  heavy_vehicle = TRUE)

inla_model_heavy

predictions_heavy <- dplyr::full_join(prepared_traffic_links, inla_model_heavy$predictions)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3.b Run balancing for heavy AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
balanced_model_heavy <- balance_predictions(data = predictions_heavy,
                                      nodes = nodes,
                                      balancing_grouping_variable = clusters,
                                      nodes_to_balance = "complete nodes",
                                      heavy_vehicle = TRUE,
                                      year = year)


predictions_heavy <- dplyr::full_join(predictions_heavy, balanced_model_heavy$balanced_res)

saveRDS(predictions_heavy, "data-raw/results/predictions_heavy.rds")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Publish to GitHub.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

predictions <- dplyr::full_join(predictions_total, predictions_heavy)


# 1. Open your .Renviron file and add token there as
# TRAFIKKDATA_GH_PKG_TOKEN=ghp_YourActualTokenHere1234567890abcdef
# (no spaces around "=", no quotes, each environment variable on its own line.)
# usethis::edit_r_environ()

data_2025 <- predictions |>
  dplyr::mutate(
    estimatedAadt = dplyr::case_when(
      traffic_volume_source == "Bus" | is.na(aadt) ~ balanced_pred,
      .default = aadt),
    estimatedAadtHeavy = dplyr::case_when(
      traffic_volume_source == "Bus" | is.na(heavyAadt) ~ balanced_pred,
      .default = heavyAadt)) |>
  dplyr::select(
    id, parentTrafficLinkId, estimatedAadt,
    estimatedAadtStandardDeviation = balanced_sd,
    estimatedAadtHeavy,
    estimatedAadtHeavyStandardDeviation = balanced_sd_heavy)

colSums(is.na(data_2025))


# Try uploading (as test release)
#upload_df_to_github_release(data_2025, year = 2025, prerelease = TRUE, overwrite = TRUE)


# An actual release
#upload_df_to_github_release(data_2025, year = 2025, prerelease = FALSE, overwrite = TRUE)









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualize the difference ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Which links lost connections?
neighbor_diff <- Matrix::rowSums(adj_matrix_full) - Matrix::rowSums(adj_matrix_excl_pt)

tibble::tibble(
  link_id = 1:10,
  has_pt_only = link_data$hasOnlyPublicTransportLanes,
  neighbors_lost = neighbor_diff
) |>
  dplyr::filter(neighbors_lost > 0 | has_pt_only)
