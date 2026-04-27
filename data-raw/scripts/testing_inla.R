library(xdtkit)
library(ggplot2)
library(INLA)

year <- 2025

# Traffic links: Load and preprocess
preprocessed_traffic_links <- preprocess_traffic_links(buskerud_directed_traffic_links, year = year)

# Bus data: Load and preprocess
bus_aadt <- calculate_bus_aadt(stops_on_traffic_links, bus_counts, year = year)

# Fill missing values and add bus data
prepared_traffic_links <- fill_missing_values(
  df = preprocessed_traffic_links,
  unknown_impute_columns = c("functionClass", "highestSpeedLimit", "lowestSpeedLimit","maxLanes", "minLanes"),
  mode_impute_columns = c("hasOnlyPublicTransportLanes"),
  median_impute_columns = c("lastYearAadt_aadt", "lastYearAadt_heavyRatio",
                            "lastYearAadt_heavyAadt")) |>
  remove_negative_aadt() |>
  add_logLastYear() |>
  join_bus_to_traffic(bus_aadt)

# Adjacency matrix (may take several minutes to run)
adjacency_matrix <- build_adjacency_matrix(
  prepared_traffic_links,
  exclude_public_transport = TRUE)

# Balancing clusters
clusters <- strategic_network_clustering(
  data = prepared_traffic_links,
  year = year,
  boundary_links = c("Trafikkdata_continuous"))

# Nodes: Load and preprocess (may take a minute to run)
nodes <- identify_unbalanceable_nodes(buskerud_nodes, prepared_traffic_links)

prepared_traffic_links$spatial.idx <- 1:nrow(prepared_traffic_links)

# formula <- aadt ~ f(spatial.idx, model = "besagproper", graph = adjacency_matrix,
#                     adjust.for.con.comp = FALSE, constr = TRUE) + f(roadSystem,
#                                                                     model = "iid") +
#   functionalRoadClass + lastYearAadt_logAadt
#
# formula <- aadt ~ lastYearAadt_logAadt + functionalRoadClass

formula <- aadt ~ f(spatial.idx, model = "besagproper", graph = adjacency_matrix,
         adjust.for.con.comp = FALSE, constr = TRUE) + f(roadSystem,
                                                         model = "iid") + functionalRoadClass + maxLanes + roadCategory +
  hasOnlyPublicTransportLanes + isRamp + lastYearAadt_logAadt +
  functionalRoadClass:maxLanes + functionalRoadClass:roadCategory +
  roadCategory:minLanes + functionalRoadClass:isRamp

mod_test <- inla(formula, data = prepared_traffic_links, family = "poisson")

summary(mod_test)

plot(prepared_traffic_links$lastYearAadt_aadt, mod_test$summary.fitted.values$mean)


prepared_traffic_links$inla_pred <- mod_test$summary.fitted.values$mean

ggplot(prepared_traffic_links, aes(x = aadt, y = inla_pred)) +
  geom_point()
