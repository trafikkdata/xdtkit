
<!-- README.md is generated from README.Rmd. Please edit that file -->

# xdtkit

<!-- badges: start -->

<!-- badges: end -->

The goal of xdtkit is to make available functions for predicting AADT
(annual average daily traffic) on the Norwegian road network.

## Installation

You can install the development version of xdtkit from GitHub with:

``` r
# install.packages("pak")
pak::pak("trafikkdata/xdtkit")
```

## Example

The AADT modelling process requires some fairly large datasets that are
not publicly available and are too large to include with the package. To
show how the package should be used, we have included data from (some
county).

**To come: Example of AADT-process on subset of Norway.**

Until then, code for running the full AADT model can be found in the
repository
[xdt-modelling](https://github.com/trafikkdata/xdt-modelling).

``` r
library(xdtkit)

year <- 2025

# Traffic links: Load and preprocess
preprocessed_traffic_links <- preprocess_traffic_links(buskerud_directed_traffic_links, year = year)

missing_counts <- colSums(is.na(preprocessed_traffic_links))
missing_counts[missing_counts > 0]
#>               functionClass                    maxLanes 
#>                          18                           1 
#>                    minLanes hasOnlyPublicTransportLanes 
#>                           1                           1 
#>           lastYearAadt_aadt     lastYearAadt_heavyRatio 
#>                           1                           1 
#>      lastYearAadt_heavyAadt                        aadt 
#>                           1                        1129 
#>                    coverage                  heavyRatio 
#>                        1129                        1460 
#>                   heavyAadt       traffic_volume_source 
#>                        1460                        1129 
#>         traffic_volume_year 
#>                        1129


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

missing_counts <- colSums(is.na(prepared_traffic_links))
missing_counts[missing_counts > 0]
#>                  aadt              coverage            heavyRatio 
#>                  1129                  1129                  1460 
#>             heavyAadt traffic_volume_source   traffic_volume_year 
#>                  1460                  1129                  1129 
#>          stopPointRef         stopCertainty 
#>                  1774                  1774


# Nodes: Load and preprocess (may take a minute to run)
nodes <- identify_unbalanceable_nodes(buskerud_nodes, prepared_traffic_links) 
#> Joining with `by = join_by(id, roadSystems)`


# Adjacency matrix (may take several minutes to run)
adjacency_matrix <- build_adjacency_matrix(
  prepared_traffic_links,
  exclude_public_transport = TRUE) 
#> Building adjacency matrix for 1774 links...
#> Finding adjacent links...
#> Building sparse matrix from 15018 adjacency pairs...
#> Excluding 1 public transport links...
#> Adjacency matrix complete: 13326 non-zero entries


# Balancing clusters
clusters <- strategic_network_clustering(
  data = prepared_traffic_links,
  year = year, 
  boundary_links = c("Trafikkdata_continuous"))
#> Joining with `by = join_by(parentTrafficLinkId)`
#> Identifying mainland and island components...
#> Creating base clusters on mainland...
#> Merging small clusters...
#> Assigning barrier links to neighboring mainland clusters...
#> Assigning island components...
#> === Clustering Summary ===
#> Network Overview:
#> Total links: 955
#> Mainland: 954 links
#> Islands: 1 components, 1 links
#> Clustering Results:
#> Initial mainland clusters: 23
#> After merging: 1 mainland clusters
#> Total final clusters: 4 ( 1 mainland + 1 islands + 2 singletons)
#> Boundary Handling:
#> Duplicate assignments (boundaries): 0
#> Cluster Size Distribution:
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>     1.0     1.0     1.0   238.8   238.8   952.0


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Model setup
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

covariates <- ~ functionalRoadClass:maxLanes +
  functionalRoadClass:roadCategory +
  minLanes:roadCategory + functionalRoadClass +
  maxLanes + roadCategory +
  hasOnlyPublicTransportLanes + 
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
#> Preparing data for INLA model...
#> Fitting INLA model with family = poisson...
#> Model fitting complete.

inla_model_total
#> INLA Traffic Model
#> ==================
#> 
#> Number of predictions: 1774 
#> Family:  poisson 
#> Formula: aadt ~ f(spatial.idx, model = "besagproper", graph = adjacency_matrix, 
#>     adjust.for.con.comp = FALSE, constr = TRUE) + f(roadSystem, 
#>     model = "iid") + functionalRoadClass + maxLanes + roadCategory + 
#>     hasOnlyPublicTransportLanes + isRamp + lastYearAadt_logAadt + 
#>     functionalRoadClass:maxLanes + functionalRoadClass:roadCategory + 
#>     roadCategory:minLanes + functionalRoadClass:isRamp
#> 
#> Use $summary for model details
#> Use $predictions to access predictions data frame

predictions_total <- dplyr::full_join(prepared_traffic_links, inla_model_total$predictions)
#> Joining with `by = join_by(id)`

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.b Run balancing for total AADT.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
balanced_model_total <- balance_predictions(data = predictions_total,
                                            nodes = nodes,
                                            balancing_grouping_variable = clusters,
                                            nodes_to_balance = "complete nodes",
                                            year = year)
#> Balancing predictions for all groups... --------------
#>   Balancing predictions for group:  1 
#>     Building incidence matrix...
#>     Building measurement matrix...
#>     Creating Sigma_vb...
#>     Inverting Sigma_b...
#>   Balancing predictions for group:  2 
#>     Building incidence matrix...
#>   Balancing predictions for group:  3 
#>     Building incidence matrix...
#>     Building measurement matrix...
#>     Creating Sigma_vb...
#>     Inverting Sigma_b...
#>   Balancing predictions for group:  4 
#>     Building incidence matrix...
#>     Building measurement matrix...
#>     Creating Sigma_vb...
#>     Inverting Sigma_b...

predictions_total <- dplyr::full_join(predictions_total, balanced_model_total$balanced_res)
#> Joining with `by = join_by(id)`
```
