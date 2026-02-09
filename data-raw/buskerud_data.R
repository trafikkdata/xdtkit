# Testing running the model on a subset

library(dplyr)
library(purrr)

year <-  2025
raw_path <- "data-raw/"

# TRAFFIC LINKS: Load and preprocess ----
directed_traffic_links <- jsonlite::fromJSON(paste0(raw_path, "raw/directed-traffic-links-", year, ".json"))

buskerud_directed_traffic_links <- directed_traffic_links |>
  dplyr::filter(purrr::map_lgl(countyIds, ~ .x[1] == 33))


usethis::use_data(buskerud_directed_traffic_links, compress = "xz",  overwrite = TRUE)


# BUS DATA: Load and preprocess ----
stops_on_traffic_links <- read.csv(paste0(raw_path, "raw/Trafikklenker med holdeplasser ", year, ".csv"))
bus_counts <- read.csv(paste0(raw_path, "raw/holdeplasspasseringer_entur_", year, ".csv"))
usethis::use_data(stops_on_traffic_links, compress = "xz",  overwrite = TRUE)
usethis::use_data(bus_counts, compress = "xz",  overwrite = TRUE)


# NODES: Load and preprocess ----
raw_nodes_geo <- sf::st_read(paste0(raw_path, "raw/traffic-nodes-", year, ".geojson"))

# Get the traffic link IDs from your filtered county dataset
county_link_ids <- buskerud_directed_traffic_links$parentTrafficLinkId

# Filter nodes where any connected link is in the county dataset
buskerud_nodes <- raw_nodes_geo %>%
  filter(map_lgl(connectedTrafficLinkIds,
                 ~ any(.x %in% county_link_ids)))

usethis::use_data(buskerud_nodes, compress = "xz",  overwrite = TRUE)




