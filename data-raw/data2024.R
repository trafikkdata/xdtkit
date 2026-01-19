# The following code reads in GeoJSON-files with traffic links,
# directed traffic links, and nodes, and saves them into the following data
# objects:
library(sf)
library(dplyr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading data from raw files ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

year = 2024
raw_path = "data-raw/"

#raw_links_geo <- sf::st_read(paste0(raw_path, "traffic_links_", year, ".geojson"))
raw_directed_links_geo <- sf::st_read(paste0(raw_path, "directed_traffic_links_", year, ".geojson"))
raw_directed_links <- jsonlite::fromJSON(paste0(raw_path, "directed-traffic-links-", year, ".json"))
raw_nodes_geo <- sf::st_read(paste0(raw_path, "traffic-nodes-", year, ".geojson"))

# Geo mappings (contain only id and geometry columns)
#links_geo_map <- raw_links_geo[c("id", "geometry")]
directed_links_geo_map <- raw_directed_links_geo[c("id", "geometry")] |>
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
# Compare sizes
object.size(raw_directed_links_geo[c("id", "geometry")])
object.size(directed_links_geo_map)

nodes_geo_map <- raw_nodes_geo[c("id", "geometry")]

# Data frames without geometries
#traffic_links <- sf::st_drop_geometry(raw_links_geo)
directed_traffic_links <- raw_directed_links
nodes <- sf::st_drop_geometry(raw_nodes_geo)

# Save dataframes without geometries as package data
#usethis::use_data(traffic_links, overwrite = TRUE)
#usethis::use_data(directed_traffic_links, overwrite = TRUE)
#usethis::use_data(nodes, overwrite = TRUE)
usethis::use_data(directed_links_geo_map,
                  internal = TRUE,
                  compress = "xz",
                  overwrite = TRUE)

# Bus data
stops_on_traffic_links_data <- read.csv("data-raw/Trafikklenker med holdeplasser.csv")
bus_counts_data <- read.csv("data-raw/holdeplasspasseringer_entur.csv")





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract linestrings from geometry collections ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(st_geometry_type(directed_links_geo_map))

