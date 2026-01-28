# The following code reads in GeoJSON-files with traffic links,
# directed traffic links, and nodes, and saves them into the following data
# objects:
library(sf)
library(dplyr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading data from raw files ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

year = 2025
raw_path = "data-raw/"

#raw_links_geo <- sf::st_read(paste0(raw_path, "traffic_links_", year, ".geojson"))
raw_directed_links_geo <- sf::st_read(paste0(raw_path, "raw/directed_traffic_links_", year, ".geojson"))

# Geo mappings (contain only id and geometry columns)
#links_geo_map <- raw_links_geo[c("id", "geometry")]
directed_links_geo_map <- raw_directed_links_geo[c("id", "geometry")] |>
  st_simplify(dTolerance = 1, preserveTopology = TRUE)
# Compare sizes
object.size(raw_directed_links_geo[c("id", "geometry")])
object.size(directed_links_geo_map)

usethis::use_data(directed_links_geo_map,
                  internal = TRUE,
                  compress = "xz",
                  overwrite = TRUE)







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# extract linestrings from geometry collections ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
table(st_geometry_type(directed_links_geo_map))

