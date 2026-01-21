year <- 2025
raw_path <- "data-raw/"

# Traffic links: Load and preprocess
directed_traffic_links2025 <- jsonlite::fromJSON(paste0(raw_path, "raw/directed-traffic-links-", "2025", ".json"))
directed_traffic_links2024 <- jsonlite::fromJSON(paste0(raw_path, "raw/directed-traffic-links-", "2024", ".json"))

missing_counts <- colSums(is.na(directed_traffic_links))
missing_counts[missing_counts > 0]

pt_links2024 <- dplyr::filter(directed_traffic_links2024, hasOnlyPublicTransportLanes)
pt_links2025 <- dplyr::filter(directed_traffic_links2025, hasOnlyPublicTransportLanes)

pt_links_both_years <- intersect(pt_links2025$id, pt_links2024$id)

new_pt_links <- dplyr::filter(pt_links2025, !(id %in% pt_links_both_years))


stops_on_traffic_links2024 <- read.csv(paste0(raw_path, "raw/Trafikklenker med holdeplasser ", 2024, ".csv"))
bus_counts2025 <- pt_links2025 |>
  dplyr::select(id, parentTrafficLinkId, roadSystemReferences) |>
  dplyr::left_join(stops_on_traffic_links2024) |>
  dplyr::mutate(onlyPtLastYear = id %in% pt_links2024$id) |>
  dplyr::select(id, roadSystemReferences, onlyPtLastYear, stopPointRef, stopOnTrafficLink,
                stopCertainty, stopsServeDifferentBuses, trikkestopp, comment)

bus_counts2025$roadSystemReferences <- sapply(bus_counts2025$roadSystemReferences, paste, collapse = ", ")

write.csv(bus_counts2025, paste0(raw_path, "prepared_data/pt_tl.csv"))

table(bus_counts2025$onlyPtLastYear)


bus_counts <- read.csv(paste0(raw_path, "raw/holdeplasspasseringer_entur_", year, ".csv"))
tl_med_holdeplasser <- read.csv(paste0(raw_path, "raw/Trafikklenker med holdeplasser ", year, ".csv"))
