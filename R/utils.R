# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# add geometries to traffic data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Add traffic link geometries
#'
#' @param traffic_data Data frame with link id column
#' @param id_col Name of the id column (default "id")
#' @return sf object with geometries joined
#' @export
add_geometries <- function(traffic_data, id_col = "id") {
  # directed_links_geo_map_simple available from sysdata.rda
  traffic_data |>
    dplyr::left_join(directed_links_geo_map,
                     by = stats::setNames("id", id_col)) |>
    sf::st_as_sf()
}


#' NVDB objects for Leaflet maps
#'
#' @returns A list of nvdb_url, nvdb_attribution and nvdb_crs that are needed for using the NVDB map in Leaflet.
#' @export
#'
#' @examples
#' nvdb_objects()
nvdb_objects <- function(){
  # Use NVDB map in leaflet
  nvdb_map_url <-
    "https://nvdbcache.geodataonline.no/arcgis/rest/services/Trafikkportalen/GeocacheTrafikkJPG/MapServer/tile/{z}/{y}/{x}"

  nvdb_map_attribution <-
    "NVDB, Geovekst, kommunene og Open Street Map contributors (utenfor Norge)"

  nvdb_crs <- leaflet::leafletCRS(
    crsClass = "L.Proj.CRS", code = "EPSG:25833",
    proj4def = "+proj=utm +zone=33 +ellps=GRS80 +units=m +no_defs",
    resolutions = c(
      21674.7100160867,
      10837.35500804335,
      5418.677504021675,
      2709.3387520108377,
      1354.6693760054188,
      677.3346880027094,
      338.6673440013547,
      169.33367200067735,
      84.66683600033868,
      42.33341800016934,
      21.16670900008467,
      10.583354500042335,
      5.291677250021167,
      2.6458386250105836,
      1.3229193125052918,
      0.6614596562526459,
      0.33072982812632296,
      0.16536491406316148
    ),
    origin = c(-2500000.0, 9045984.0))

  return(list(nvdb_url = nvdb_map_url,
              nvdb_attribution = nvdb_map_attribution,
              nvdb_crs = nvdb_crs))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global variable bindings ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

utils::globalVariables(c(
  # Municipality and county columns
  "county_names", "countyIds", "municipality_names", "municipalityIds",
  # Directed traffic link columns
  "id", "parentTrafficLinkId", "isTrafficWithMetering", "functionalRoadClass",
  "functionClass", "highestSpeedLimit", "lowestSpeedLimit",
  "isNorwegianScenicRoute", "isFerryRoute", "isRamp", "isBlocked",
  "tollStationIds", "isInvalid", "yearAppliesTo", "startTrafficNodeId",
  "endTrafficNodeId", "municipalityIds", "countyIds", "roadSystemReferences",
  "roadCategory", "roadLinkIds", "roadNodeIds", "roadPlacements", "length",
  "maxLanes", "minLanes", "hasOnlyPublicTransportLanes", "associatedTrpIds",
  "lastYearAadt", "bestDataSourceAadt", "trafficVolumes",
  # Nodes columns
  "id", "isRoundabout", "numberOfIncomingLinks", "numberOfOutgoingLinks",
  "numberOfUndirectedLinks", "legalTurningMovements", "connectedTrafficLinkIds",
  "connectedTrafficLinkCandidateIds", "roadNodeIds", "roadSystemReferences",
  "roadSystems", "geometry",
  # Prepared traffic links
  "id", "parentTrafficLinkId", "functionalRoadClass", "functionClass",
  "highestSpeedLimit", "lowestSpeedLimit", "isNorwegianScenicRoute",
  "isFerryRoute", "isRamp", "yearAppliesTo", "startTrafficNodeId",
  "endTrafficNodeId", "municipality", "county", "roadSystem", "roadCategory",
  "length", "logLength", "maxLanes", "minLanes", "hasOnlyPublicTransportLanes",
  "lastYearAadt_aadt", "lastYearAadt_heavyRatio", "aadt", "coverage",
  "heavyRatio", "traffic_volume_source", "traffic_volume_year",
  "isMissinglastYearAadt_aadt", "isMissinglastYearAadt_heavyRatio",
  "bus_sd", "stopPointRef", "stopCertainty",
  # Bus data
  "id", "parentTrafficLinkId", "stopPointRef", "stopOnTrafficLink",
  "stopCertainty", "stopsServeDifferentBuses", "trikkestopp", "comment",
  "hasOnlyPublicTransportLanes", "isRamp", "length", "maxLanes", "minLanes",
  "startTrafficNodeId", "endTrafficNodeId", "text",
  "no_of_buses", "stopAggregatesDirections",
  # Others
  ".had_aadt", "bestDataSourceAadt_coverage", "bestDataSourceAadt_heavyRatio",
  "bestDataSourceAadt_trafficVolumeValue", "bestDataSourceAadt_year",
  "bus_aadt", "bus_source", "bus_year", "lastYearAadt_trafficVolumeValue",
  "mean_buses", "n_roadSystems", "n_stops", "n_trafficLinkRoadSystems",
  "number_of_candidate_links", "number_of_traffic_links", "sd", "sd_buses",
  "total_buses",
  "V", "cluster_id", "size", "merged_size", "traffic_node", "link_id.x",
  "link_id.y", "link_pair", "cluster_id",
  "estimatedAadt", "estimatedAadtStandardDeviation", "estimatedAadtHeavy",
  "estimatedAadtHeavyStandardDeviation",
  "lastYearAadt_heavyAadt", "balanced_pred", "balanced_sd", "n_duplicates",
  "density", "n", "label", "unbalanceable_node", "heavyAadt"

  ))
