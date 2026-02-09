
#' Municipality numbers and names
#'
#' Municipality numbers and names for all Norwegian municipalities.
#'
#' @format A data frame with 357 rows and 3 variables.
#' \describe{
#'   \item{kommunenavn}{Municipality name}
#'   \item{kommunenummer}{Municipality number}
#'   \item{fylke}{County}
#' }
#' @docType data
#' @name municipality_names
#' @usage data(municipality_names)
#' @keywords data
NULL

#' County numbers and names
#'
#' County numbers and names for all Norwegian counties
#'
#' @format A data frame with 357 rows and 2 variables.
#' \describe{
#'   \item{fylkesnavn}{County name}
#'   \item{fylkesnummer}{County number}
#' }
#' @docType data
#' @name county_names
#' @usage data(county_names)
#' @keywords data
NULL

#' Directed traffic links for Buskerud
#'
#' Directed traffic links for Buskerud. Raw, unprocessed data to test package functions on.
#'
#' @format A data frame with 1774 rows and 31 variables.
#' \describe{
#'   \item{id}{Unique identifier for the traffic link}
#'   \item{parentTrafficLinkId}{ID of the undirected traffic link}
#'   \item{isTrafficWithMetering}{Logical indicating if traffic direction is with metering or not}
#'   \item{functionalRoadClass}{Functional road classification of the traffic link}
#'   \item{functionClass}{Function class of the traffic link}
#'   \item{highestSpeedLimit}{Highest speed limit along the traffic link}
#'   \item{lowestSpeedLimit}{Lowest speed limit along the traffic link}
#'   \item{isNorwegianScenicRoute}{Logical indicating if this is a Norwegian scenic route}
#'   \item{isFerryRoute}{Logical indicating if this is a ferry route}
#'   \item{isRamp}{Logical indicating if this is a ramp}
#'   \item{isBlocked}{Logical indicating if the traffic link is blocked}
#'   \item{tollStationIds}{IDs of toll stations on this traffic link}
#'   \item{isInvalid}{Logical indicating if the traffic link data is invalid}
#'   \item{yearAppliesTo}{Year for which this traffic link data applies}
#'   \item{startTrafficNodeId}{ID of the node at the beginning of the traffic link}
#'   \item{endTrafficNodeId}{ID of the node at the end of the traffic link}
#'   \item{municipalityIds}{IDs of municipalities containing this traffic link}
#'   \item{countyIds}{IDs of counties containing this traffic link}
#'   \item{roadSystemReferences}{Road system references}
#'   \item{roadCategory}{Category of the road, e.g., E (European), R (national), F (county), K (municipal), P (private)}
#'   \item{roadLinkIds}{IDs of road links from NVDB}
#'   \item{roadNodeIds}{IDs of road nodes from NVDB}
#'   \item{roadPlacements}{Road placement information}
#'   \item{length}{Length of the traffic link in meters}
#'   \item{maxLanes}{Maximum number of lanes along the traffic link}
#'   \item{minLanes}{Minimum number of lanes along the traffic link}
#'   \item{hasOnlyPublicTransportLanes}{Logical indicating if the link has only public transport lanes}
#'   \item{associatedTrpIds}{IDs of associated traffic registration points}
#'   \item{lastYearAadt}{AADT information from the previous year (list column)}
#'   \item{bestDataSourceAadt}{AADT information from the best available data source (list column)}
#'   \item{trafficVolumes}{List containing traffic volume measurements and metadata}
#' }
#' @docType data
#' @name buskerud_directed_traffic_links
#' @usage data(buskerud_directed_traffic_links)
#' @keywords data
NULL

#' Traffic nodes for Buskerud
#'
#' Traffic nodes for Buskerud. Raw, unprocessed data to test package functions on.
#'
#' @format A sf data frame with 730 rows and 12 variables.
#' \describe{
#'   \item{id}{Unique identifier for the traffic node}
#'   \item{isRoundabout}{Logical indicating if the node is a roundabout}
#'   \item{numberOfIncomingLinks}{Number of directed traffic links entering this node}
#'   \item{numberOfOutgoingLinks}{Number of directed traffic links leaving this node}
#'   \item{numberOfUndirectedLinks}{Number of undirected traffic links at this node}
#'   \item{legalTurningMovements}{Legal turning movements allowed at this node}
#'   \item{connectedTrafficLinkIds}{List of undirected traffic link IDs connected to this node}
#'   \item{connectedTrafficLinkCandidateIds}{List of candidate traffic link IDs that connect to this node}
#'   \item{roadNodeIds}{IDs of road nodes from NVDB}
#'   \item{roadSystemReferences}{Road system references}
#'   \item{roadSystems}{Road systems connected to this node (indicates if there are connected road systems outside of the traffic links)}
#'   \item{geometry}{Spatial geometry of the node (point)}
#' }
#' @docType data
#' @name buskerud_nodes
#' @usage data(buskerud_nodes)
#' @keywords data
NULL

#' Bus stops on traffic links
#'
#' A data frame containing all the traffic links that have only public transport lanes, with connected bus stops. Raw, unprocessed data to test package functions on.
#'
#' @format A data frame with 149 rows and 10 variables.
#' \describe{
#'   \item{id}{Unique identifier for the directed traffic link}
#'   \item{roadSystemReferences}{Road system references}
#'   \item{onlyPtLastYear}{Logical indicating if this was also only public transport last year}
#'   \item{stopPointRef}{Identifier for the stop point}
#'   \item{stopCertainty}{Certainty level of the stop location, how certain is it that the number of buses at this stop represents the number of buses on the traffic link?}
#'   \item{stopOnTrafficLink}{Logical indicating if the stop is located on the traffic link}
#'   \item{stopAggregatesDirections}{Logical indicating if the stop aggregates both directions}
#'   \item{stopsServeDifferentBuses}{Logical indicating if the stops serve different bus routes}
#'   \item{comment}{Additional comments about the bus stop}
#'   \item{trikkestopp}{Logical indicating if this is a tram stop}
#' }
#' @docType data
#' @name stops_on_traffic_links
#' @usage data(stops_on_traffic_links)
#' @keywords data
NULL

#' Bus stop counts
#'
#' A data frame containing the number of buses at each bus stop throughout all of 2025. From EnTur. Raw, unprocessed data to test package functions on.
#'
#' @format A data frame with 149 rows and 10 variables.
#' \describe{
#'   \item{stopPointRef}{Identifier for the stop point}
#'   \item{no_of_buses}{The total number of buses that passed that bus stop.}
#' }
#' @docType data
#' @name bus_counts
#' @usage data(bus_counts)
#' @keywords data
NULL
