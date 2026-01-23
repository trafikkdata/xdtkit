# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Identify unbalanceable nodes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Identify unbalanceable nodes
#'
#' Identifies nodes in the traffic network that cannot be balanced due to incomplete
#' information or structural issues. Flags both I-intersections (where road systems
#' cross without merging) and incomplete nodes (where candidate links exceed actual
#' traffic links).
#'
#' @param nodes A spatial data frame (sf object) containing traffic nodes with columns:
#'   id, connectedTrafficLinkIds, roadSystems, numberOfIncomingLinks, numberOfOutgoingLinks,
#'   numberOfUndirectedLinks, and connectedTrafficLinkCandidateIds
#' @param directed_traffic_links A data frame of directed traffic links containing
#'   parentTrafficLinkId and roadSystem columns
#'
#' @return A spatial data frame with additional columns:
#'   \itemize{
#'     \item n_roadSystems: Number of road systems at the node
#'     \item trafficLinkRoadSystems: List of unique road systems from connected traffic links
#'     \item n_trafficLinkRoadSystems: Number of unique road systems from traffic links
#'     \item number_of_traffic_links: Count of connected traffic links
#'     \item number_of_candidate_links: Count of candidate traffic links
#'     \item i_intersection: Logical flag for I-intersections (2 in, 2 out, 2 undirected,
#'       with more road systems than traffic link systems)
#'     \item unbalanceable_node: Logical flag for nodes with incomplete information
#'   }
#' @export
identify_unbalanceable_nodes <- function(nodes, directed_traffic_links){
  data <- directed_traffic_links
  nodes_raw <- nodes

  # Unnest to get one row per node-traffic link combination
  node_traffic_links <- nodes_raw |> sf::st_drop_geometry() |>
    dplyr::select(id, connectedTrafficLinkIds, roadSystems) |>
    tidyr::unnest(connectedTrafficLinkIds)

  # Join with data to get road systems from traffic links
  node_covered_systems <- node_traffic_links |>
    dplyr::left_join(data |> dplyr::select(parentTrafficLinkId, roadSystem) |>
                       dplyr::distinct(),
                     by = c("connectedTrafficLinkIds" = "parentTrafficLinkId")) |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      roadSystems = list(dplyr::first(roadSystems)),
      n_roadSystems = length(dplyr::first(roadSystems)),
      trafficLinkRoadSystems = list(unique(roadSystem)),
      n_trafficLinkRoadSystems = length(unique(roadSystem))
    )

  nodes <- nodes_raw |> dplyr::left_join(node_covered_systems)

  nodes <- nodes |>
    dplyr::mutate(
      number_of_traffic_links = lengths(connectedTrafficLinkIds),
      number_of_candidate_links = lengths(connectedTrafficLinkCandidateIds),
      # I intersections
      i_intersection = numberOfIncomingLinks == 2 &
        numberOfOutgoingLinks == 2 &
        numberOfUndirectedLinks == 2 &
        n_roadSystems > n_trafficLinkRoadSystems,
      # Incomplete nodes
      unbalanceable_node = number_of_candidate_links > number_of_traffic_links)

  return(nodes)
}
