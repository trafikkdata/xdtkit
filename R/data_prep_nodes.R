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

  link_ids <- nodes$connectedTrafficLinkIds
  n_nodes <- length(link_ids)

  # Unique road systems per traffic link (looked up once)
  link_systems <- directed_traffic_links |>
    sf::st_drop_geometry() |>
    dplyr::distinct(link = parentTrafficLinkId, roadSystem)

  # One row per node-traffic link combination
  node_links <- data.frame(
    node = rep.int(seq_len(n_nodes), lengths(link_ids)),
    link = unlist(link_ids, use.names = FALSE)
  )

  # Unique road systems per node (NA for links missing from the data)
  node_systems <- node_links |>
    dplyr::left_join(link_systems, by = "link",
                     relationship = "many-to-many") |>
    dplyr::distinct(node, roadSystem)

  traffic_link_road_systems <- split(
    node_systems$roadSystem,
    factor(node_systems$node, levels = seq_len(n_nodes))
  )
  names(traffic_link_road_systems) <- NULL
  n_traffic_link_road_systems <- lengths(traffic_link_road_systems)

  # Attach results (no join needed, row order is preserved)
  nodes$n_roadSystems <- lengths(nodes$roadSystems)
  nodes$trafficLinkRoadSystems <- traffic_link_road_systems
  nodes$n_trafficLinkRoadSystems <- n_traffic_link_road_systems

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
      unbalanceable_node = number_of_candidate_links > number_of_traffic_links
    )

  return(nodes)
}
