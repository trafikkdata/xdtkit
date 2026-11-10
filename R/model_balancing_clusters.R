# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create balancing clusters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Strategic network clustering for traffic flow balancing
#'
#' Partitions a road network into clusters suitable for traffic flow balancing.
#' The algorithm separates mainland from island components, uses measurement
#' points as barriers to create base clusters, merges small clusters to achieve
#' target sizes, and assigns boundary measurements to multiple adjacent clusters
#' to ensure prediction continuity.
#'
#' @param data A data frame containing directional traffic link data with columns:
#'   \describe{
#'     \item{parentTrafficLinkId}{Unique identifier for undirected traffic links}
#'     \item{startTrafficNodeId}{ID of the start node}
#'     \item{endTrafficNodeId}{ID of the end node}
#'     \item{aadt}{Traffic measurement value (NA if unmeasured)}
#'   }
#' @param year The current year, used to decide which registration points are recent enough to use ar borders.
#' @param boundary_links Which traffic_volume_source values will be used as boundary points. Default is only Traffikdata_continuous.
#' @param extra_boundary_links A vector with any additional parentTrafficLinkId's to be used as boundaries, regardless of traffic volume source and age. This can be useful if the clusters are very large without allowing these.
#' @param heavy_vehicle If TRUE, only uses data sources that have heavy vehicle counts as boundaries.
#'
#' @return A data frame with columns:
#'   \describe{
#'     \item{id}{Parent traffic link ID}
#'     \item{cluster_id}{Assigned cluster ID (1 to N)}
#'   }
#'   Note: Boundary measurement links appear multiple times (once per adjacent cluster)
#'
#' @details
#' The clustering algorithm follows these steps:
#' \enumerate{
#'   \item Build link adjacency graph where traffic links are vertices
#'   \item Identify mainland (largest component) vs island components
#'   \item Use all measurement links as barriers on mainland
#'   \item Create base clusters by removing barriers
#'   \item Merge small clusters (<100 links) with neighbors (keeping result <1000 links)
#'   \item Assign barrier links to all neighboring clusters (creates duplicates)
#'   \item Assign each island as a separate cluster
#' }
#'
#' @examples
#' \dontrun{
#' cluster_assignments <- strategic_network_clustering(data)
#' }
#' @export
strategic_network_clustering <- function(data,
                                         year,
                                         boundary_links = c("Trafikkdata_continuous"),
                                         extra_boundary_links = NULL,
                                         heavy_vehicle = FALSE) {

  undirected <- data |> dplyr::distinct(parentTrafficLinkId, .keep_all = TRUE) |>
    dplyr::select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId)

  # Find parent traffic links where both children have data
  parent_links_with_data <- data |>
    dplyr::group_by(parentTrafficLinkId) |>
    dplyr::summarise(
      child_link_has_data = all(
        traffic_volume_source %in% boundary_links &
          traffic_volume_year %in% c(year, NA) &
          coverage > 0.05 &
          if (heavy_vehicle) !is.na(heavyAadt) else TRUE
      )
    ) |>
    dplyr::mutate(
      child_link_has_data = .data$child_link_has_data |
        parentTrafficLinkId %in% extra_boundary_links
    )

  undirected <- dplyr::full_join(undirected, parent_links_with_data)

  # Step 1: Build graph where traffic links are vertices
  # Two links are connected if they share a traffic node

  n_missing_nodes <- sum(is.na(undirected$startTrafficNodeId))
  if(n_missing_nodes > 0){
    warning(paste("There are", n_missing_nodes, "traffic links that are missing nodes.\n"))
    undirected <- undirected |> tidyr::drop_na()
  }

  # Create a mapping of traffic nodes to the links that connect to them
  node_to_links <- undirected |>
    dplyr::select(parentTrafficLinkId, startTrafficNodeId, endTrafficNodeId) |>
    # Reshape to long format: each row is a (node, link) pair
    tidyr::pivot_longer(cols = c(startTrafficNodeId, endTrafficNodeId),
                        names_to = "endpoint",
                        values_to = "traffic_node") |>
    dplyr::select(traffic_node, link_id = parentTrafficLinkId) |>
    tidyr::drop_na()

  # Find pairs of links that share traffic nodes
  link_connections <- node_to_links |>
    # Self-join on traffic_node to find all links meeting at same node
    dplyr::inner_join(node_to_links, by = "traffic_node", relationship = "many-to-many") |>
    # Don't connect link to itself
    dplyr::filter(link_id.x != link_id.y) |>
    # Keep one direction only for undirected graph
    dplyr::filter(link_id.x < link_id.y) |>
    dplyr::distinct(link_id.x, link_id.y) |>
    dplyr::select(from = link_id.x, to = link_id.y)

  # Build undirected graph with links as vertices
  network_graph <- igraph::graph_from_data_frame(
    link_connections,
    vertices = data.frame(name = undirected$parentTrafficLinkId),
    directed = FALSE
  )

  # NEW: Identify mainland vs island components
  message("Identifying mainland and island components...\n")
  all_components <- igraph::components(network_graph)

  # Find the largest component (mainland)
  component_sizes <- table(all_components$membership)
  mainland_component_id <- as.integer(names(which.max(component_sizes)))

  # Separate mainland and island links
  mainland_links <- names(all_components$membership[all_components$membership == mainland_component_id])
  island_components <- all_components$membership[all_components$membership != mainland_component_id]

  # Create subgraph for mainland only
  mainland_graph <- igraph::induced_subgraph(network_graph, mainland_links)

  # Step 2: Strategic sampling of measurement points (MAINLAND ONLY)
  measurement_links <- undirected$parentTrafficLinkId[undirected$child_link_has_data == TRUE]
  mainland_measurement_links <- intersect(measurement_links, mainland_links)

  # Use all measurement links as barriers (or apply your preferred sampling strategy)
  selected_barriers <- mainland_measurement_links

  # Step 3: Create base clusters by removing barriers (MAINLAND ONLY)
  message("Creating base clusters on mainland...\n")

  # Remove barrier links from mainland network
  remaining_links <- setdiff(igraph::V(mainland_graph)$name, selected_barriers)
  cluster_graph <- igraph::induced_subgraph(mainland_graph, remaining_links)

  # Find connected components
  mainland_components <- igraph::components(cluster_graph)

  # Create base cluster assignments for mainland
  base_assignments <- data.frame(
    id = names(mainland_components$membership),
    cluster_id = mainland_components$membership,
    stringsAsFactors = FALSE
  )

  # Step 3.5: Merge small clusters with neighbors
  message("Merging small clusters...\n")

  # Neighbour lookup (integer vertex ids), built once and shared by the steps below
  adjacency <- link_adjacency(mainland_graph)

  base_assignments <- merge_small_clusters(
    base_assignments,
    mainland_graph,  # Use original graph to find neighbors
    selected_barriers,
    min_size = 100,
    max_size = 1000,
    adjacency = adjacency
  )

  # Step 4: Assign barrier links to neighboring clusters (MAINLAND ONLY)
  message("Assigning barrier links to neighboring mainland clusters...\n")

  barrier_assignments <- assign_barriers_to_clusters(
    mainland_graph,
    selected_barriers,
    base_assignments,
    adjacency = adjacency
  )

  # Step 5: Handle non-barrier measurement links (MAINLAND ONLY)
  non_barrier_measurements <- setdiff(mainland_measurement_links, selected_barriers)
  measurement_assignments <- assign_measurements_to_clusters(
    base_assignments,
    non_barrier_measurements
  )

  # Step 6: Combine mainland assignments
  mainland_assignments <- rbind(
    base_assignments,
    barrier_assignments,
    measurement_assignments
  )

  # NEW: Handle island components - each island becomes one cluster
  message("Assigning island components...\n")

  max_mainland_cluster <- max(mainland_assignments$cluster_id)

  # Create unique cluster ID for each island component
  island_cluster_map <- data.frame(
    original_component = unique(island_components),
    cluster_id = (max_mainland_cluster + 1):(max_mainland_cluster + length(unique(island_components)))
  )

  island_assignments <- data.frame(
    id = names(island_components),
    original_component = as.integer(island_components),
    stringsAsFactors = FALSE
  ) |>
    dplyr::left_join(island_cluster_map, by = "original_component") |>
    dplyr::select(id, cluster_id)

  # Step 7: Combine mainland and island assignments
  all_assignments <- rbind(
    mainland_assignments,
    island_assignments
  )

  # Handle any unassigned links
  unassigned_links <- setdiff(undirected$parentTrafficLinkId, all_assignments$id)
  if(length(unassigned_links) > 0) {

    max_cluster <- max(all_assignments$cluster_id)
    singleton_assignments <- data.frame(
      id = unassigned_links,
      cluster_id = (max_cluster + 1):(max_cluster + length(unassigned_links)),
      stringsAsFactors = FALSE
    )

    all_assignments <- rbind(all_assignments, singleton_assignments)
  }

  # Summary
  cluster_sizes <- table(all_assignments$cluster_id)
  duplicate_count <- sum(duplicated(all_assignments$id))
  final_mainland_clusters <- length(unique(mainland_assignments$cluster_id))

  message("\n=== Clustering Summary ===\n")

  # 1. Network Overview
  message("Network Overview:")
  message(paste("  Total links:", nrow(undirected)))
  message(paste("  Mainland:", length(mainland_links), "links"))
  message(paste("  Islands:", length(unique(island_components)), "components,",
                length(island_components), "links\n"))

  # 2. Clustering Results
  message("Clustering Results:")
  message(paste("  Initial mainland clusters:", mainland_components$no))
  message(paste("  After merging:", final_mainland_clusters, "mainland clusters"))
  message(paste("  Total final clusters:", length(cluster_sizes),
                "(", final_mainland_clusters, "mainland +",
                length(unique(island_components)), "islands +",
                length(unassigned_links), "singletons)\n"))

  # 3. Boundary Handling
  message("Boundary Handling:")
  message(paste("  Duplicate assignments (boundaries):", duplicate_count, "\n"))

  # 4. Cluster Size Distribution
  message("Cluster Size Distribution:")
  print(summary(as.numeric(cluster_sizes)))

  return(all_assignments)
}

#' Assign barrier links to all neighboring clusters
#'
#' For each barrier link (measurement point used to separate clusters), identifies
#' which clusters it borders and assigns it to all of them. This creates duplicate
#' entries for boundary links, ensuring flow continuity during Bayesian balancing.
#'
#' @param network_graph An igraph object representing the link adjacency network
#' @param barriers Character vector of barrier link IDs
#' @param base_assignments Data frame with columns \code{id} and \code{cluster_id}
#'   containing base cluster assignments (before boundaries added)
#' @param adjacency Optional neighbor lookup from \code{link_adjacency()}. Built from
#'   \code{network_graph} if \code{NULL}; pass it in to avoid rebuilding it.
#'
#' @return Data frame with columns:
#'   \describe{
#'     \item{id}{Barrier link ID}
#'     \item{cluster_id}{Cluster ID (one row per adjacent cluster)}
#'   }
#'   Each barrier appears multiple times if it borders multiple clusters.
#'
#' @details
#' For each barrier link:
#' \enumerate{
#'   \item Find its neighbors in the network
#'   \item Determine which clusters those neighbors belong to
#'   \item Create one assignment row per unique neighboring cluster
#' }
#'
#' This ensures measurement values are available to all clusters they border,
#' providing continuity constraints during flow balancing.
#'
#' @examples
#' \dontrun{
#' barrier_assignments <- assign_barriers_to_clusters(
#'   mainland_graph,
#'   selected_barriers,
#'   base_assignments
#' )
#' }
#' @seealso \code{\link{strategic_network_clustering}}, \code{\link{merge_small_clusters}}
#' @export
assign_barriers_to_clusters <- function(network_graph, barriers, base_assignments,
                                        adjacency = NULL) {

  link_names <- igraph::V(network_graph)$name
  if (is.null(adjacency)) adjacency <- link_adjacency(network_graph)

  # Cluster of every vertex (NA for barriers and anything not in base_assignments)
  cluster_of <- rep(NA_integer_, length(link_names))
  cluster_of[match(base_assignments$id, link_names)] <- base_assignments$cluster_id

  barrier_vid <- match(barriers, link_names)
  barrier_vid <- unique(barrier_vid[!is.na(barrier_vid)])
  neighbor_vid <- adjacency[barrier_vid]

  # One row per (barrier, neighbor), then keep each unique (barrier, cluster) pair
  barrier_assignments <- data.frame(
    id = rep(link_names[barrier_vid], lengths(neighbor_vid)),
    cluster_id = cluster_of[unlist(neighbor_vid, use.names = FALSE)],
    stringsAsFactors = FALSE
  )
  barrier_assignments <- barrier_assignments[!is.na(barrier_assignments$cluster_id), , drop = FALSE]
  barrier_assignments <- barrier_assignments[!duplicated(barrier_assignments), , drop = FALSE]
  rownames(barrier_assignments) <- NULL

  return(barrier_assignments)
}

#' Assign non-barrier measurement links to their containing cluster
#'
#' Identifies measurement links that were not used as barriers and assigns them
#' to whichever cluster they ended up in during base clustering. These links
#' appear only once (unlike barrier links which appear in multiple clusters).
#'
#' @param base_assignments Data frame with columns \code{id} and \code{cluster_id}
#'   containing base cluster assignments
#' @param measurement_links Character vector of measurement link IDs that were
#'   not selected as barriers
#'
#' @return Data frame subset of \code{base_assignments} containing only the
#'   non-barrier measurement links with columns:
#'   \describe{
#'     \item{id}{Link ID}
#'     \item{cluster_id}{Cluster ID}
#'   }
#'
#' @details
#' Non-barrier measurements are those that:
#' \itemize{
#'   \item Have traffic volume measurements
#'   \item Were not selected as cluster boundaries
#'   \item Ended up inside a cluster after barrier removal
#' }
#'
#'
#' @examples
#' \dontrun{
#' non_barrier_measurements <- setdiff(measurement_links, selected_barriers)
#' measurement_assignments <- assign_measurements_to_clusters(
#'   base_assignments,
#'   non_barrier_measurements
#' )
#' }
#' @seealso \code{\link{assign_barriers_to_clusters}}
#' @export
assign_measurements_to_clusters <- function(base_assignments, measurement_links) {

  # These are measurement links that weren't selected as barriers
  # They should be assigned to whichever cluster they ended up in
  measurement_assignments <- base_assignments[base_assignments$id %in% measurement_links, ]

  return(measurement_assignments)
}

#' Merge small clusters with neighboring clusters
#'
#' Iteratively merges clusters smaller than \code{min_size} with neighboring
#' clusters, preferring neighbors that keep the merged result under \code{max_size}.
#' Processes smallest clusters first to maximize successful merges.
#'
#' @param base_assignments Data frame with columns \code{id} and \code{cluster_id}
#'   containing initial cluster assignments
#' @param network_graph An igraph object representing the full link adjacency network
#'   (before barrier removal)
#' @param barrier_links Character vector of barrier link IDs used to separate clusters
#' @param min_size Minimum target cluster size. Clusters below this are candidates
#'   for merging. Default is 100.
#' @param max_size Maximum allowed cluster size after merging. Prevents creating
#'   clusters that are too large. Default is 1000.
#'
#' @param adjacency Optional neighbor lookup from \code{link_adjacency()}. Built from
#'   \code{network_graph} if \code{NULL}; pass it in to avoid rebuilding it.
#' @return Data frame with same structure as \code{base_assignments} but with
#'   updated cluster IDs after merging
#'
#' @details
#' The merging algorithm:
#' \enumerate{
#'   \item Identify clusters smaller than min_size
#'   \item Sort by size (smallest first)
#'   \item For each small cluster:
#'     \itemize{
#'       \item Find neighboring clusters (looking across barriers)
#'       \item Filter to neighbors where merged_size <= max_size
#'       \item Select smallest valid neighbor
#'       \item Reassign all links to target cluster
#'       \item Update size tracking for subsequent iterations
#'     }
#'   \item Report merge statistics
#' }
#'
#' @section Neighbor finding:
#' Neighbors are found by looking through barrier links in the original network.
#' This allows merging of clusters that are separated by measurement barriers
#' but would naturally belong together.
#'
#' @section Merge strategy:
#' \itemize{
#'   \item Prefers smallest valid neighbors (keeps sizes balanced)
#'   \item Cannot merge if all neighbors would exceed max_size
#'   \item Earlier merges affect later ones (updated sizes propagate)
#' }
#'
#' @examples
#' \dontrun{
#' merged_assignments <- merge_small_clusters(
#'   base_assignments,
#'   mainland_graph,
#'   selected_barriers,
#'   min_size = 100,
#'   max_size = 1000
#' )
#' }
#' @seealso \code{\link{find_neighboring_clusters}}, \code{\link{strategic_network_clustering}}
#' @export
merge_small_clusters <- function(base_assignments, network_graph, barrier_links,
                                 min_size = 100, max_size = 1000, adjacency = NULL) {

  link_names <- igraph::V(network_graph)$name
  if (is.null(adjacency)) adjacency <- link_adjacency(network_graph)
  is_barrier <- link_names %in% barrier_links

  # Work with integer vertex ids and a cluster lookup that is updated in place,
  # instead of scanning the assignments data frame for every link
  vid <- match(base_assignments$id, link_names)
  cluster_of <- rep(NA_integer_, length(link_names))
  cluster_of[vid] <- base_assignments$cluster_id

  members <- split(vid, base_assignments$cluster_id)  # vertex ids per cluster
  sizes <- lengths(members)                           # named by cluster id

  # Small clusters, smallest first (ties: lowest cluster id first)
  small <- which(sizes < min_size)
  small <- names(small[order(sizes[small])])

  for(small_id in small) {
    cluster_vids <- members[[small_id]]

    # Skip if this cluster was already merged into another
    if(is.null(cluster_vids)) next

    current_size <- length(cluster_vids)

    # Skip if cluster has grown beyond min_size due to merges
    if(current_size >= min_size) next

    # Find neighboring clusters, looking across barriers
    neighbor_clusters <- .neighbor_cluster_ids(cluster_vids, adjacency, cluster_of, is_barrier)
    if(length(neighbor_clusters) == 0) next

    # Keep neighbors where the merged size stays within max_size
    neighbor_sizes <- sizes[as.character(neighbor_clusters)]
    valid <- (neighbor_sizes + current_size) <= max_size
    if(!any(valid)) next

    # Pick the smallest valid neighbor (ties: lowest cluster id)
    candidates <- neighbor_clusters[valid]
    target_cluster <- candidates[order(neighbor_sizes[valid], candidates)][1]
    target_key <- as.character(target_cluster)

    # Merge: reassign all links from small cluster to target cluster
    cluster_of[cluster_vids] <- target_cluster
    members[[target_key]] <- c(members[[target_key]], cluster_vids)
    members[[small_id]] <- NULL
    sizes[target_key] <- sizes[target_key] + current_size
  }

  assignments <- base_assignments
  assignments$cluster_id <- cluster_of[vid]

  return(assignments)
}

#' Find neighboring clusters across barriers
#'
#' Identifies which clusters neighbor a given cluster by looking through barrier
#' links. Essential for merging clusters that are separated by measurement points
#' but should be combined.
#'
#' @param cluster_links Character vector of link IDs in the cluster of interest
#' @param network_graph An igraph object representing the full link adjacency network
#' @param base_assignments Data frame with columns \code{id} and \code{cluster_id}
#' @param barrier_links Character vector of barrier link IDs
#' @param adjacency Optional neighbor lookup from \code{link_adjacency()}. Built from
#'   \code{network_graph} if \code{NULL}; pass it in to avoid rebuilding it.
#'
#' @return Integer vector of unique neighboring cluster IDs (excluding the
#'   cluster itself)
#'
#' @details
#' For each link in the cluster:
#' \enumerate{
#'   \item Find its neighbors in the full network
#'   \item For each neighbor:
#'     \itemize{
#'       \item If neighbor is a barrier: look at barrier's neighbors to find
#'         clusters on the other side
#'       \item If neighbor is non-barrier: directly record its cluster ID
#'     }
#'   \item Return unique cluster IDs (excluding self)
#' }
#'
#' @section Why look through barriers:
#' Base clusters are disconnected by design (barriers removed). To enable merging,
#' we need to identify which clusters are adjacent in the original network, even
#' if separated by measurement barriers.
#'
#' @section Example:
#' \preformatted{
#' [Cluster A] <-> [barrier] <-> [Cluster B]
#'    (small)                      (medium)
#' }
#' Function identifies Cluster B as a neighbor of Cluster A by looking through
#' the barrier link.
#'
#' @examples
#' \dontrun{
#' cluster_links <- base_assignments$id[base_assignments$cluster_id == 1]
#' neighbors <- find_neighboring_clusters(
#'   cluster_links,
#'   network_graph,
#'   base_assignments,
#'   barrier_links
#' )
#' }
#' @seealso \code{\link{merge_small_clusters}}
#' @export
find_neighboring_clusters <- function(cluster_links, network_graph,
                                      base_assignments, barrier_links,
                                      adjacency = NULL) {

  link_names <- igraph::V(network_graph)$name
  if (is.null(adjacency)) adjacency <- link_adjacency(network_graph)

  cluster_of <- rep(NA_integer_, length(link_names))
  cluster_of[match(base_assignments$id, link_names)] <- base_assignments$cluster_id
  is_barrier <- link_names %in% barrier_links

  cluster_vids <- match(cluster_links, link_names)
  cluster_vids <- cluster_vids[!is.na(cluster_vids)]

  return(.neighbor_cluster_ids(cluster_vids, adjacency, cluster_of, is_barrier))
}

#' Build a neighbor lookup for the link adjacency graph
#'
#' @param network_graph An igraph object with links as vertices.
#' @return A list with one element per vertex (in vertex order), each an integer
#'   vector of neighboring vertex ids.
#' @noRd
link_adjacency <- function(network_graph) {
  edges <- igraph::as_edgelist(network_graph, names = FALSE)
  from <- c(edges[, 1], edges[, 2])
  to <- c(edges[, 2], edges[, 1])
  split(as.integer(to), factor(from, levels = seq_len(igraph::vcount(network_graph))))
}

#' Neighboring cluster ids across barriers, using integer vertex ids
#'
#' Workhorse behind \code{find_neighboring_clusters} and \code{merge_small_clusters}.
#'
#' @param cluster_vids Integer vertex ids of the links in the cluster.
#' @param adjacency Output of \code{link_adjacency}.
#' @param cluster_of Integer vector (one per vertex) with the current cluster id,
#'   NA for barriers.
#' @param is_barrier Logical vector (one per vertex).
#' @return Integer vector of unique neighboring cluster ids, excluding the cluster itself.
#' @noRd
.neighbor_cluster_ids <- function(cluster_vids, adjacency, cluster_of, is_barrier) {

  if(length(cluster_vids) == 0) return(integer(0))

  neighbors <- unique(unlist(adjacency[cluster_vids], use.names = FALSE))
  neighbor_is_barrier <- is_barrier[neighbors]

  # Direct non-barrier neighbors
  direct <- neighbors[!neighbor_is_barrier]

  # Non-barrier neighbors on the other side of adjacent barriers
  across <- unique(unlist(adjacency[neighbors[neighbor_is_barrier]], use.names = FALSE))
  across <- setdiff(across[!is_barrier[across]], cluster_vids)

  neighbor_clusters <- unique(cluster_of[c(direct, across)])
  neighbor_clusters <- neighbor_clusters[!is.na(neighbor_clusters)]

  neighbor_clusters[neighbor_clusters != cluster_of[cluster_vids[1]]]
}
