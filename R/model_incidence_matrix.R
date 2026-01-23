# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build incidence matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build incidence matrix from traffic links and turning movements
#'
#' Constructs a directed incidence matrix that encodes flow conservation
#' constraints at traffic intersections. Each row represents a "flow node"
#' (a group of connected turning movements) where incoming traffic must equal
#' outgoing traffic. Uses legal turning movement data to determine which links
#' are actually connected by traffic flow, rather than just geometric adjacency.
#'
#' @param nodes Data frame containing traffic node data with column
#'   `legalTurningMovements` (JSON format) describing legal turns at each
#'   intersection, plus columns `unbalanceable_node` and `i_intersection` for
#'   filtering.
#' @param traffic_links Data frame of traffic link data with columns `id`,
#'   `startTrafficNodeId`, and `endTrafficNodeId`.
#' @param nodes_to_balance Character string specifying which nodes should have
#'   flow conservation constraints. Either "all" (balance at all nodes with no
#'   filtering), or "complete nodes" (exclude both I-intersections and nodes
#'   marked as unbalanceable).
#'
#' @return Sparse incidence matrix (A1 in the flow balancing equations) with
#'   one row per flow node and one column per traffic link. Entry (i,j) is
#'   -1 if link j flows into flow node i, +1 if it flows out, and 0 otherwise.
#'   When multiplied by traffic volumes, each row sums to zero if flow is
#'   conserved at that flow node.
#'
#' @details
#' **Flow nodes vs traffic nodes:** A single traffic node (intersection) may
#' generate multiple flow nodes. The algorithm identifies connected components
#' in the turning movement graph - each component becomes one flow node where
#' flow conservation is enforced independently.
#'
#' **Boundary nodes:** Nodes at cluster boundaries (where some turning movements
#' reference links outside the current link set) are automatically excluded from
#' flow conservation to avoid measurement artifacts.
#'
#' **I-intersections:** Simple pass-through intersections where a single link
#' enters and a single link exits are excluded when `nodes_to_balance = "complete nodes"`
#' as they provide redundant constraints.
#'
#' @export
build_incidence_matrix <- function(nodes, traffic_links, nodes_to_balance){
  if(!(nodes_to_balance %in% c("all", "complete nodes"))){
    warning("'nodes_to_balance' should be either 'all' or 'complete nodes'. With the current value, balancing will be done on every node that is not an I-intersection.")
  }

  # Filter out the nodes that appear in the traffic link data
  nodes_in_traffic_links <- unique(c(traffic_links$startTrafficNodeId,
                                     traffic_links$endTrafficNodeId))


  if(nodes_to_balance != "all"){
    balancing_nodes <- remove_incomplete_nodes(nodes, nodes_to_balance)
    relevant_nodes <- nodes_in_traffic_links[nodes_in_traffic_links %in% balancing_nodes]
  }else{
    relevant_nodes <- nodes_in_traffic_links
  }


  # Get character vector of traffic link id's
  traffic_link_ids <- traffic_links$id

  # Initialize matrix
  A1 <- matrix(numeric(0), nrow = 0, ncol = length(traffic_link_ids))
  colnames(A1) <- traffic_link_ids

  # Iterate over the traffic nodes
  # Note: This iterates over the traffic nodes, but some of the traffic nodes
  # will result in two (or more) rows in the incidence matrix.
  for(node in relevant_nodes){
    #print(node)
    # Get legal turning movements for traffic node
    node_row <- dplyr::filter(nodes, id == node)
    turning_movements <- node_row$legalTurningMovements

    # Process turning movements to get flow nodes and the corresponding row(s)
    # for the incidence matrix.
    results <- process_turning_movements(turning_movements_json = turning_movements,
                                         link_ids = traffic_link_ids,
                                         node_id = node)

    row_in_incidence_matrix <- results$constraint_rows
    A1 <- rbind(A1, row_in_incidence_matrix)
  }


  return(A1)
}


#' Filter nodes based on balancing criteria
#'
#' @param nodes Data frame of traffic nodes
#' @param nodes_to_balance Either "all" or "complete nodes"
#'
#' @return Character vector of node IDs to include in flow balancing
remove_incomplete_nodes <- function(nodes, nodes_to_balance){
  unbalanceable <- dplyr::filter(nodes, unbalanceable_node)$id
  i_intersection <- dplyr::filter(nodes, i_intersection)$id

  if(nodes_to_balance == "complete nodes"){
    # Return all nodes that balancable and are not I-intersections
    balanceable_nodes <- setdiff(nodes$id, union(unbalanceable, i_intersection))
    return(balanceable_nodes)
  }

  not_i_intersection <- setdiff(nodes$id, i_intersection)
  # Return all the nodes that are not I-intersections
  return(not_i_intersection)
}


#' Process turning movements JSON and create flow nodes
#'
#' Parses JSON turning movement data for a single traffic node and identifies
#' flow nodes (connected components in the turning movement graph). Boundary
#' nodes (with movements referencing links outside the dataset) return empty
#' constraints.
#'
#' @param turning_movements_json JSON string containing turning movement data
#' @param link_ids Character vector of all traffic link IDs in the dataset
#' @param node_id Character string identifying the traffic node
#'
#' @return List with elements: `flow_nodes` (character vector of flow node
#'   names), `constraint_rows` (matrix rows for incidence matrix),
#'   `movements_data` (parsed data frame), and optionally `is_boundary` (logical)
process_turning_movements <- function(turning_movements_json, link_ids, node_id) {
  # Parse JSON string
  if(is.na(turning_movements_json) || turning_movements_json == "" || is.null(turning_movements_json)) {
    return(list(
      flow_nodes = character(0),
      constraint_rows = matrix(nrow = 0, ncol = length(link_ids)),
      movements_data = data.frame()
    ))
  }

  # Clean and parse JSON
  movements <- jsonlite::fromJSON(turning_movements_json, simplifyVector = FALSE)

  if(length(movements) == 0) {
    return(list(
      flow_nodes = character(0),
      constraint_rows = matrix(nrow = 0, ncol = length(link_ids)),
      movements_data = data.frame()
    ))
  }

  # Convert to data frame for easier processing
  movements_df <- data.frame(
    incoming = character(length(movements)),
    outgoing = I(vector("list", length(movements))),  # Use I() to keep as list column
    stringsAsFactors = FALSE
  )

  for(i in seq_along(movements)) {
    movement <- movements[[i]]
    movements_df$incoming[i] <- movement$incomingId
    # Check if there are no outgoing traffic links
    if(is.null(unlist(movement$outgoingIds))){
      outgoing <- list(unlist(movement$outgoingIds))
      print(paste("Node", node_id,
                  "has incomplete turning movement from traffic link",
                  movement$incomingId))
    }else{
      outgoing <- unlist(movement$outgoingIds)
    }
    movements_df$outgoing[[i]] <- outgoing
  }

  # Check if this is a boundary node
  all_incoming <- unique(movements_df$incoming)
  all_outgoing <- unique(unlist(movements_df$outgoing))

  # If any links are outside the cluster, this is a boundary node
  is_boundary <- !all(all_incoming %in% link_ids) || !all(all_outgoing %in% link_ids)

  if(is_boundary) {
    # Return empty constraint for boundary nodes
    return(list(
      flow_nodes = character(0),
      constraint_rows = matrix(nrow = 0, ncol = length(link_ids)),
      movements_data = movements_df,
      is_boundary = TRUE
    ))
  }

  # Create flow nodes by grouping movements
  flow_nodes <- create_flow_nodes(movements_df = movements_df, node_id = node_id)

  # We need the total number of outgoing links for each flow node, for later comparison
  node_ids <- sapply(flow_nodes, function(x) x$name)
  outgoing_counts <- sapply(flow_nodes, function(x) length(x$outgoing_links))
  names(outgoing_counts) <- node_ids


  # Build constraint matrix rows
  n_flow <- length(flow_nodes)
  n_links <- length(link_ids)
  constraint_rows <- matrix(0, nrow = n_flow, ncol = n_links)
  colnames(constraint_rows) <- link_ids

  flow_node_names <- character(n_flow)

  for(i in seq_along(flow_nodes)) {
    #print(flow_nodes[[i]])
    flow_node <- flow_nodes[[i]]
    flow_node_names[i] <- flow_node$name

    # Set -1 for incoming links
    for(incoming_link in flow_node$incoming_links) {
      if(incoming_link %in% link_ids) {
        col_idx <- which(link_ids == incoming_link)
        constraint_rows[i, col_idx] <- -1
      }
    }

    # Set +1 for outgoing links
    for(outgoing_link in flow_node$outgoing_links) {
      if(outgoing_link %in% link_ids) {
        col_idx <- which(link_ids == outgoing_link)
        constraint_rows[i, col_idx] <- 1
      }
    }
  }

  rownames(constraint_rows) <- flow_node_names


  return(list(
    flow_nodes = flow_node_names,
    constraint_rows = constraint_rows,
    movements_data = movements_df
  ))
}


#' Create flow nodes from turning movements using graph connectivity
#'
#' Identifies connected components in the bipartite turning movement graph.
#' Each component represents a group of turning movements where flow should be
#' conserved independently.
#'
#' @param movements_df Data frame with columns `incoming` and `outgoing` (list column)
#' @param node_id Character string for the traffic node ID
#'
#' @return List of flow node objects, each containing `name`, `incoming_links`,
#'   `outgoing_links`, `parent_node`, and `flow_type`
create_flow_nodes <- function(movements_df, node_id) {

  if(nrow(movements_df) == 0) {
    return(list())
  }

  # Get all unique incoming and outgoing links
  all_incoming <- unique(movements_df$incoming)
  all_outgoing <- unique(unlist(movements_df$outgoing))

  # Create bipartite adjacency representation
  # We'll use a simple approach: create edges and find connected components
  edges <- data.frame(
    from = character(0),
    to = character(0),
    stringsAsFactors = FALSE
  )

  # Add edges for each turning movement
  for(i in seq_len(nrow(movements_df))) {
    incoming_link <- movements_df$incoming[i]
    outgoing_links <- movements_df$outgoing[[i]]

    if(!is.null(unlist(outgoing_links))){ # Skip over incoming links that have no outgoing links
      for(outgoing_link in outgoing_links) {
        edges <- rbind(edges, data.frame(
          from = incoming_link,
          to = outgoing_link,
          stringsAsFactors = FALSE
        ))
      }
    }
  }

  # Find connected components using union-find approach
  components <- find_connected_components(edges, all_incoming, all_outgoing)

  # Create flow nodes from components
  flow_nodes <- list()

  for(i in seq_along(components)) {
    component <- components[[i]]

    # Separate incoming and outgoing links in this component
    component_incoming <- intersect(component, all_incoming)
    component_outgoing <- intersect(component, all_outgoing)

    # Determine flow type for naming
    flow_type <- determine_flow_type(component_incoming, component_outgoing)

    flow_node <- list(
      name = paste0(node_id, "_component_", i, "_", flow_type),
      incoming_links = component_incoming,
      outgoing_links = component_outgoing,
      parent_node = node_id,
      flow_type = flow_type
    )

    flow_nodes[[i]] <- flow_node
  }

  return(flow_nodes)
}


#' Find connected components using union-find algorithm
#'
#' @param edges Data frame with `from` and `to` columns representing graph edges
#' @param all_incoming Character vector of incoming link IDs
#' @param all_outgoing Character vector of outgoing link IDs
#'
#' @return List of character vectors, each containing node IDs in one component
find_connected_components <- function(edges, all_incoming, all_outgoing) {

  if(nrow(edges) == 0) {
    return(list())
  }

  # All nodes in the bipartite graph
  all_nodes <- unique(c(all_incoming, all_outgoing))

  # Initialize union-find structure
  parent <- stats::setNames(all_nodes, all_nodes)  # Each node is its own parent initially

  # Union-find helper functions
  find_root <- function(node) {
    if(parent[node] != node) {
      parent[node] <<- find_root(parent[node])  # Path compression
    }
    return(parent[node])
  }

  union_nodes <- function(node1, node2) {
    root1 <- find_root(node1)
    root2 <- find_root(node2)
    if(root1 != root2) {
      parent[root2] <<- root1
    }
  }

  # Process all edges to build connected components
  for(i in seq_len(nrow(edges))) {
    union_nodes(edges$from[i], edges$to[i])
  }

  # Group nodes by their root
  components_map <- list()
  for(node in all_nodes) {
    root <- find_root(node)
    if(is.null(components_map[[root]])) {
      components_map[[root]] <- character(0)
    }
    components_map[[root]] <- c(components_map[[root]], node)
  }

  # Convert to list format
  components <- unname(components_map)

  # Filter out empty components
  components[lengths(components) > 0]
}


#' Determine flow type from link counts
#'
#' @param incoming_links Character vector of incoming link IDs
#' @param outgoing_links Character vector of outgoing link IDs
#'
#' @return Character string: "passthrough", "merge", "split", "mixing", or "unknown"
determine_flow_type <- function(incoming_links, outgoing_links) {

  n_in <- length(incoming_links)
  n_out <- length(outgoing_links)

  if(n_in == 1 && n_out == 1) {
    return("passthrough")
  } else if(n_in > 1 && n_out == 1) {
    return("merge")
  } else if(n_in == 1 && n_out > 1) {
    return("split")
  } else if(n_in > 1 && n_out > 1) {
    return("mixing")
  } else {
    return("unknown")
  }
}
