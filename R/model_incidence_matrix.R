# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build incidence matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Build incidence matrix from traffic links and constructed flow nodes
#'
#' @param nodes data frame containing legalTurningMovements column (which has JSON syntax)
#' @param traffic_links traffic link data set
#'
#' @return incidence matrix
#'
build_incidence_matrix <- function(nodes, traffic_links, nodes_to_balance){
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

  # Remove first row since this is just NA from initialization
  #A1 <- A1[-1, , drop = FALSE]

  return(A1)
}


remove_incomplete_nodes <- function(nodes, nodes_to_balance){
  unbalanceable <- dplyr::filter(nodes, unbalanceable_node)$id
  i_intersection <- dplyr::filter(nodes, i_intersection)$id

  if(nodes_to_balance == "complete_nodes"){
    # Return all nodes that balancable and are not I-intersections
    balanceable_nodes <- setdiff(nodes$id, union(unbalanceable, i_intersection))
    return(balanceable_nodes)
  }

  not_i_intersection <- setdiff(nodes$id, i_intersection)
  # Return all the nodes that are not I-intersections
  return(not_i_intersection)
}


#' Build flow constraint matrix
#'
#' This is old and balances at traffic nodes, not flow nodes!!!
#'
#' @param link_data Data frame with columns "startTrafficNodeId" and "endTrafficNodeId".
#'
#' @returns A matrix with rows equal to the number of nodes (with incoming and outgoing traffic), and columns equal to the number of traffic links. In a given row, the links corresponding to incoming traffic get value 1, and the links corresponding to outgoing traffic get value -1.
#' @export
#'
build_flow_constraints <- function(link_data) {
  n_links <- nrow(link_data)
  constraints <- list()
  node_names <- character()  # Track which node each row represents

  # Get all unique nodes in the network
  all_nodes <- unique(c(link_data$startTrafficNodeId, link_data$endTrafficNodeId))

  for(node_id in all_nodes) {
    # Find links that END at this node (incoming traffic)
    incoming_links <- which(link_data$endTrafficNodeId == node_id)

    # Find links that START from this node (outgoing traffic)
    outgoing_links <- which(link_data$startTrafficNodeId == node_id)

    # Only create constraint if we have both incoming and outgoing traffic
    # (otherwise it's a network boundary node)
    if(length(incoming_links) > 0 & length(outgoing_links) > 0) {
      constraint_row <- rep(0, n_links)
      constraint_row[incoming_links] <- 1    # incoming = +1
      constraint_row[outgoing_links] <- -1   # outgoing = -1
      constraints <- append(constraints, list(constraint_row))
      node_names <- c(node_names, as.character(node_id))  # Store the node ID
    }
  }

  # Convert to matrix and add rownames and colnames
  if(length(constraints) > 0) {
    result_matrix <- do.call(rbind, constraints)
    rownames(result_matrix) <- node_names
    colnames(result_matrix) <- as.character(link_data$id)  # Add link IDs as column names
    return(result_matrix)
  } else {
    return(NULL)
  }
}

#' Process turning movements JSON string and create flow nodes
#'
#' @param turning_movements_json character string containing JSON data
#' @param link_ids character vector of all possible link IDs
#' @param node_id character string identifying the traffic node
#'
#' @return list containing flow node constraints and metadata
#'
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


  # num_outgoing_rowsums <- rowSums(as.matrix(constraint_rows) == 1)
  # common_nodes <- intersect(names(outgoing_counts), names(num_outgoing_rowsums))
  # nodes_to_exclude <- common_nodes[
  #   outgoing_counts[common_nodes] != num_outgoing_rowsums[common_nodes]]
  #
  # constraint_rows <- constraint_rows[!rownames(constraint_rows) %in% nodes_to_exclude, , drop = FALSE]
  # if(nrow(constraint_rows) == 0) {
  #   return(list(
  #     flow_nodes = character(0),
  #     constraint_rows = matrix(nrow = 0, ncol = length(link_ids)),
  #     movements_data = data.frame()
  #   ))
  # }

  return(list(
    flow_nodes = flow_node_names,
    constraint_rows = constraint_rows,
    movements_data = movements_df
  ))
}


#' Create flow nodes from turning movements using graph connectivity
#'
#' Strategy:
#' 1. Create a bipartite graph of incoming -> outgoing connections
#' 2. Find connected components in this graph
#' 3. Each connected component becomes one flow node
#'
#' @param movements_df data frame with incoming and outgoing columns
#' @param node_id character string for the traffic node
#'
#' @return list of flow node objects
#'
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


#' Find connected components in bipartite graph using union-find
#'
#' @param edges data frame with from/to columns representing graph edges
#' @param all_incoming character vector of incoming link IDs
#' @param all_outgoing character vector of outgoing link IDs
#'
#' @return list of character vectors, each representing a connected component
#'
find_connected_components <- function(edges, all_incoming, all_outgoing) {

  if(nrow(edges) == 0) {
    return(list())
  }

  # All nodes in the bipartite graph
  all_nodes <- unique(c(all_incoming, all_outgoing))

  # Initialize union-find structure
  parent <- setNames(all_nodes, all_nodes)  # Each node is its own parent initially

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


#' Determine the flow type based on incoming/outgoing link counts
#'
#' @param incoming_links character vector of incoming links
#' @param outgoing_links character vector of outgoing links
#'
#' @return character string describing flow type
#'
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
