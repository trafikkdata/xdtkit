# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build adjacency matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build adjacency matrix
#'
#' @param link_data Data frame with columns "startTrafficNodeId" and "endTrafficNodeId".
#'
#' @returns Adjacency matrix. The number of rows and columns are both equal to the number of traffic links. An entry is 1 if the correspondng traffic links are connected, 0 otherwise.
#' @export
#'
build_adjacency_matrix <- function(link_data, exclude_public_transport = FALSE) {
  n_links <- nrow(link_data)

  # Create all possible pairs and check for shared nodes
  link_pairs <- expand.grid(i = 1:n_links, j = 1:n_links)
  link_pairs <- link_pairs[link_pairs$i != link_pairs$j, ]  # remove self-pairs

  # Check if pairs share nodes
  shared_node <- (
    (link_data$startTrafficNodeId[link_pairs$i] == link_data$startTrafficNodeId[link_pairs$j]) |
      (link_data$startTrafficNodeId[link_pairs$i] == link_data$endTrafficNodeId[link_pairs$j]) |
      (link_data$endTrafficNodeId[link_pairs$i] == link_data$startTrafficNodeId[link_pairs$j]) |
      (link_data$endTrafficNodeId[link_pairs$i] == link_data$endTrafficNodeId[link_pairs$j])
  )

  # Build sparse matrix
  adj_sparse <- Matrix::sparseMatrix(
    i = link_pairs$i[shared_node],
    j = link_pairs$j[shared_node],
    x = 1,
    dims = c(n_links, n_links)
  )

  # Make it symmetric manually
  adj_sparse <- adj_sparse | Matrix::t(adj_sparse)

  # Convert to numeric at the end for consistency
  adj_sparse <- as(adj_sparse, "dMatrix")


  # Exclude public transport links if requested
  if (exclude_public_transport) {
    public_transport_idx <- which(link_data$hasOnlyPublicTransportLanes)
    if (length(public_transport_idx) > 0) {
      # Convert to numeric matrix temporarily to avoid logical coercion warning
      adj_sparse[public_transport_idx, ] <- 0
      adj_sparse[, public_transport_idx] <- 0
      # Drop explicit zeros to keep it sparse
      adj_sparse <- Matrix::drop0(adj_sparse)
    }
  }

  return(adj_sparse)
}


