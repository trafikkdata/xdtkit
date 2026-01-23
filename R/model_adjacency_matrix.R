# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build adjacency matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build adjacency matrix for traffic network
#'
#' Constructs a sparse adjacency matrix representing which traffic links are
#' geometrically connected (share a traffic node). Two links are considered
#' adjacent if they share at least one traffic node (either start or end node).
#' The matrix is symmetric and can optionally exclude public transport links.
#'
#' @param link_data Data frame with columns `startTrafficNodeId` and
#'   `endTrafficNodeId` identifying the endpoints of each directed traffic link.
#'   If `exclude_public_transport = TRUE`, must also contain column
#'   `hasOnlyPublicTransportLanes`.
#' @param exclude_public_transport Logical; if `TRUE`, sets all adjacencies
#'   involving public transport-only links to zero. Default `FALSE`. Public
#'   transport links often have very different traffic volumes than neighboring
#'   links, which can affect spatial smoothing in statistical models.
#'
#' @returns A sparse symmetric adjacency matrix (dgCMatrix) with dimensions
#'   n x n where n is the number of traffic links. Entry (i,j) is 1 if links
#'   i and j share a traffic node, 0 otherwise. The diagonal is always 0
#'   (links are not adjacent to themselves).
#'
#' @details
#' The function builds adjacency based on geometric connectivity only - it does
#' not consider whether traffic can legally flow between links. For flow
#' conservation constraints, use turning movement data instead.
#'
#' When `exclude_public_transport = TRUE`, rows and columns corresponding to
#' public transport links are zeroed out, effectively isolating them from the
#' adjacency structure. This is useful for spatial models where you want to
#' prevent public transport links from influencing predictions on regular roads.
#'
#' @export
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
  adj_sparse <- methods::as(adj_sparse, "dMatrix")


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


