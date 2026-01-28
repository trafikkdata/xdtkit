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

  message("Building adjacency matrix for ", n_links, " links...")

  # Create node-to-links mapping for efficient lookup
  # For each node, find all links that start or end at that node
  start_nodes <- link_data$startTrafficNodeId
  end_nodes <- link_data$endTrafficNodeId

  # Create lists of link indices for each node
  all_nodes <- unique(c(start_nodes, end_nodes))

  # Initialize vectors to store i, j pairs for adjacency
  i_vec <- integer()
  j_vec <- integer()

  message("Finding adjacent links...")

  # For each unique node, find all links connected to it
  for (node in all_nodes) {
    # Find all links that touch this node
    links_at_node <- which(start_nodes == node | end_nodes == node)

    # If more than one link at this node, they're all adjacent to each other
    if (length(links_at_node) > 1) {
      # Create all pairs of links at this node
      pairs <- utils::combn(links_at_node, 2)
      i_vec <- c(i_vec, pairs[1, ], pairs[2, ])
      j_vec <- c(j_vec, pairs[2, ], pairs[1, ])
    }
  }

  message("Building sparse matrix from ", length(i_vec), " adjacency pairs...")

  # Build sparse matrix
  adj_sparse <- Matrix::sparseMatrix(
    i = i_vec,
    j = j_vec,
    x = 1,
    dims = c(n_links, n_links),
    symmetric = FALSE  # We'll make it symmetric below
  )

  # Make it symmetric (shouldn't be necessary given our construction, but just in case)
  adj_sparse <- adj_sparse | Matrix::t(adj_sparse)

  # Convert to numeric for consistency
  adj_sparse <- methods::as(adj_sparse, "dMatrix")

  # Exclude public transport links if requested
  if (exclude_public_transport) {
    public_transport_idx <- which(link_data$hasOnlyPublicTransportLanes)
    if (length(public_transport_idx) > 0) {
      message("Excluding ", length(public_transport_idx), " public transport links...")
      adj_sparse[public_transport_idx, ] <- 0
      adj_sparse[, public_transport_idx] <- 0
      adj_sparse <- Matrix::drop0(adj_sparse)
    }
  }

  message("Adjacency matrix complete: ", Matrix::nnzero(adj_sparse), " non-zero entries")

  return(adj_sparse)
}
