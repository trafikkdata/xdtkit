# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build adjacency matrix ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Build adjacency matrix for traffic network
#'
#' Constructs a sparse adjacency matrix representing which traffic links are
#' geometrically connected (share a traffic node). Two links are considered
#' adjacent if they share at least one traffic node (either start or end node).
#' The matrix is symmetric. Public transport-only links are never connected to
#' regular links, since their traffic volumes are typically very different from
#' those of neighboring links, which can affect spatial smoothing in
#' statistical models.
#'
#' @param link_data Data frame with columns `startTrafficNodeId`,
#'   `endTrafficNodeId` and `hasOnlyPublicTransportLanes`. The first two
#'   identify the endpoints of each directed traffic link. Missing values in
#'   `hasOnlyPublicTransportLanes` are treated as `FALSE`.
#' @param exclude_public_transport Logical. Controls how public transport-only
#'   links are handled:
#'   * `FALSE` (default): public transport links stay in the graph and are
#'     connected to each other when they share a node, but all connections
#'     between public transport links and regular links are removed.
#'   * `TRUE`: public transport links are left out entirely, so their rows and
#'     columns are zero.
#'
#' @returns A sparse symmetric adjacency matrix (dgCMatrix) with dimensions
#'   n x n where n is the number of traffic links. Entry (i,j) is 1 if links
#'   i and j share a traffic node (and are allowed to be connected, see above),
#'   0 otherwise. The diagonal is always 0 (links are not adjacent to
#'   themselves).
#'
#' @details
#' The function builds adjacency based on geometric connectivity only - it does
#' not consider whether traffic can legally flow between links. For flow
#' conservation constraints, use turning movement data instead.
#'
#' Adjacency is computed via a sparse link-by-node incidence matrix `B`, so that
#' `B %*% t(B)` counts the nodes shared by each pair of links. This avoids
#' looping over nodes.
#'
#' @export
build_adjacency_matrix <- function(link_data, exclude_public_transport = FALSE) {
  n_links <- nrow(link_data)

  message("Building adjacency matrix for ", n_links, " links...")

  # Link-by-node incidence matrix
  # One entry per (link, node) pair; unique() so that links with identical
  # start and end nodes are only counted once
  link_idx <- rep(seq_len(n_links), times = 2)
  node_id <- c(link_data$startTrafficNodeId, link_data$endTrafficNodeId)

  incidence <- unique(data.frame(link = link_idx, node = node_id))
  incidence <- incidence[!is.na(incidence$node), ]
  node_idx <- match(incidence$node, unique(incidence$node))

  B <- Matrix::sparseMatrix(
    i = incidence$link,
    j = node_idx,
    x = 1,
    dims = c(n_links, max(node_idx))
  )

  # Adjacency: B %*% t(B) has entry (i, j) = number of nodes shared by i and j
  message("Finding adjacent links...")
  shared <- Matrix::summary(Matrix::tcrossprod(B))
  shared <- shared[shared$i != shared$j, ]  # drop diagonal

  # Public transport handling (done on the pair list, before building the matrix)
  is_pt <- link_data$hasOnlyPublicTransportLanes %in% TRUE
  n_pt <- sum(is_pt)

  if (n_pt > 0) {
    if (exclude_public_transport) {
      message("Excluding ", n_pt, " public transport links...")
      # Option 1: public transport links are left out entirely
      keep <- !is_pt[shared$i] & !is_pt[shared$j]
    } else {
      message("Separating ", n_pt, " public transport links from regular links...")
      # Option 2: only keep pairs where both links are public transport,
      # or both are regular
      keep <- is_pt[shared$i] == is_pt[shared$j]
    }
    shared <- shared[keep, ]
  }

  # Build the 0/1 matrix with both triangles filled
  adj_sparse <- Matrix::sparseMatrix(
    i = shared$i,
    j = shared$j,
    x = 1,
    dims = c(n_links, n_links),
    repr = "C"
  )
  # summary() may list only one triangle for a symmetric result, so add the
  # transpose and reset values to 1
  adj_sparse <- adj_sparse + Matrix::t(adj_sparse)
  adj_sparse@x[] <- 1

  message("Adjacency matrix complete: ", Matrix::nnzero(adj_sparse), " non-zero entries")

  return(adj_sparse)
}
