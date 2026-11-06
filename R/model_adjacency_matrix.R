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
#' Adjacency is computed via a sparse link-by-node incidence matrix `B`, so that
#' `B %*% t(B)` counts the nodes shared by each pair of links. This avoids
#' looping over nodes.
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

  # Rebuild as a general 0/1 matrix with both triangles filled
  adj_sparse <- Matrix::sparseMatrix(
    i = shared$i,
    j = shared$j,
    x = 1,
    dims = c(n_links, n_links),
    repr = "C"
  )
  # Symmetric input gives both (i,j) and (j,i) in summary(), but if tcrossprod
  # returned a symmetric class only one triangle is listed, so add the transpose
  adj_sparse <- adj_sparse + Matrix::t(adj_sparse)
  adj_sparse@x[] <- 1

  # Exclude public transport links if requested
  if (exclude_public_transport) {
    public_transport_idx <- which(link_data$hasOnlyPublicTransportLanes)
    if (length(public_transport_idx) > 0) {
      message("Excluding ", length(public_transport_idx), " public transport links...")
      keep <- Matrix::Diagonal(
        x = as.numeric(!seq_len(n_links) %in% public_transport_idx)
      )
      adj_sparse <- Matrix::drop0(keep %*% adj_sparse %*% keep)
    }
  }

  message("Adjacency matrix complete: ", Matrix::nnzero(adj_sparse), " non-zero entries")

  return(adj_sparse)
}
