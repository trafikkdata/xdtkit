#' Print turning movements for a node
#'
#' Prints a human-readable summary of all legal turning movements for a given
#' node, showing incoming and outgoing traffic link IDs for each movement.
#' Useful for debugging flow balancing issues.
#'
#' @param node Character. The node ID to inspect.
#' @param nodes An sf/data.frame of nodes containing a `legalTurningMovements`
#'   column.
#' @param traffic_links A data.frame of traffic links containing an `id` column.
#'
#' @return Invisibly returns a data.frame with columns `incoming` (character)
#'   and `outgoing` (list of character vectors), one row per turning movement.
#'
#' @examples
#' \dontrun{
#' print_turning_movements("1073490", nodes_norway, traffic_links)
#' }
print_turning_movements <- function(node, nodes, traffic_links) {
  traffic_link_ids  <- traffic_links$id
  node_row          <- dplyr::filter(nodes, id == node)
  turning_movements <- node_row$legalTurningMovements

  if (is.list(turning_movements)) {
    turning_movements <- paste0("[", paste(turning_movements[[1]], collapse = ", "), "]")
  }

  results <- process_turning_movements(
    turning_movements_json = turning_movements,
    link_ids               = traffic_link_ids,
    node_id                = node
  )

  movements <- results$movements_data

  cat(sprintf("Turning movements for node %s\n", node))
  cat(strrep("-", 40), "\n")

  for (i in seq_len(nrow(movements))) {
    cat(sprintf("Movement %d\n", i))
    cat(sprintf("  Incoming: %s\n", movements$incoming[i]))
    cat(sprintf("  Outgoing:\n"))
    outgoing <- movements$outgoing[[i]]
    for (id in outgoing) {
      cat(sprintf("    - %s\n", id))
    }
    cat("\n")
  }

  invisible(movements)
}
