test_that("Adjacency matrix creation works", {
  link_data <- tibble::tibble(
    link_id = 1:10,
    startTrafficNodeId = c(1, 2, 3, 4, 5, 2, 3, 6, 7, 4),
    endTrafficNodeId   = c(2, 3, 4, 5, 6, 7, 8, 7, 8, 9),
    hasOnlyPublicTransportLanes = c(FALSE, FALSE, TRUE, FALSE, FALSE,
                                    FALSE, TRUE, FALSE, FALSE, FALSE),
    aadt = c(5000, 8000, 200, 6000, 4000,
             7000, 150, 5500, 6500, 4500)
  )

  adj_matrix_full <- build_adjacency_matrix(link_data, exclude_public_transport = FALSE)
  adj_matrix_excl_pt <- build_adjacency_matrix(link_data, exclude_public_transport = TRUE)
  adj_matrix_full - adj_matrix_excl_pt

})
