test_that("independent and duplicate constraints give the same balanced results", {
  data <- data.frame(
    id = 1:3,
    aadt = c(NA, NA, 80),
    sigma_error = c(NA, NA, 2)
  )
  constraint <- matrix(c(1, -1, 0), nrow = 1)

  for (duplicate_constraint in c(FALSE, TRUE)) {
    A1 <- if (duplicate_constraint) rbind(constraint, constraint) else constraint
    capture.output(
      result <- balance_group_predictions(
        data = data,
        nodes = NULL,
        pred = c(100, 120, 90),
        sd = c(10, 10, 10),
        constraint_matrix = A1,
        colname_aadt = "aadt",
        nodes_to_balance = "complete nodes"
      )
    )

    expect_equal(result$results$balanced_pred, c(110, 110, 80 + 10 / 26),
                 tolerance = 1e-8)
    expect_equal(result$results$balanced_sd, sqrt(c(50, 50, 100 / 26)),
                 tolerance = 1e-8)
    expect_identical(result$diagnostics$rank_deficit, as.integer(duplicate_constraint))
    expect_identical(
      result$diagnostics$method_used,
      if (duplicate_constraint) "pseudoinverse" else "standard"
    )
    expect_identical(result$matrices$A1, A1)
    A <- rbind(A1, result$matrices$A2)
    expect_identical(result$diagnostics$rank_deficit, nrow(A) - qr(A)$rank)
    expect_identical(result$diagnostics$n_links, 3L)
    expect_identical(result$diagnostics$n_constraints, nrow(A1))
    expect_identical(result$diagnostics$n_measurements, 1L)
  }
})

test_that("measurements can make an independent flow constraint redundant", {
  A1 <- matrix(c(1, -1, 0), nrow = 1)
  capture.output(
    result <- balance_group_predictions(
      data = data.frame(
        id = 1:3,
        aadt = c(100, 100, NA),
        sigma_error = c(2, 2, NA)
      ),
      nodes = NULL,
      pred = c(100, 120, 90),
      sd = c(10, 10, 10),
      constraint_matrix = A1,
      colname_aadt = "aadt",
      nodes_to_balance = "complete nodes"
    )
  )

  A2 <- result$matrices$A2
  A <- rbind(A1, A2)
  expect_identical(qr(A1)$rank, nrow(A1))
  expect_identical(qr(A2)$rank, nrow(A2))
  expect_identical(result$diagnostics$rank_deficit, nrow(A) - qr(A)$rank)
  expect_identical(result$diagnostics$rank_deficit, 1L)
  expect_identical(result$diagnostics$method_used, "pseudoinverse")
  expect_equal(result$results$balanced_pred,
               c(100 + 10 / 26, 100 + 10 / 26, 90), tolerance = 1e-8)
  expect_equal(result$results$balanced_sd, sqrt(c(100 / 52, 100 / 52, 100)),
               tolerance = 1e-8)
})

test_that("groups without measurements or constraints preserve predictions", {
  capture.output(
    result <- balance_group_predictions(
      data = data.frame(id = 1:2, aadt = c(NA_real_, NA_real_)),
      nodes = NULL,
      pred = c(100, 120),
      sd = c(10, 20),
      constraint_matrix = matrix(numeric(0), nrow = 0, ncol = 2),
      colname_aadt = "aadt",
      nodes_to_balance = "complete nodes"
    )
  )

  expect_equal(result$results$balanced_pred, c(100, 120))
  expect_equal(result$results$balanced_sd, c(10, 20))
  expect_true(is.na(result$diagnostics$method_used))
  expect_true(is.na(result$diagnostics$rank_deficit))
})

test_that("nearly duplicate flow constraints remain independent at 1e-6", {
  for (epsilon in c(1e-4, 1e-6)) {
    A1 <- rbind(c(1, 0, 0), c(1, epsilon, 0))
    capture.output(
      result <- balance_group_predictions(
        data = data.frame(id = 1:3, aadt = rep(NA_real_, 3)),
        nodes = NULL,
        pred = rep(0, 3),
        sd = rep(1, 3),
        constraint_matrix = A1,
        colname_aadt = "aadt",
        nodes_to_balance = "complete nodes"
      )
    )

    expect_identical(result$diagnostics$rank_deficit, nrow(A1) - qr(A1)$rank)
    expect_identical(result$diagnostics$rank_deficit, 0L)
    expect_equal(result$results$balanced_pred, rep(0, 3))
  }
})

test_that("near-dependent constraints document the Cholesky tolerance boundary", {
  # Squaring the system loses the 1e-8 separation; QR still detects it.
  A1 <- rbind(c(1, 0, 0), c(1, 1e-8, 0))
  capture.output(
    result <- balance_group_predictions(
      data = data.frame(id = 1:3, aadt = rep(NA_real_, 3)),
      nodes = NULL,
      pred = rep(0, 3),
      sd = rep(1, 3),
      constraint_matrix = A1,
      colname_aadt = "aadt",
      nodes_to_balance = "complete nodes"
    )
  )

  expect_identical(qr(A1)$rank, 2L)
  expect_identical(result$diagnostics$rank_deficit, 1L)
  expect_identical(result$diagnostics$method_used, "pseudoinverse")
})
