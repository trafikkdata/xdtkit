test_that("vector variances match the dense covariance calculation", {
  cases <- list(
    list(sd = c(5, 10, 20), duplicate = FALSE, method = "standard"),
    list(sd = c(5, 10, 20), duplicate = TRUE, method = "pseudoinverse"),
    list(sd = c(2e5, 10, 20), duplicate = FALSE, method = "standard"),
    list(sd = c(0, 10, 20), duplicate = FALSE, method = "standard"),
    list(sd = c(1e-7, 10, 20), duplicate = FALSE, method = "regularized",
         measurements_only = TRUE)
  )

  for (case in cases) {
    measurements_only <- isTRUE(case$measurements_only)
    data <- data.frame(
      id = 1:3,
      aadt = if (measurements_only) c(100, 120, 80) else c(NA, NA, 80),
      sigma_error = if (measurements_only) c(0, 2, 2) else c(NA, NA, 2)
    )
    A1 <- matrix(c(1, -1, 0), nrow = 1)
    if (case$duplicate) A1 <- rbind(A1, A1)
    if (measurements_only) A1 <- matrix(numeric(0), nrow = 0, ncol = 3)
    pred <- c(100, 120, 90)
    capture.output(
      result <- balance_group_predictions(
        data = data, nodes = NULL, pred = pred, sd = case$sd,
        constraint_matrix = A1, colname_aadt = "aadt",
        nodes_to_balance = "complete nodes"
      )
    )

    A <- rbind(A1, result$matrices$A2)
    Sigma_v <- diag(pmin(case$sd^2, 1e10))
    Sigma_vb <- Sigma_v %*% t(A)
    Sigma_epsilon <- diag(c(rep(0, nrow(A1)), na.omit(data$sigma_error)^2),
                          nrow = nrow(A))
    Sigma_b <- A %*% Sigma_v %*% t(A) + Sigma_epsilon
    Sigma_b <- (Sigma_b + t(Sigma_b)) / 2
    Sigma_b_inv <- switch(
      case$method,
      pseudoinverse = MASS::ginv(Sigma_b),
      regularized = solve(Sigma_b + 1e-10 * diag(nrow(Sigma_b))),
      standard = solve(Sigma_b)
    )
    gain <- Sigma_vb %*% Sigma_b_inv
    b <- c(rep(0, nrow(A1)), na.omit(data$aadt))
    expected_pred <- pmax(as.vector(pred + gain %*% (b - A %*% pred)), 0)
    expected_sd <- sqrt(pmax(diag(Sigma_v - gain %*% t(Sigma_vb)), 1e-6))

    expect_identical(result$diagnostics$method_used, case$method)
    expect_equal(result$diagnostics$capped_count, sum(case$sd^2 > 1e10))
    expect_equal(result$results$balanced_pred, expected_pred, tolerance = 1e-8)
    expect_equal(result$results$balanced_sd, expected_sd, tolerance = 1e-8)
  }
})

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
