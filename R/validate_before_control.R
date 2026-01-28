# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-validation functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Calculate pre-approval metrics using previous year AADT
#'
#' Validates model predictions against previous year's AADT values before manual
#' control. This provides an early check that predictions are reasonable and helps
#' identify segments that may need closer review.
#'
#' @param model An INLA model object with summary.fitted.values. If NULL, must
#'   provide pred and sd, or data must contain prediction columns.
#' @param pred Numeric vector of predictions. Used if model is NULL.
#' @param sd Numeric vector of standard deviations. Used if model is NULL.
#' @param data Data frame containing directed traffic link predictions with
#'   columns parentTrafficLinkId, and optionally pred/sd or balanced_pred/balanced_sd.
#' @param previous_aadt_col Character. Name of column in data containing
#'   previous year's AADT values. Default is "lastYearAadt_aadt".
#' @param model_name Character. Name to identify the model in output. Default
#'   is "inla".
#'
#' @return List with three elements:
#'   \itemize{
#'     \item summary: Data frame with median_eale and other summary statistics
#'     \item directed_data: Original data with added predictions
#'     \item undirected_data: Aggregated undirected segments with eALE calculations
#'   }
#'
#' @details
#' This function calculates exponential Absolute Log Error (eALE) between model
#' predictions and previous year's AADT. While predictions should ultimately match
#' the current year's true values, comparison with previous year provides a useful
#' sanity check since AADT typically doesn't change drastically year-over-year.
#'
#' Large eALE values may indicate:
#' \itemize{
#'   \item Model predictions that need review
#'   \item Segments where traffic has genuinely changed significantly
#'   \item Data quality issues in either current predictions or previous AADT
#' }
#'
#' @seealso \code{\link{calculate_approval_metrics}} for validation against
#'   official (undirected) corrected values.
#'
#' @examples
#' \dontrun{
#' pre_validation <- calculate_preapproval_metrics(
#'   model = inla_model,
#'   data = directed_predictions
#' )
#'
#' # View median eALE
#' pre_validation$summary$median_eale
#'
#' # Identify segments with large changes
#' large_changes <- pre_validation$undirected_data |>
#'   filter(eale > 0.5)
#' }
#'
#' @export
calculate_preapproval_metrics <- function(
    model = NULL,
    pred = NULL,
    sd = NULL,
    data,
    previous_aadt_col = "lastYearAadt_aadt",
    model_name = "inla") {

  # Avoid NSE notes for dplyr
  parentTrafficLinkId <- eale <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Extract predictions and uncertainties
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  if (!is.null(model)) {
    pred <- round(model$summary.fitted.values[, "0.5quant"])
    sd <- round(model$summary.fitted.values[, "sd"])
  }

  if (is.null(model) & is.null(pred) & is.null(sd)) {
    if ("balanced_pred" %in% colnames(data)) {
      pred <- data$balanced_pred
      sd <- data$balanced_sd
    } else {
      pred <- data$pred
      sd <- data$sd
    }
  }

  # Add predictions to data
  data$pred <- pred
  data$sd <- sd


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aggregate to undirected segments and calculate metrics
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data_undirected <- data |>
    dplyr::group_by(parentTrafficLinkId) |>
    dplyr::summarise(
      pred = sum(.data$pred),
      sd = calculate_sd_of_sum(.data$sd),
      last_year = sum(.data[[previous_aadt_col]]),
      .groups = "drop"
    ) |>
    tidyr::drop_na(dplyr::all_of(c("last_year", "pred"))) |>
    dplyr::mutate(
      eale = exp(abs(log(.data$last_year) - log(.data$pred))) - 1,
      abs_diff = abs(.data$pred - .data$last_year),
      pct_change = (.data$pred - .data$last_year) / .data$last_year
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate summary statistics
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  summary_stats <- data_undirected |>
    dplyr::summarise(
      n_segments = dplyr::n(),
      median_eale = stats::median(.data$eale, na.rm = TRUE),
      mean_eale = mean(.data$eale[is.finite(.data$eale)], na.rm = TRUE),
      pct_eale_below_10 = mean(.data$eale < 0.10, na.rm = TRUE),  # Very stable
      pct_eale_below_20 = mean(.data$eale < 0.20, na.rm = TRUE),  # Reasonably stable
      pct_eale_above_50 = mean(.data$eale > 0.50, na.rm = TRUE),  # Large changes
      median_abs_diff = stats::median(.data$abs_diff, na.rm = TRUE),
      median_pct_change = stats::median(.data$pct_change, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(model = model_name, .before = 1)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return results
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  return(list(
    summary = summary_stats,
    directed_data = data,
    undirected_data = data_undirected
  ))
}

#' Print pre-approval summary for one or more models
#'
#' Formats and prints a summary table of pre-approval metrics comparing model
#' predictions to previous year's AADT values.
#'
#' @param ... One or more model objects with pre-approval summaries, or a
#'   single list of such models.
#'
#' @return Invisibly returns an unformatted tibble of pre-approval metrics.
#'
#' @details
#' Prints a formatted table showing how much predictions differ from previous
#' year's values. This helps identify if the model is producing reasonable
#' year-over-year changes.
#'
#' @examples
#' \dontrun{
#' print_preapproval_summary(model1, model2, model3)
#' print_preapproval_summary(list(model1, model2))
#' }
#'
#' @export
print_preapproval_summary <- function(...) {

  # Avoid NSE notes
  model <- n_segments <- median_eale <- pct_eale_below_20 <- pct_eale_above_50 <- NULL
  median_pct_change <- NULL

  models <- list(...)

  # If the first (and only) element is a list, unlist it
  if (length(models) == 1 && is.list(models[[1]])) {
    models <- models[[1]]
  }

  # Extract pre-approval summaries from each model
  summaries <- purrr::map_dfr(models, function(model) {
    model$summary |>
      dplyr::select(
        model,
        n_segments,
        median_eale,
        pct_eale_below_20,
        pct_eale_above_50,
        median_pct_change
      )
  })

  # Format percentages for readability
  summaries_formatted <- summaries |>
    dplyr::mutate(
      median_eale = sprintf("%.1f%%", .data$median_eale * 100),
      pct_eale_below_20 = sprintf("%.1f%%", .data$pct_eale_below_20 * 100),
      pct_eale_above_50 = sprintf("%.1f%%", .data$pct_eale_above_50 * 100),
      median_pct_change = sprintf("%+.1f%%", .data$median_pct_change * 100)
    ) |>
    dplyr::select(
      model,
      n_segments,
      median_eale,
      pct_eale_below_20,
      pct_eale_above_50,
      median_pct_change
    )

  print(summaries_formatted, row.names = FALSE)

  # Return the unformatted version for further use
  invisible(summaries)
}
