# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Autoapproval and validation functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.default_aadt_col <- "\u00c5DT.offisiell"

#' Calculate approval metrics for AADT predictions
#'
#' Main validation function that aggregates directed predictions to undirected
#' segments and calculates approval rates and error metrics. Compares model
#' predictions against manual estimates using autoapproval criteria.
#'
#' @param model An INLA model object with summary.fitted.values. If NULL, must
#'   provide pred and sd, or data must contain prediction columns.
#' @param pred Numeric vector of predictions. Used if model is NULL.
#' @param sd Numeric vector of standard deviations. Used if model is NULL.
#' @param data Data frame containing directed traffic link predictions with
#'   columns parentTrafficLinkId, and optionally pred/sd or balanced_pred/balanced_sd.
#' @param data_manual Data frame of manual estimates containing columns ID,
#'   Datagrunnlag.TRP.ID, Strekningslengde..m., and truth_col.
#' @param truth_col Character. Name of column in data_manual containing ground
#'   truth AADT values. Default is "<U+00C5>DT.offisiell".
#' @param aadt_col Character. Name of column containing total AADT for ratio
#'   models. Default is "<U+00C5>DT.offisiell".
#' @param model_name Character. Name to identify the model in output. Default
#'   is "inla".
#' @param is_ratio_model Logical. If TRUE, converts ratio predictions to counts
#'   using aadt_col. Default is FALSE.
#' @param ratio_uncertainty_fn Optional function(ratio, aadt) to estimate ratio
#'   uncertainty. If NULL, uses default estimation.
#'
#' @return List with four elements:
#'   \itemize{
#'     \item summary: Data frame with approval rates, coverage, and eALE metrics
#'     \item directed_data: Original data with added predictions
#'     \item undirected_data: Aggregated undirected segments with approval status
#'     \item segments_without_counters: Filtered data for segments without TRP counters
#'   }
#'
#' @importFrom rlang :=
#' @export
calculate_approval_metrics <- function(
    model = NULL,
    pred = NULL,
    sd = NULL,
    data,
    data_manual,
    truth_col = .default_aadt_col,
    aadt_col = .default_aadt_col,
    model_name = "inla",
    is_ratio_model = FALSE,
    ratio_uncertainty_fn = NULL) {

  # Avoid NSE notes for dplyr
  parentTrafficLinkId <- Datagrunnlag.TRP.ID <- Strekningslengde..m. <- NULL
  pred_lower <- pred_upper <- eale <- approved <- covered <- NULL
  truth_lower <- truth_upper <- NULL

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

  # Ensure columns are numeric
  data_manual[[truth_col]][data_manual[[truth_col]] == "null"] <- NA
  data_manual[[truth_col]] <- as.numeric(data_manual[[truth_col]])

  data_manual[[aadt_col]][data_manual[[aadt_col]] == "null"] <- NA
  data_manual[[aadt_col]] <- as.numeric(data_manual[[aadt_col]])

  # Cap unreasonable standard deviations
  data$sd[is.na(data$sd) | data$sd > 20000] <- 20000

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Aggregate to undirected segments
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  data_undirected <- data |>
    dplyr::select(parentTrafficLinkId, pred, sd) |>
    dplyr::group_by(parentTrafficLinkId) |>
    dplyr::summarise(
      pred = sum(.data$pred),
      sd = calculate_sd_of_sum(.data$sd),
      .groups = "drop"
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Join with manual estimates and calculate approval
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  combined_data <- data_manual |>
    dplyr::full_join(data_undirected, by = dplyr::join_by(.data$ID == parentTrafficLinkId)) |>
    dplyr::mutate(!!truth_col := as.numeric(.data[[truth_col]])) |>
    tidyr::drop_na(dplyr::all_of(c(truth_col, "pred")))

  # If validating a ratio model (e.g., heavy vehicle %), convert to counts
  if (is_ratio_model) {
    combined_data <- convert_ratio_to_count(
      data = combined_data,
      truth_col = truth_col,
      aadt_col = aadt_col,
      ratio_uncertainty_fn = ratio_uncertainty_fn
    )
  }

  combined_data <- combined_data |>
    dplyr::mutate(
      pred_lower = .data$pred - 1.96 * .data$sd,
      pred_upper = .data$pred + 1.96 * .data$sd,
      approved = check_autoapproval(
        truth = .data[[truth_col]],
        pred = .data$pred,
        pred_lower = .data$pred_lower,
        pred_upper = .data$pred_upper),
      eale = exp(abs(log(.data[[truth_col]]) - log(.data$pred))) - 1
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Filter to segments without traffic counters
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  segments_without_counters <- dplyr::filter(
    combined_data,
    .data$Datagrunnlag.TRP.ID == "null"
  )

  approved_segments <- dplyr::filter(segments_without_counters, .data$approved)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate approval rates
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  approval_rate_count <- nrow(approved_segments) / nrow(segments_without_counters)
  approval_rate_length <- sum(approved_segments$Strekningslengde..m.) /
    sum(segments_without_counters$Strekningslengde..m.)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate coverage (calibration check)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  segments_without_counters <- segments_without_counters |>
    dplyr::mutate(
      truth_lower = .data[[truth_col]] - 1.96 * estimate_standard_error(.data[[truth_col]]),
      truth_upper = .data[[truth_col]] + 1.96 * estimate_standard_error(.data[[truth_col]]),
      covered = .data[[truth_col]] > .data$pred_lower & .data[[truth_col]] < .data$pred_upper
    )

  coverage_rate <- sum(segments_without_counters$covered) / nrow(segments_without_counters)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate failure mode breakdown
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  failure_modes <- segments_without_counters |>
    dplyr::mutate(
      eale_threshold = calculate_eale_threshold(.data$pred),
      approved_eale = .data$eale < .data$eale_threshold,
      approved_lower_bound = .data$pred_lower < .data$truth_upper,
      approved_upper_bound = .data$pred_upper > .data$truth_lower,
      fail_eale = !.data$approved_eale,
      fail_lower = !.data$approved_lower_bound,
      fail_upper = !.data$approved_upper_bound
    ) |>
    dplyr::summarise(
      pct_fail_eale = mean(.data$fail_eale, na.rm = TRUE),
      pct_fail_lower = mean(.data$fail_lower, na.rm = TRUE),
      pct_fail_upper = mean(.data$fail_upper, na.rm = TRUE),
      .groups = "drop"
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculate eALE statistics
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  eale_stats <- segments_without_counters |>
    dplyr::summarise(
      #mean_eale = mean(eale, na.rm = TRUE), # Typically Inf
      median_eale = stats::median(.data$eale, na.rm = TRUE),
      pct_eale_below_10 = mean(.data$eale < 0.10, na.rm = TRUE),  # Very good predictions
      pct_eale_below_20 = mean(.data$eale < 0.20, na.rm = TRUE),  # Good predictions
      pct_eale_above_50 = mean(.data$eale > 0.50, na.rm = TRUE),  # Poor predictions
      .groups = "drop"
    )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Compile summary metrics
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  summary_metrics <- data.frame(
    model = model_name,
    approval_rate_count = approval_rate_count,
    approval_rate_length = approval_rate_length,
    coverage_rate = coverage_rate
  ) |>
    dplyr::bind_cols(failure_modes) |>
    dplyr::bind_cols(eale_stats)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return results
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  return(list(
    summary = summary_metrics,
    directed_data = data,
    undirected_data = combined_data,
    segments_without_counters = segments_without_counters
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Helper Functions
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Calculate standard deviation of sum assuming independence
#'
#' Combines standard deviations for independent random variables using the
#' formula SD(X + Y) = sqrt(SD(X)^2 + SD(Y)^2).
#'
#' @param sd Numeric vector of standard deviations to combine.
#'
#' @return Numeric. The combined standard deviation.
#'
#' @examples
#' calculate_sd_of_sum(c(10, 20, 30))  # Returns sqrt(10^2 + 20^2 + 30^2)
#'
#' @export
calculate_sd_of_sum <- function(sd) {
  return(sqrt(sum(sd^2)))
}

#' Convert ratio-based truth values to counts for validation
#'
#' For ratio models (e.g., heavy vehicle percentage), converts ratio predictions
#' and uncertainties to absolute counts by multiplying with total AADT and
#' propagating uncertainties.
#'
#' @param data Data frame containing ratio predictions and AADT values.
#' @param truth_col Character. Name of column containing ratio truth values.
#' @param aadt_col Character. Name of column containing total AADT values.
#' @param ratio_uncertainty_fn Optional function(ratio, aadt) to estimate ratio
#'   standard error. If NULL, uses default estimation based on ratio value.
#'
#' @return Data frame with truth_col converted to counts and added uncertainty column.
#'
#' @details
#' Uncertainty propagation uses: Var(ratio x aadt) = (approx.) (ratio x SE_aadt)^2 + (aadt * SE_ratio)^2
#'
#' @export
convert_ratio_to_count <- function(data, truth_col, aadt_col, ratio_uncertainty_fn = NULL) {

  # Extract the ratio and AADT
  ratio <- data[[truth_col]]
  aadt <- data[[aadt_col]]

  # Convert ratio to count
  count <- ratio * aadt

  # Estimate uncertainty in the count
  if (!is.null(ratio_uncertainty_fn)) {
    # User-provided function to estimate ratio uncertainty
    ratio_se <- ratio_uncertainty_fn(ratio, aadt)
  } else {
    # Default: assume ratio uncertainty scales with ratio and inversely with AADT
    # This is a placeholder - you should calibrate this based on your data
    ratio_se <- estimate_ratio_uncertainty(ratio, aadt)
  }

  # Estimate AADT uncertainty (reusing existing function)
  aadt_se <- estimate_standard_error(aadt)

  # Propagate uncertainty: Var(ratio * aadt) ≈ (ratio * aadt_se)^2 + (aadt * ratio_se)^2
  count_se <- sqrt((ratio * aadt_se)^2 + (aadt * ratio_se)^2)

  # Store the converted count as the new truth value
  data[[truth_col]] <- count

  # Store the uncertainty for later use in approval checks
  data[[paste0(truth_col, "_se")]] <- count_se

  return(data)
}

#' Estimate uncertainty in ratio estimates
#'
#' Default function to estimate standard error of ratio estimates (e.g., heavy
#' vehicle percentage). Assumes lower ratios have higher relative uncertainty.
#'
#' @param ratio Numeric vector of ratio values (e.g., 0.15 for 15\%).
#' @param aadt Numeric vector of total AADT values (currently unused but available
#'   for future calibration).
#'
#' @return Numeric vector of estimated standard errors for the ratios.
#'
#' @details
#' This is a placeholder function - you should calibrate based on your data.
#' Current assumptions:
#' \itemize{
#'   \item Very low ratios (< 5\%) have SE = 0.02
#'   \item Low ratios (5-10\%) have SE = 0.015
#'   \item Medium ratios (10-20\%) have SE = 0.01
#'   \item Higher ratios (> 20\%) have SE = 0.01
#' }
#'
#' @export
estimate_ratio_uncertainty <- function(ratio, aadt) {
  dplyr::case_when(
    # Very low ratios are less reliable
    ratio < 0.05 ~ 0.02,
    ratio < 0.10 ~ 0.015,
    ratio < 0.20 ~ 0.01,
    # Higher ratios are more stable
    TRUE ~ 0.01
  )
}

#' Check if a prediction meets autoapproval criteria
#'
#' Determines whether a traffic prediction should be automatically approved by
#' checking three criteria: (1) eALE below threshold, (2) prediction confidence
#' interval overlaps with truth lower bound, (3) prediction confidence interval
#' overlaps with truth upper bound.
#'
#' @param truth Numeric vector of ground truth AADT values.
#' @param pred Numeric vector of predicted AADT values.
#' @param pred_lower Numeric vector of prediction lower bounds (typically pred - 1.96*sd).
#' @param pred_upper Numeric vector of prediction upper bounds (typically pred + 1.96*sd).
#'
#' @return Logical vector indicating whether each prediction is approved.
#'
#' @details
#' All three criteria must be met for approval:
#' \itemize{
#'   \item eALE < threshold(pred): Exponential Absolute Log Error below volume-dependent threshold
#'   \item pred_lower < truth_upper: Prediction CI lower bound below truth CI upper bound
#'   \item pred_upper > truth_lower: Prediction CI upper bound above truth CI lower bound
#' }
#'
#' @seealso \code{\link{calculate_eale_threshold}}, \code{\link{estimate_standard_error}}
#'
#' @export
check_autoapproval <- function(truth, pred, pred_lower, pred_upper) {

  # Calculate eALE and check against threshold
  threshold <- calculate_eale_threshold(pred)
  eale <- exp(abs(log(truth) - log(pred))) - 1
  approved_eale <- eale < threshold

  # Calculate truth confidence interval
  truth_se <- estimate_standard_error(truth)
  truth_lower <- truth - 1.96 * truth_se
  truth_upper <- truth + 1.96 * truth_se

  # Check if confidence intervals overlap
  approved_lower_bound <- pred_lower < truth_upper
  approved_upper_bound <- pred_upper > truth_lower

  # All three criteria must be met
  approved <- approved_eale & approved_lower_bound & approved_upper_bound

  return(approved)
}

#' Calculate eALE threshold as function of predicted AADT
#'
#' Returns volume-dependent threshold for exponential Absolute Log Error (eALE).
#' Threshold is more lenient for low-volume roads and stricter for high-volume roads.
#'
#' @param pred_aadt Numeric vector of predicted AADT values.
#'
#' @return Numeric vector of eALE thresholds.
#'
#' @details
#' Threshold function uses three regions:
#' \itemize{
#'   \item 0-1000: Sigmoid from ~200% to ~40%, reflecting high uncertainty in low volumes
#'   \item 1000-10000: Square-root transition to ~25%
#'   \item 10000-50000: Square-root decrease to 20%
#'   \item 50000+: Constant 20% threshold
#' }
#'
#' @examples
#' calculate_eale_threshold(c(100, 500, 1000, 5000, 20000, 60000))
#'
#' @export
calculate_eale_threshold <- function(pred_aadt) {

  # Pre-calculate sigmoid value at 1000
  sigmoid_at_1000 <- 2 - (1.6 / (1 + exp(-0.01 * (1000 - 500))))

  # Calculate value at 10000
  value_at_10000 <- sigmoid_at_1000 - 0.15 * ((10000 - 1000) / 9000)^0.5

  dplyr::case_when(
    # Sigmoid: high threshold for low traffic, decreasing to ~40% at 1000
    pred_aadt <= 1000  ~ 2 - (1.6 / (1 + exp(-0.01 * (pred_aadt - 500)))),
    # Transition curve from 1000 to 10000, reaching ~25% at 10000
    pred_aadt <= 10000 ~ sigmoid_at_1000 - 0.15 * ((pred_aadt - 1000) / 9000)^0.5,
    # Decreasing from 10000 to 50000, reaching 20% at 50000
    pred_aadt <= 50000 ~ value_at_10000 - 0.06 * ((pred_aadt - 10000) / 40000)^0.5,
    # Constant 20% threshold for high traffic volumes
    TRUE               ~ 0.20
  )
}

#' Estimate standard error for manual traffic estimates
#'
#' Provides volume-dependent standard error estimates for manual AADT estimates
#' based on empirical uncertainty patterns.
#'
#' @param aadt Numeric vector of AADT values.
#'
#' @return Numeric vector of estimated standard errors.
#'
#' @details
#' Standard error bins:
#' \itemize{
#'   \item < 500: SE = 40
#'   \item 500-1000: SE = 100
#'   \item 1000-5000: SE = 200
#'   \item 5000-10000: SE = 400
#'   \item 10000-30000: SE = 1000
#'   \item 30000+: SE = 2000
#' }
#'
#' @examples
#' estimate_standard_error(c(200, 800, 3000, 15000, 40000))
#'
#' @export
estimate_standard_error <- function(aadt) {
  dplyr::case_when(
    aadt < 500    ~ 40,
    aadt < 1000   ~ 100,
    aadt < 5000   ~ 200,
    aadt < 10000  ~ 400,
    aadt < 30000  ~ 1000,
    TRUE          ~ 2000
  )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Segmented Analysis Function
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Calculate approval rates by segment characteristics
#'
#' Stratifies validation results by segment characteristics (e.g., AADT bins)
#' to understand model performance across different traffic volume levels.
#'
#' @param validation_results List output from \code{\link{calculate_approval_metrics}}.
#' @param segment_by Character. Segmentation strategy. Currently only "aadt_bin"
#'   is implemented.
#'
#' @return Data frame with columns: segment_group, n_segments, approval_rate,
#'   mean_eale, median_eale, coverage_rate.
#'
#' @details
#' AADT bins: 0-500, 500-1000, 1000-5000, 5000-10000, 10000+
#'
#' @export
calculate_approval_by_segment <- function(validation_results, segment_by = "aadt_bin") {

  # Avoid NSE notes
  segment_group <- pred <- official_aadt <- approved <- covered <- NULL

  segments <- validation_results$segments_without_counters

  if (segment_by == "aadt_bin") {
    segments <- segments |>
      dplyr::mutate(
        segment_group = dplyr::case_when(
          .data$pred < 500    ~ "0-500",
          .data$pred < 1000   ~ "500-1000",
          .data$pred < 5000   ~ "1000-5000",
          .data$pred < 10000  ~ "5000-10000",
          TRUE                ~ "10000+"
        )
      )
  }

  segmented_results <- segments |>
    dplyr::group_by(segment_group) |>
    dplyr::summarise(
      n_segments = dplyr::n(),
      approval_rate = mean(.data$approved),
      mean_eale = mean(exp(abs(log(.data$official_aadt) - log(.data$pred))) - 1),
      median_eale = stats::median(exp(abs(log(.data$official_aadt) - log(.data$pred))) - 1),
      coverage_rate = mean(.data$covered),
      .groups = "drop"
    )

  return(segmented_results)
}

#' Get approval rates per group
#'
#' Calculates approval fraction for a specified grouping variable and joins with
#' municipality names.
#'
#' @param df Data frame containing approval data with approved column.
#' @param group_name Character. Name of column to group by (e.g., "Kommunenr").
#'
#' @return Data frame with columns: group_name, fraction_approved, links_in_group,
#'   kommunenavn (if group is Kommunenr).
#'
#' @details
#' Expects a CSV file at "data/raw/kommunenummer.csv" with columns kommunenummer
#' and kommunenavn for municipality name lookup.
#'
#' @export
get_approval_per_group <- function(df, group_name){

  # Avoid NSE notes
  kommunenummer <- kommunenavn <- Kommunenr <- approved <- NULL
  fraction_approved <- links_in_group <- NULL

  kommunenavn_df <- utils::read.csv("data/raw/kommunenummer.csv", sep = ";") |>
    dplyr::select(kommunenummer, kommunenavn)

  approval_per_group <- df |>
    dplyr::group_by(.data[[group_name]]) |>
    dplyr::summarise(
      fraction_approved = sum(.data$approved) / dplyr::n(),
      links_in_group = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(fraction_approved)) |>
    dplyr::left_join(kommunenavn_df, by = dplyr::join_by(Kommunenr == kommunenummer))

  return(approval_per_group)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Print Approval Summary
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Extract and display key approval metrics from one or more models
#'
#' Formats and prints a summary table of approval rates, median eALE, and
#' coverage rates for multiple models.
#'
#' @param ... One or more model objects with approval summaries at
#'   model$diagnostics$approval$summary, or a single list of such models.
#'
#' @return Invisibly returns an unformatted tibble of approval metrics.
#'
#' @details
#' Prints a formatted table with percentage values for easy reading. The
#' returned (invisible) object contains the original numeric values for
#' further analysis.
#'
#' @examples
#' \dontrun{
#' print_approval_summary(model1, model2, model3)
#' print_approval_summary(list(model1, model2))
#' }
#'
#' @export
print_approval_summary <- function(...) {

  # Avoid NSE notes
  model <- approval_rate_count <- approval_rate_length <- median_eale <- coverage_rate <- NULL
  approval_count <- approval_length <- NULL

  models <- list(...)

  # If the first (and only) element is a list, unlist it
  if (length(models) == 1 && is.list(models[[1]])) {
    models <- models[[1]]
  }

  # Extract approval summaries from each model
  summaries <- purrr::map_dfr(models, function(model) {
    model$diagnostics$approval$summary |>
      dplyr::select(model, approval_rate_count, approval_rate_length, median_eale, coverage_rate)
  })

  # Format percentages for readability
  summaries_formatted <- summaries |>
    dplyr::mutate(
      approval_count = sprintf("%.1f%%", .data$approval_rate_count * 100),
      approval_length = sprintf("%.1f%%", .data$approval_rate_length * 100),
      median_eale = sprintf("%.1f%%", .data$median_eale * 100),
      coverage_rate = sprintf("%.1f%%", .data$coverage_rate * 100)
    ) |>
    dplyr::select(
      model,
      approval_count,
      approval_length,
      median_eale,
      coverage_rate
    )

  print(summaries_formatted, row.names = FALSE)

  # Return the unformatted version for further use
  invisible(summaries)
}
