# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Predictions against last year's values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot AADT predictions against last year's values
#'
#' Creates a scatter plot comparing predicted AADT values against the previous
#' year's AADT, with optional log scaling and faceting. Useful for assessing
#' year-over-year prediction stability.
#'
#' @param directed_predictions Data frame with prediction and last year columns
#' @param heavy_vehicle Logical; plot heavy vehicle predictions? Default FALSE
#' @param balanced Logical; use balanced predictions? Default TRUE
#' @param log10_axis Logical; apply pseudo-log transformation to both axes? Default FALSE
#' @param color_by Optional column name for color aesthetic
#' @param facet_by Optional column name for one-way faceting
#' @param facet_by_2 Optional column name for two-way faceting (with facet_by)
#' @param facet_scales Facet scale type: "fixed", "free", "free_x", "free_y". Default "fixed"
#'
#' @return A ggplot object with predictions (y-axis) vs last year's AADT (x-axis)
#'   and a 1:1 reference line. Points above the line indicate increased predicted
#'   traffic; points below indicate decreased traffic.
#'
#' @details
#' The function selects columns based on `heavy_vehicle` and `balanced`:
#' * Total AADT: `balanced_pred`/`inla_pred` vs `lastYearAadt_aadt`
#' * Heavy vehicles: `balanced_pred_heavy`/`inla_pred_heavy` vs `lastYearAadt_heavyAadt`
#'
#' When `log10_axis = TRUE`, uses `scales::pseudo_log_trans()` which handles
#' zeros gracefully while maintaining log-like scaling for larger values.
#'
#' @export
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_abline labs theme_minimal
#'   scale_x_continuous scale_y_continuous annotation_logticks
#' @importFrom scales pseudo_log_trans
#' @importFrom rlang .data
plot_predictions_against_last_year <- function(directed_predictions,
                                               heavy_vehicle = FALSE,
                                               balanced = TRUE,
                                               log10_axis = FALSE,
                                               color_by = NULL,
                                               facet_by = NULL,
                                               facet_by_2 = NULL,
                                               facet_scales = "fixed") {
  # Select appropriate prediction column
  pred_col <- if (heavy_vehicle) {
    if (balanced) "balanced_pred_heavy" else "inla_pred_heavy"
  } else {
    if (balanced) "balanced_pred" else "inla_pred"
  }
  # Select appropriate last year AADT column
  last_year_col <- if (heavy_vehicle) {
    "lastYearAadt_heavyAadt"
  } else {
    "lastYearAadt_aadt"
  }
  # Create base plot
  if (is.null(color_by)) {
    p <- ggplot2::ggplot(directed_predictions,
                         ggplot2::aes(x = .data[[last_year_col]],
                                      y = .data[[pred_col]])) +
      ggplot2::geom_point(alpha = 0.5)
  } else {
    p <- ggplot2::ggplot(directed_predictions,
                         ggplot2::aes(x = .data[[last_year_col]],
                                      y = .data[[pred_col]],
                                      color = .data[[color_by]])) +
      ggplot2::geom_point(alpha = 0.5)
  }
  # Build on base plot
  p <- p +
    ggplot2::geom_abline(intercept = 0, slope = 1,
                         linetype = "dashed", color = "red") +
    ggplot2::labs(
      x = paste0("Last year AADT", if (heavy_vehicle) " (heavy vehicles)" else ""),
      y = paste0("Predicted AADT", if (heavy_vehicle) " (heavy vehicles)" else ""),
      title = paste0(
        if (balanced) "Balanced" else "INLA",
        " predictions vs last year",
        if (heavy_vehicle) " (heavy vehicles)" else ""
      )
    ) +
    ggplot2::theme_minimal()

  # Add faceting if requested
  if (!is.null(facet_by)) {
    if (!is.null(facet_by_2)) {
      # Two-way faceting (grid)
      p <- p + ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[facet_by]]),
        cols = ggplot2::vars(.data[[facet_by_2]]),
        scales = facet_scales
      )
    } else {
      # One-way faceting (wrap)
      p <- p + ggplot2::facet_wrap(
        ggplot2::vars(.data[[facet_by]]),
        scales = facet_scales
      )
    }
  }

  # Add log10 scales if requested
  if (log10_axis) {
    p <- p +
      ggplot2::scale_x_continuous(trans = scales::pseudo_log_trans(base = 10),
                                  breaks = c(0, 1, 10, 100, 1000, 10000, 100000)) +
      ggplot2::scale_y_continuous(trans = scales::pseudo_log_trans(base = 10),
                                  breaks = c(0, 1, 10, 100, 1000, 10000, 100000)) +
      ggplot2::annotation_logticks()
  }
  return(p)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Uncertainty ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot prediction uncertainty vs predicted AADT
#'
#' Creates a scatter plot showing prediction uncertainty (standard deviation)
#' against predicted AADT values, with optional log scaling, faceting, and
#' color grouping. Useful for understanding how uncertainty scales with traffic
#' volume and identifying high-uncertainty segments.
#'
#' @param directed_predictions Data frame with prediction and uncertainty columns
#' @param heavy_vehicle Logical; plot heavy vehicle predictions? Default FALSE
#' @param balanced Logical; use balanced predictions? Default TRUE
#' @param log10_x Logical; apply pseudo-log transformation to x-axis? Default FALSE
#' @param log10_y Logical; apply pseudo-log transformation to y-axis? Default FALSE
#' @param color_by Optional column name for color aesthetic
#' @param alpha Point transparency (0-1). Default 0.5
#' @param cap_sd Optional SD threshold; values above are excluded with warning
#' @param title_text Optional text to append to plot title
#' @param facet_by Optional column name for one-way faceting
#' @param facet_by_2 Optional column name for two-way faceting (with facet_by)
#' @param facet_scales Facet scale type: "fixed", "free", "free_x", "free_y". Default "fixed"
#'
#' @return A ggplot object showing standard deviation (y-axis) vs predicted
#'   AADT (x-axis)
#'
#' @details
#' The function selects columns based on `heavy_vehicle` and `balanced`:
#' * Total AADT: `balanced_pred`/`inla_pred` and `balanced_sd`/`inla_sd`
#' * Heavy vehicles: `balanced_pred_heavy`/`inla_pred_heavy` and `balanced_sd_heavy`/`inla_sd_heavy`
#'
#' Using `color_by` is useful for distinguishing measured vs unmeasured segments,
#' comparing across road classes, or identifying spatial patterns.
#'
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal
#'   scale_x_continuous scale_y_continuous annotation_logticks
#' @importFrom scales pseudo_log_trans
#' @importFrom rlang .data
#'
#' @export
plot_prediction_uncertainty <- function(directed_predictions,
                                        heavy_vehicle = FALSE,
                                        balanced = TRUE,
                                        log10_x = FALSE,
                                        log10_y = FALSE,
                                        color_by = NULL,
                                        alpha = 0.5,
                                        cap_sd = NULL,
                                        title_text = NULL,
                                        facet_by = NULL,
                                        facet_by_2 = NULL,
                                        facet_scales = "fixed") {
  # Select appropriate prediction column
  pred_col <- if (heavy_vehicle) {
    if (balanced) "balanced_pred_heavy" else "inla_pred_heavy"
  } else {
    if (balanced) "balanced_pred" else "inla_pred"
  }
  # Select appropriate uncertainty column
  sd_col <- if (heavy_vehicle) {
    if (balanced) "balanced_sd_heavy" else "inla_sd_heavy"
  } else {
    if (balanced) "balanced_sd" else "inla_sd"
  }
  # Cap extreme values if requested
  plot_data <- directed_predictions
  if (!is.null(cap_sd)) {
    n_capped <- sum(plot_data[[sd_col]] > cap_sd, na.rm = TRUE)
    if (n_capped > 0) {
      warning(sprintf("%d values (%.2f%%) exceeded SD cap of %s and were excluded from plot",
                      n_capped,
                      100 * n_capped / nrow(plot_data),
                      format(cap_sd, big.mark = " ", scientific = FALSE)))
      plot_data <- plot_data[plot_data[[sd_col]] <= cap_sd, ]
    }
  }
  # Create base plot
  if (is.null(color_by)) {
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = .data[[pred_col]],
                                      y = .data[[sd_col]])) +
      ggplot2::geom_point(alpha = alpha)
  } else {
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = .data[[pred_col]],
                                      y = .data[[sd_col]],
                                      color = .data[[color_by]])) +
      ggplot2::geom_point(alpha = alpha)
  }
  # Add labels
  p <- p +
    ggplot2::labs(
      x = paste0("Predicted AADT", if (heavy_vehicle) " (heavy vehicles)" else ""),
      y = "Standard deviation",
      title = paste0(
        if (balanced) "Balanced" else "INLA",
        " prediction uncertainty",
        if (heavy_vehicle) " (heavy vehicles)" else "",
        if (!is.null(title_text)) paste0(", ", title_text) else ""
      )
    ) +
    ggplot2::theme_minimal()

  # Add faceting if requested
  if (!is.null(facet_by)) {
    if (!is.null(facet_by_2)) {
      # Two-way faceting (grid)
      p <- p + ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[facet_by]]),
        cols = ggplot2::vars(.data[[facet_by_2]]),
        scales = facet_scales
      )
    } else {
      # One-way faceting (wrap)
      p <- p + ggplot2::facet_wrap(
        ggplot2::vars(.data[[facet_by]]),
        scales = facet_scales
      )
    }
  }

  # Add x-axis transformation if requested
  if (log10_x) {
    p <- p +
      ggplot2::scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = " "),
        breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10^7, 10^8, 10^9)
      ) +
      ggplot2::annotation_logticks(sides = "b")
  } else {
    p <- p +
      ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = " "))
  }
  # Add y-axis transformation if requested
  if (log10_y) {
    p <- p +
      ggplot2::scale_y_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = " "),
        breaks = c(0, 1, 10, 100, 1000, 10000, 100000, 1000000, 10^7, 10^8, 10^9)#,
        #limits = c(0, 100000)
      ) +
      ggplot2::annotation_logticks(sides = "l")
  } else {
    p <- p +
      ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " "))
  }
  # Adjust logticks if both axes are log
  if (log10_x && log10_y) {
    p <- p +
      ggplot2::annotation_logticks(sides = "bl")
  }
  return(p)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Coefficient of variation ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Plot prediction coefficient of variation
#'
#' Creates a scatter plot of predicted AADT vs coefficient of variation (SD/mean).
#'
#' @param directed_predictions Data frame with prediction results
#' @param heavy_vehicle Logical; plot heavy vehicle predictions? Default FALSE
#' @param balanced Logical; use balanced predictions? Default TRUE
#' @param log10_x Logical; log transform x-axis? Default FALSE
#' @param log10_y Logical; log transform y-axis? Default FALSE
#' @param color_by Optional column name for color aesthetic
#' @param alpha Point transparency (0-1). Default 0.5
#' @param cap_cv Optional CV threshold; values above are excluded with warning
#'
#' @return A ggplot object
#' @export
plot_prediction_cv <- function(directed_predictions,
                               heavy_vehicle = FALSE,
                               balanced = TRUE,
                               log10_x = FALSE,
                               log10_y = FALSE,
                               color_by = NULL,
                               alpha = 0.5,
                               cap_cv = NULL) {

  # Select appropriate prediction column
  pred_col <- if (heavy_vehicle) {
    if (balanced) "balanced_pred_heavy" else "inla_pred_heavy"
  } else {
    if (balanced) "balanced_pred" else "inla_pred"
  }

  # Select appropriate uncertainty column
  sd_col <- if (heavy_vehicle) {
    if (balanced) "balanced_sd_heavy" else "inla_sd_heavy"
  } else {
    if (balanced) "balanced_sd" else "inla_sd"
  }

  # Calculate CV
  plot_data <- directed_predictions
  plot_data$cv <- plot_data[[sd_col]] / plot_data[[pred_col]]

  # Cap extreme values if requested
  if (!is.null(cap_cv)) {
    n_capped <- sum(plot_data$cv > cap_cv, na.rm = TRUE)
    if (n_capped > 0) {
      warning(sprintf("%d values (%.2f%%) exceeded CV cap of %.2f and were excluded from plot",
                      n_capped,
                      100 * n_capped / nrow(plot_data),
                      cap_cv))
      plot_data <- plot_data[plot_data$cv <= cap_cv, ]
    }
  }

  # Create base plot
  if (is.null(color_by)) {
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = .data[[pred_col]],
                                      y = .data[["cv"]])) +
      ggplot2::geom_point(alpha = alpha)
  } else {
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = .data[[pred_col]],
                                      y = .data[["cv"]],
                                      color = .data[[color_by]])) +
      ggplot2::geom_point(alpha = alpha)
  }

  # Add labels
  p <- p +
    ggplot2::labs(
      x = paste0("Predicted AADT", if (heavy_vehicle) " (heavy vehicles)" else ""),
      y = "Coefficient of variation (SD/mean)",
      title = paste0(
        if (balanced) "Balanced" else "INLA",
        " prediction uncertainty",
        if (heavy_vehicle) " (heavy vehicles)" else ""
      )
    ) +
    ggplot2::theme_minimal()

  # Add x-axis transformation if requested
  if (log10_x) {
    p <- p +
      ggplot2::scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = " ")
      ) +
      ggplot2::annotation_logticks(sides = "b")
  } else {
    p <- p +
      ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = " "))
  }

  # Add y-axis transformation if requested
  if (log10_y) {
    p <- p +
      ggplot2::scale_y_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = " ")
      ) +
      ggplot2::annotation_logticks(sides = "l")
  } else {
    p <- p +
      ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " "))
  }

  # Adjust logticks if both axes are log
  if (log10_x && log10_y) {
    p <- p +
      ggplot2::annotation_logticks(sides = "bl")
  }

  return(p)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Histogram of prediction relative uncertainty ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot histogram of prediction relative uncertainty
#'
#' Creates a histogram of coefficient of variation (SD/predicted AADT) with
#' optional faceting and reference lines.
#'
#' @param directed_predictions Data frame with prediction results
#' @param heavy_vehicle Logical; plot heavy vehicle predictions? Default FALSE
#' @param balanced Logical; use balanced predictions? Default TRUE
#' @param log10_x Logical; log transform x-axis? Default FALSE
#' @param cap_cv Optional CV threshold; values above are excluded with warning
#' @param facet_by Optional column name for one-way faceting
#' @param facet_by_2 Optional column name for two-way faceting (with facet_by)
#' @param facet_scales Facet scale type: "fixed", "free", "free_x", "free_y". Default "fixed"
#' @param bins Number of histogram bins. Default 50
#' @param show_density Logical; show density instead of count? Default FALSE
#' @param reference_lines Numeric vector of vertical reference lines. Default c(0.5, 1.0)
#' @param show_n Logical; show sample sizes in facet labels? Default TRUE
#'
#' @return A ggplot object
#' @export
plot_prediction_relative_uncertainty_histogram <- function(directed_predictions,
                                                           heavy_vehicle = FALSE,
                                                           balanced = TRUE,
                                                           log10_x = FALSE,
                                                           cap_cv = NULL,
                                                           facet_by = NULL,
                                                           facet_by_2 = NULL,
                                                           facet_scales = "fixed",
                                                           bins = 50,
                                                           show_density = FALSE,
                                                           reference_lines = c(0.5, 1.0),
                                                           show_n = TRUE) {

  # Select appropriate prediction column
  pred_col <- if (heavy_vehicle) {
    if (balanced) "balanced_pred_heavy" else "inla_pred_heavy"
  } else {
    if (balanced) "balanced_pred" else "inla_pred"
  }

  # Select appropriate uncertainty column
  sd_col <- if (heavy_vehicle) {
    if (balanced) "balanced_sd_heavy" else "inla_sd_heavy"
  } else {
    if (balanced) "balanced_sd" else "inla_sd"
  }

  # Calculate relative uncertainty (CV)
  plot_data <- directed_predictions
  plot_data$relative_uncertainty <- plot_data[[sd_col]] / plot_data[[pred_col]]

  # Cap extreme values if requested
  if (!is.null(cap_cv)) {
    n_capped <- sum(plot_data$relative_uncertainty > cap_cv, na.rm = TRUE)
    if (n_capped > 0) {
      warning(sprintf("%d values (%.2f%%) exceeded relative uncertainty cap of %.2f and were excluded from plot",
                      n_capped,
                      100 * n_capped / nrow(plot_data),
                      cap_cv))
      plot_data <- plot_data[plot_data$relative_uncertainty <= cap_cv, ]
    }
  }

  # Create base plot
  if (show_density) {
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = .data[["relative_uncertainty"]],
                                      y = ggplot2::after_stat(density))) +
      ggplot2::geom_histogram(bins = bins, fill = "steelblue", color = "white", alpha = 0.7)
  } else {
    p <- ggplot2::ggplot(plot_data,
                         ggplot2::aes(x = .data[["relative_uncertainty"]])) +
      ggplot2::geom_histogram(bins = bins, fill = "steelblue", color = "white", alpha = 0.7)
  }

  # Add reference lines if provided
  if (!is.null(reference_lines)) {
    p <- p +
      ggplot2::geom_vline(xintercept = reference_lines,
                          linetype = "dashed",
                          color = "red",
                          alpha = 0.6)
  }

  # Add sample size labels if faceting and requested
  if (show_n && (!is.null(facet_by) || !is.null(facet_by_2))) {
    # Calculate sample sizes for each facet combination
    if (!is.null(facet_by) && !is.null(facet_by_2)) {
      # Two-way faceting
      n_labels <- plot_data |>
        dplyr::group_by(.data[[facet_by]], .data[[facet_by_2]]) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(label = paste0("n = ", format(n, big.mark = " ")))
    } else if (!is.null(facet_by)) {
      # One-way faceting
      n_labels <- plot_data |>
        dplyr::group_by(.data[[facet_by]]) |>
        dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
        dplyr::mutate(label = paste0("n = ", format(n, big.mark = " ")))
    }

    # Add text labels to plot
    p <- p +
      ggplot2::geom_text(
        data = n_labels,
        ggplot2::aes(x = Inf, y = Inf, label = label),
        hjust = 1.1,
        vjust = 1.5,
        size = 3,
        inherit.aes = FALSE
      )
  }

  # Add labels
  p <- p +
    ggplot2::labs(
      x = "Relative uncertainty (SD/predicted AADT)",
      y = if (show_density) "Density" else "Number of traffic links",
      title = paste0(
        if (balanced) "Balanced" else "INLA",
        " prediction relative uncertainty",
        if (heavy_vehicle) " (heavy vehicles)" else ""
      )
    ) +
    ggplot2::theme_bw()

  # Add faceting if requested
  if (!is.null(facet_by)) {
    if (!is.null(facet_by_2)) {
      # Two-way faceting (grid)
      p <- p + ggplot2::facet_grid(
        rows = ggplot2::vars(.data[[facet_by]]),
        cols = ggplot2::vars(.data[[facet_by_2]]),
        scales = facet_scales
      )
    } else {
      # One-way faceting (wrap)
      p <- p + ggplot2::facet_wrap(
        ggplot2::vars(.data[[facet_by]]),
        scales = facet_scales
      )
    }
  }

  # Add x-axis transformation if requested
  if (log10_x) {
    p <- p +
      ggplot2::scale_x_continuous(
        trans = scales::pseudo_log_trans(base = 10),
        labels = scales::label_number(big.mark = " ")
      ) +
      ggplot2::annotation_logticks(sides = "b")
  } else {
    p <- p +
      ggplot2::scale_x_continuous(labels = scales::label_number(big.mark = " "))
  }

  # Format y-axis
  p <- p +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = " "))

  return(p)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Leaflet map of traffic links ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Create interactive map of traffic links
#'
#' Creates a leaflet map displaying traffic links colored by a specified variable.
#' Useful for spatial exploration of predictions, uncertainty, and quality control.
#'
#' @param directed_predictions A data frame containing prediction data with geometry.
#'   Must be an sf object or convertible to one.
#' @param color_by Character string specifying the column name to color links by.
#'   Default is "uncertainty_category".
#' @param heavy_vehicle Logical. If `TRUE`, uses heavy vehicle predictions and
#'   uncertainties in popups. If `FALSE` (default), uses total AADT.
#' @param balanced Logical. If `TRUE` (default), uses flow-balanced predictions.
#'   If `FALSE`, uses raw INLA predictions.
#' @param palette Character string specifying the color palette. For categorical
#'   variables, options include "Set1", "Set2", "Dark2", "Paired". For continuous
#'   variables, options include "YlOrRd", "RdYlGn", "viridis", "plasma". Default
#'   is "RdYlGn" (reversed so low=green, high=red).
#' @param opacity Numeric between 0 and 1 controlling line opacity. Default is 0.8.
#' @param weight Numeric controlling line width. Default is 3.
#'
#' @return A leaflet map object showing traffic links with interactive popups.
#'
#' @examples
#' \dontrun{
#' # Color by uncertainty category
#' plot_traffic_links_map(predictions, color_by = "uncertainty_category")
#'
#' # Color by relative uncertainty (continuous)
#' plot_traffic_links_map(
#'   predictions,
#'   color_by = "relative_uncertainty",
#'   palette = "YlOrRd"
#' )
#'
#' # Color by road class
#' plot_traffic_links_map(
#'   predictions,
#'   color_by = "vegkategori",
#'   palette = "Set2"
#' )
#' }
#'
#' @importFrom leaflet leaflet addTiles addPolylines colorFactor colorNumeric
#'   addLegend
#' @importFrom sf st_transform
#'
#' @export
plot_traffic_links_map <- function(directed_predictions,
                                   color_by = "uncertainty_category",
                                   heavy_vehicle = FALSE,
                                   balanced = TRUE,
                                   palette = "RdYlGn",
                                   opacity = 0.8,
                                   weight = 3) {
  # Add traffic link geometries
  directed_predictions <- add_geometries(directed_predictions)

  # Ensure data is sf object
  if (!inherits(directed_predictions, "sf")) {
    stop("directed_predictions must be an sf object with geometry")
  }

  # Transform to WGS84 for leaflet
  map_data <- sf::st_transform(directed_predictions, crs = 4326)

  # Select appropriate prediction columns
  pred_col <- if (heavy_vehicle) {
    if (balanced) "balanced_pred_heavy" else "inla_pred_heavy"
  } else {
    if (balanced) "balanced_pred" else "inla_pred"
  }

  sd_col <- if (heavy_vehicle) {
    if (balanced) "balanced_sd_heavy" else "inla_sd_heavy"
  } else {
    if (balanced) "balanced_sd" else "inla_sd"
  }

  last_year_col <- if (heavy_vehicle) {
    "lastYearAadt_heavyAadt"
  } else {
    "lastYearAadt_aadt"
  }

  aadt_col <- if (heavy_vehicle) {
    "heavyAadt"
  } else {
    "aadt"
  }

  # Calculate relative uncertainty if not already present
  if (!"relative_uncertainty" %in% names(map_data)) {
    map_data$relative_uncertainty <- map_data[[sd_col]] / map_data[[pred_col]]
  }

  # Create popup text
  map_data$popup_text <- sprintf(
    "<strong>Link ID:</strong> %s<br>
    <strong>Predicted AADT:</strong> %s<br>
    <strong>Standard deviation:</strong> %s<br>
    <strong>Measured:</strong> %s (%s)<br>
    <strong>Relative uncertainty:</strong> %.2f<br>
    <strong>Last year AADT:</strong> %s<br>
    <strong>%s:</strong> %s",
    map_data$id,
    format(round(map_data[[pred_col]]), big.mark = " ", scientific = FALSE),
    format(round(map_data[[sd_col]]), big.mark = " ", scientific = FALSE),
    format(round(map_data[[aadt_col]]), big.mark = " "), map_data[["traffic_volume_source"]],
    map_data$relative_uncertainty,
    format(round(map_data[[last_year_col]]), big.mark = " "),
    color_by,
    map_data[[color_by]]
  )

  # Determine if color_by is categorical or continuous
  is_categorical <- is.factor(map_data[[color_by]]) ||
    is.character(map_data[[color_by]]) ||
    length(unique(map_data[[color_by]])) <= 10

  # Create color palette
  # Create color palette
  if (is_categorical) {
    # For categorical variables
    unique_values <- unique(map_data[[color_by]])
    # Handle ordered factors for uncertainty categories
    if (color_by == "uncertainty_category" ||
        all(c("low", "medium", "high") %in% tolower(as.character(unique_values)))) {
      # Order levels appropriately
      ordered_levels <- c("low", "medium", "high")
      map_data[[color_by]] <- factor(
        as.character(map_data[[color_by]]),
        levels = ordered_levels
      )
      pal <- leaflet::colorFactor(
        palette = c("green", "orange", "red"),
        domain = ordered_levels,
        levels = ordered_levels
      )
    } else {
      pal <- leaflet::colorFactor(
        palette = palette,
        domain = unique_values
      )
    }
  } else {
    # For continuous variables
    # Reverse palette if it's RdYlGn so low values are green
    if (palette == "RdYlGn") {
      pal <- leaflet::colorNumeric(
        palette = palette,
        domain = map_data[[color_by]],
        reverse = TRUE
      )
    } else {
      pal <- leaflet::colorNumeric(
        palette = palette,
        domain = map_data[[color_by]]
      )
    }
  }

  nvdb <- nvdb_objects()

  # Create leaflet map
  m <- leaflet::leaflet(
    map_data,
    options = leaflet::leafletOptions(crs = nvdb$nvdb_crs, zoomControl = TRUE)) |>
    leaflet::addTiles(urlTemplate = nvdb$nvdb_url,
                      attribution = nvdb$nvdb_attribution) |>
    leaflet::addPolylines(
      color = ~pal(map_data[[color_by]]),
      weight = weight,
      opacity = opacity,
      popup = ~popup_text,
      #label = ~as.character(id),
      highlightOptions = leaflet::highlightOptions(
        weight = weight + 2,
        color = "white",
        bringToFront = TRUE
      )
    ) |>
    leaflet::addLegend(
      position = "bottomright",
      pal = pal,
      values = ~map_data[[color_by]],
      title = color_by,
      opacity = opacity
    )

  return(m)
}
