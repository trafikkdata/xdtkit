#' Calculate bus-based AADT estimates
#'
#' Converts bus stop counts into AADT estimates on traffic links with uncertainty.
#'
#' @param stops_on_traffic_links_data Data frame linking bus stops to traffic links
#' @param bus_counts_data Data frame with bus counts by stop (columns: stopPointRef, no_of_buses)
#' @param year The year the data were collected
#'
#' @return Data frame with columns: id, stopPointRef, bus_aadt, bus_sd, stopCertainty
#' @export
calculate_bus_aadt <- function(stops_on_traffic_links_data,
                               bus_counts_data,
                               year) {

  # Find number of days in year
  date_common <- as.Date(paste0(year, "-01-01"))
  days_in_year <- Hmisc::yearDays(date_common)

  # Check if stopAggregatesDirection exists
  if(!("stopAggregatesDirection" %in% colnames(stops_on_traffic_links_data))){
    stops_on_traffic_links_data$stopAggregatesDirections = NA
  }

  # Prepare stop-to-link mapping (handle multiple stops per link)
  stops_expanded <- stops_on_traffic_links_data |>
    dplyr::select(id, stopPointRef, stopCertainty,
                  stopsServeDifferentBuses, stopAggregatesDirections) |>
    dplyr::mutate(stopPointRef = strsplit(stopPointRef, ", ")) |>
    tidyr::unnest(cols = stopPointRef) |>
    dplyr::mutate(stopPointRef = trimws(stopPointRef)) |>
    tidyr::drop_na(stopPointRef)

  # Join with bus counts and aggregate to link level
  bus_aadt <- stops_expanded |>
    dplyr::left_join(bus_counts_data, by = "stopPointRef") |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      stopCertainty = dplyr::first(stopCertainty),
      stopsServeDifferentBuses = dplyr::first(stopsServeDifferentBuses),
      stopAggregatesDirections = dplyr::first(stopAggregatesDirections),
      stopPointRef = dplyr::first(stopPointRef),
      n_stops = dplyr::n(),
      total_buses = sum(no_of_buses, na.rm = TRUE),
      mean_buses = mean(no_of_buses, na.rm = TRUE),
      #sd_buses = if (dplyr::n() > 1) stats::sd(no_of_buses, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Determine aggregation method
      bus_aadt = calculate_aggregated_bus_aadt(
        stopsServeDifferentBuses, stopAggregatesDirections, total_buses, mean_buses, days_in_year
      )#,
      # Calculate temporal uncertainty
      #bus_sd = calculate_bus_uncertainty(
      #  stopsServeDifferentBuses, stopAggregatesDirections, n_stops, sd_buses, bus_aadt,
      #  days_in_year, cv_uncertainty
      #)
    ) |>
    # Add location uncertainty
    #add_location_uncertainty(location_uncertainties) |>
    # Remove locations with NaN AADT
    dplyr::filter(!is.nan(bus_aadt)) |>
    dplyr::select(id, stopPointRef, bus_aadt, stopCertainty) |>
    dplyr::mutate(bus_aadt = round(bus_aadt),
                  traffic_volume_source = "Bus",
                  traffic_volume_year = year)

  return(bus_aadt)
}


#' Calculate aggregated AADT from bus counts
#'
#' Determines whether to sum or average bus counts based on stop configuration,
#' and adjusts for directional aggregation when needed.
#'
#' @param stopsServeDifferentBuses Character: "TRUE", "FALSE", or NA
#' @param stopAggregatesDirections Logical: whether count includes both directions
#' @param total_buses Total buses across all stops on the link
#' @param mean_buses Mean buses per stop on the link
#' @param days_in_year Number of collection days
#' @return Daily average traffic volume (directional)
calculate_aggregated_bus_aadt <- function(stopsServeDifferentBuses,
                                      stopAggregatesDirections,
                                      total_buses,
                                      mean_buses,
                                      days_in_year) {
  # Calculate based on stop configuration
  aadt <- dplyr::case_when(
    # Single stop or rural (same buses): use mean
    is.na(stopsServeDifferentBuses) | stopsServeDifferentBuses %in% FALSE ~
      mean_buses / days_in_year,
    # Terminal (different buses): sum all stops
    stopsServeDifferentBuses %in% TRUE ~
      total_buses / days_in_year
  )

  # Adjust for bidirectional counts
  dplyr::if_else(stopAggregatesDirections %in% TRUE, aadt / 2, aadt)
}


#' Calculate uncertainty for bus AADT
#'
#' Estimates uncertainty from variation between bus stops and sample size,
#' adjusting for directional aggregation when needed.
#'
#' @param stopsServeDifferentBuses Character: "TRUE", "FALSE", or NA
#' @param stopAggregatesDirections Logical: whether count includes both directions
#' @param n_stops Number of stops on the link
#' @param sd_buses Standard deviation of bus counts across stops
#' @param bus_aadt Estimated AADT value
#' @param days_in_year Number of collection days
#' @param cv_uncertainty Coefficient of variation fallback
#' @return Standard deviation estimate
calculate_bus_uncertainty <- function(stopsServeDifferentBuses,
                                      stopAggregatesDirections,
                                      n_stops,
                                      sd_buses,
                                      bus_aadt,
                                      days_in_year,
                                      cv_uncertainty) {
  # Calculate uncertainty based on stop configuration
  uncertainty <- dplyr::case_when(
    # Single stop: CV method
    is.na(stopsServeDifferentBuses) ~
      cv_uncertainty * bus_aadt / days_in_year,
    # Rural: use empirical SD if available, else CV
    stopsServeDifferentBuses %in% FALSE ~
      dplyr::coalesce(sd_buses, cv_uncertainty * bus_aadt) / days_in_year,
    # Terminal: error propagation for sum
    stopsServeDifferentBuses %in% TRUE ~
      cv_uncertainty * sqrt(n_stops) * bus_aadt / days_in_year
  )

  # Adjust for bidirectional counts (dividing by 2 scales SD by 1/2)
  dplyr::if_else(stopAggregatesDirections %in% TRUE, uncertainty / 2, uncertainty)
}



#' Filter bus AADT by certainty level
#'
#' Keeps only bus estimates meeting minimum certainty threshold.
#'
#' @param bus_aadt_data Data frame with stopCertainty column
#' @param lowest_certainty Minimum level: "Low", "Medium", "High", or "Only_on_link"
#' @return Filtered data frame
filter_by_certainty <- function(bus_aadt_data, lowest_certainty) {
  certainty_levels <- c("Low", "Medium", "High", "Only_on_link")

  if (!lowest_certainty %in% certainty_levels) {
    stop("lowest_certainty must be one of: ", paste(certainty_levels, collapse = ", "))
  }

  # Create ordered factor
  certainties_to_include <- factor(
    certainty_levels[match(lowest_certainty, certainty_levels):length(certainty_levels)],
    levels = certainty_levels,
    ordered = TRUE
  )

  bus_aadt_data |>
    dplyr::filter(
      is.na(stopCertainty) |
        stopCertainty == "" |
        stopCertainty %in% certainties_to_include
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Join bus stop counts to traffic link data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Join bus stop counts to traffic link data
#'
#' Supplements traffic link AADT values with bus stop counts where traffic data
#' is missing. Existing traffic link AADT values are never overwritten. Updates
#' traffic_volume_source and traffic_volume_year metadata only when bus data
#' fills a gap.
#'
#' @param df A data frame containing traffic link data with columns:
#'   \describe{
#'     \item{id}{Link identifier}
#'     \item{parentTrafficLinkId}{Parent link identifier}
#'     \item{aadt}{Annual Average Daily Traffic (may contain NAs)}
#'     \item{traffic_volume_source}{Source of traffic volume data}
#'     \item{traffic_volume_year}{Year of traffic volume measurement}
#'   }
#' @param bus_data A data frame containing bus stop count data with columns:
#'   \describe{
#'     \item{id}{Link identifier (matches df$id)}
#'     \item{bus_aadt}{Bus-derived AADT estimate}
#'     \item{bus_sd}{Standard deviation of bus AADT estimate}
#'     \item{stopPointRef}{Bus stop reference identifier}
#'     \item{stopCertainty}{Certainty/quality metric for bus estimate}
#'     \item{traffic_volume_source}{Source identifier for bus data}
#'     \item{traffic_volume_year}{Year of bus count data}
#'   }
#'
#' @return A data frame with the same structure as df, with additional
#'   columns from bus_data (bus_sd, stopPointRef, stopCertainty). Missing AADT
#'   values are filled with bus_aadt where available, and metadata columns
#'   (traffic_volume_source, traffic_volume_year) are updated accordingly.
#'
#' @details
#' The function prioritizes existing traffic link AADT values over bus estimates.
#' Bus data is only used to fill gaps (NA values) in the traffic link data.
#' When bus data is used, the traffic_volume_source and traffic_volume_year are
#' updated to reflect the bus data source. Bus-specific columns (bus_sd,
#' stopPointRef, stopCertainty) are retained for all links where bus data exists,
#' regardless of whether the bus AADT was used.
#'
#' @examples
#' # Create example traffic data with some missing AADT values
#' df <- data.frame(
#'   id = c("L001", "L002", "L003", "L004"),
#'   parentTrafficLinkId = c("P001", "P001", "P002", "P002"),
#'   aadt = c(5000, NA, 3000, NA),
#'   heavyAadt = c(500, NA, NA, NA),
#'   traffic_volume_source = c("continuous", NA, "periodic", NA),
#'   traffic_volume_year = c(2024, NA, 2024, NA)
#' )
#'
#' # Create example bus data
#' bus_data <- data.frame(
#'   id = c("L002", "L003", "L004"),
#'   parentTrafficLinkId = c("P001", "P002", "P002"),
#'   stopPointRef = c("NSR:12345", "NSR:67890", "NSR:11111"),
#'   bus_aadt = c(1200, 2500, 800),
#'   bus_sd = c(150, 200, 100),
#'   stopCertainty = c("high", "medium", "high"),
#'   traffic_volume_source = c("bus", "bus", "bus"),
#'   traffic_volume_year = c(2024, 2024, 2024)
#' )
#'
#' # Join data - L002 and L004 will get bus AADT, L003 keeps original
#' result <- join_bus_to_traffic(df, bus_data)
#'
#' # Check results
#' print(result)
#' # L001: aadt=5000 (original, no bus data available)
#' # L002: aadt=1200 (filled from bus), source="bus"
#' # L003: aadt=3000 (original kept, bus data ignored)
#' # L004: aadt=800 (filled from bus), source="bus"
#'
#' @export
join_bus_to_traffic <- function(df, bus_data) {
  # Identify which traffic links have missing AADT
  df <- df |>
    dplyr::mutate(.had_aadt = !is.na(aadt))

  # Join bus data
  result <- df |>
    dplyr::left_join(
      bus_data |>
        dplyr::select(id, bus_aadt, stopPointRef, stopCertainty,
                      bus_source = traffic_volume_source,
                      bus_year = traffic_volume_year),
      by = "id"
    ) |>
    dplyr::mutate(
      # Fill in AADT with bus data only where original was missing
      aadt = dplyr::coalesce(aadt, bus_aadt),
      heavyAadt = dplyr::coalesce(heavyAadt, bus_aadt),
      # Update metadata only when bus data fills a gap
      traffic_volume_source = dplyr::if_else(!.had_aadt & !is.na(bus_aadt),
                                             bus_source,
                                             traffic_volume_source),
      traffic_volume_year = dplyr::if_else(!.had_aadt & !is.na(bus_aadt),
                                           bus_year,
                                           traffic_volume_year)
    ) |>
    dplyr::select(-bus_aadt, -bus_source, -bus_year, -.had_aadt)

  return(result)
}
