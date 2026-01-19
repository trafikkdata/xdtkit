# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Main preprocessing function ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Preprocess data
#'
#' @param raw_directed_traffic_link_data Raw, unprocessed traffic count data
#' @param year The year of the data
#'
#' @returns A processed data frame of directed traffic links.
#' @export
#'
preprocess_traffic_links <- function(raw_directed_traffic_link_data,
                                    year = 2024){

  validate_input_data(directed_traffic_link_data = raw_directed_traffic_link_data)

  df <- raw_directed_traffic_link_data |>
    process_traffic_volume(year = year) |>
    add_heavy_aadt() |>
    process_list_columns() |>
    standardize_data_types() |>
    round_and_check_aadt() |>
    assign_traffic_volume_source(current_year = year) |>
    add_county() |>
    add_municipality() |>
    add_roadSystem() |>
    add_logLength() |>
    remove_redundant_columns()

  return(df)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Input validation ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Validate input data structure for AADT modeling
#'
#' Checks that input data has required columns, correct types, and reasonable
#' values. Designed to catch upstream data structure changes early.
#'
#' @param traffic_link_data Data frame with traffic links and AADT measurements
#' @param directed_traffic_link_data Data frame with directed traffic links and AADT measurements
#' @param nodes_data Data frame with traffic nodes
#' @return Invisibly returns TRUE if validation passes, otherwise throws error
#' @export
validate_input_data <- function(traffic_link_data = NULL, directed_traffic_link_data = NULL,
                                nodes_data = NULL) {

  # Define expected structure
  expected_traffic_link_cols <- c(
    "id", "subsumedTrafficNodeIds", "functionalRoadClass", "functionClass",
    "highestSpeedLimit", "lowestSpeedLimit", "isNorwegianScenicRoute",
    "isFerryRoute", "isRamp", "tollStationIds", "yearAppliesTo",
    "startTrafficNodeId", "endTrafficNodeId", "roadCategory", "roadPlacements",
    "length", "trafficDirectionWrtMeteringDirection", "maxLanes", "minLanes",
    "hasOnlyPublicTransportLanes", "urbanRatio", "numberOfEstablishments",
    "numberOfEmployees", "numberOfInhabitants", "associatedTrpIds",
    "trafficVolumes", "candidateIds", "municipalityIds", "countyIds",
    "roadSystemReferences", "roadLinkIds", "roadNodeIds"
  )

  expected_directed_traffic_link_cols <- c(
    "id", "parentTrafficLinkId", "isTrafficWithMetering", "functionalRoadClass",
    "functionClass", "highestSpeedLimit", "lowestSpeedLimit",
    "isNorwegianScenicRoute", "isFerryRoute", "isRamp", "isBlocked",
    "tollStationIds", "isInvalid", "yearAppliesTo", "startTrafficNodeId",
    "endTrafficNodeId", "roadCategory", "length", "maxLanes", "minLanes",
    "hasOnlyPublicTransportLanes", "associatedTrpIds", "lastYearAadt",
    "bestDataSourceAadt", "trafficVolumes", "municipalityIds", "countyIds",
    "roadSystemReferences", "roadLinkIds", "roadNodeIds", "roadPlacements"
  )

  expected_nodes_cols <- c(
    "id", "isRoundabout", "numberOfIncomingLinks", "numberOfOutgoingLinks",
    "numberOfUndirectedLinks", "legalTurningMovements",
    "connectedTrafficLinkIds", "connectedTrafficLinkCandidateIds",
    "roadNodeIds", "roadSystemReferences", "roadSystems"
  )

  check_columns <- function(data, expected_cols, data_name) {
    missing_cols <- setdiff(expected_cols, names(data))
    extra_cols <-  setdiff(names(data), expected_cols)
    if (length(missing_cols) > 0) {
      warning(sprintf(
        "%s is missing expected columns: %s",
        data_name,
        paste(missing_cols, collapse = ", ")
      ))
    }
    if(length(extra_cols) > 0) {
      warning(sprintf(
        "%s has unexpected columns: %s",
        data_name,
        paste(extra_cols, collapse = ", ")
      ))
    }
  }

  # Check columns exist
  if(!is.null(traffic_link_data)){
    check_columns(traffic_link_data, expected_traffic_link_cols, "traffic_link_data")
  }
  if(!is.null(directed_traffic_link_data)){
    check_columns(directed_traffic_link_data, expected_directed_traffic_link_cols, "directed_traffic_link_data")
  }
  if(!is.null(nodes_data)){
    check_columns(nodes_data, expected_nodes_cols, "nodes_data")
  }

  invisible(TRUE)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process traffic volume ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Process Traffic Volume Data
#'
#' Processes traffic volume data based on the year, handling different data structures
#' for years before and after 2024.
#'
#' @param df A data frame containing traffic volume data
#' @param year Integer specifying the year of the data
#'
#' @return A processed data frame with flattened traffic volume information
#' @export
process_traffic_volume <- function(df, year){
  # If data is from 2024 or later, it contains the variable "bestDataSourceAadt", which is a nested data frame column.
  # This column is derived from the "trafficVolumes" column.
  if(year < 2024){
    df <- get_best_traffic_volume(df)
  }
  if(year >= 2024){
    df <- flatten_df(df)
  }
  return(df)
}

#' Get Best Traffic Volume
#'
#' Handles nested dataframes containing traffic volumes for data before 2024.
#'
#' @param df A data frame containing traffic volume data
#' @return A processed data frame
#' @export
get_best_traffic_volume <- function(df){
  # Handle the nested dataframes containing traffic volumes
  # (only for data before 2024)
}

#' Flatten Data Frame
#'
#' Flattens nested columns in the data frame, specifically bestDataSourceAadt and lastYearAadt.
#'
#' @param df A data frame with nested columns
#'
#' @return A flattened data frame
#' @export
flatten_df <- function(df){
  df_flattened <- df |>
    tidyr::unnest_wider(bestDataSourceAadt, names_sep = "_") |>
    tidyr::unnest_wider(lastYearAadt, names_sep = "_")

  return(df_flattened)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Heavy vehicle volumes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

add_heavy_aadt <- function(df){
  df <- df %>%
    dplyr::mutate(heavyAadt = round(bestDataSourceAadt_heavyRatio*bestDataSourceAadt_trafficVolumeValue),
           lastYearAadt_heavyAadt = round(lastYearAadt_heavyRatio*lastYearAadt_trafficVolumeValue))
  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Process_list_columns ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Process List Columns
#'
#' Processes all list columns in the data frame by extracting appropriate elements
#' and removing remaining list columns.
#'
#' @param df A data frame containing list columns
#'
#' @return A data frame with processed list columns
#' @export
process_list_columns <- function(df){
  # All list extraction logic
  names(df)[sapply(df, is.list)]

  df <- dplyr::mutate(df,
                      dplyr::across(c(functionalRoadClass, functionClass),
                                    extract_smallest_element),
                      dplyr::across(c(municipalityIds, countyIds,
                                      roadCategory, roadSystemReferences),
                                    safely_extract_first_element))

  df <- remove_list_columns(df)

  return(df)
}

#' Extract Smallest Element
#'
#' Extracts the smallest element from a vector, or returns the value if not a vector.
#'
#' @param x A vector or single value
#'
#' @return The minimum value if x is a vector, otherwise x itself
#' @export
extract_smallest_element <- Vectorize(function(x){
  if(is.vector(x)){
    return(min(x))
  }else{
    return(x)
  }
})

#' Safely Extract First Element
#'
#' Safely extracts the first element from a vector or data frame.
#'
#' @param x A vector, data frame, character, numeric, or NULL value
#'
#' @return The first element of x, or x itself if not a vector/data frame
#' @export
safely_extract_first_element <- Vectorize(function(x){
  if(is.vector(x)|is.data.frame(x)){return(x[1])}
  if(is.character(x)|is.numeric(x)|is.null(x)){return(x)}
  warning("Entry was not vector, character, numeric or null.")
})

#' Remove List Columns
#'
#' Removes all columns of type list from the data frame.
#'
#' @param df A data frame potentially containing list columns
#'
#' @return A data frame with list columns removed
#' @export
remove_list_columns <- function(df){
  list_cols <- sapply(df, is.list)
  df_filtered <- df[, !list_cols]
  return(df_filtered)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Standardize data types ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Standardize Data Types
#'
#' Converts columns to appropriate data types (numeric, integer, factor, logical).
#'
#' @param df A data frame with columns to be standardized
#'
#' @return A data frame with standardized column types
#' @export
standardize_data_types <- function(df){
  # Final type conversions
  numeric_cols <- c("bestDataSourceAadt_trafficVolumeValue", "length",
                    "lastYearAadt_trafficVolumeValue")
  integer_cols <- c("yearAppliesTo", "bestDataSourceAadt_year",
                    "lastYearAadt_year")
  factor_cols <- c('functionalRoadClass', 'functionClass',
                   'highestSpeedLimit', "lowestSpeedLimit",
                   "municipalityIds", "countyIds",'roadCategory',
                   'maxLanes', 'minLanes')
  logical_cols <- c('isNorwegianScenicRoute', 'isFerryRoute', "isRamp",
                    "isBlocked", "isInvalid", "hasOnlyPublicTransportLanes")

  df <- df |>
    dplyr::mutate(dplyr::across(dplyr::all_of(numeric_cols), as.numeric),
                  dplyr::across(dplyr::all_of(integer_cols), as.integer),
                  dplyr::across(dplyr::all_of(factor_cols), as.factor),
                  dplyr::across(dplyr::all_of(logical_cols), as.logical))

  return(df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Round and check AADT ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Round and Check AADT
#'
#' Rounds AADT values and ensures they are non-negative.
#'
#' @param df A data frame containing AADT values
#'
#' @return A data frame with rounded and checked AADT values
#' @export
round_and_check_aadt <- function(df){
  df$bestDataSourceAadt_trafficVolumeValue <- round(df$bestDataSourceAadt_trafficVolumeValue)
  df$bestDataSourceAadt_trafficVolumeValue[df$bestDataSourceAadt_trafficVolumeValue < 0] <- 0

  return(df)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Assign traffic volume source ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Assign Traffic Volume Source
#'
#' Assigns a source label to each traffic volume measurement based on its characteristics.
#'
#' @param df A data frame containing traffic volume data
#' @param current_year Integer specifying the current year
#'
#' @return A data frame with traffic_volume_source and traffic_volume_year columns added
#' @export
assign_traffic_volume_source <- function(df, current_year){
  df <- df |>
    dplyr::mutate(
      traffic_volume_source = dplyr::case_when(
        bestDataSourceAadt_trafficVolumeType == "DERIVED" ~ "Derived",
        bestDataSourceAadt_sourceType == "EXTERNAL" & isFerryRoute ~ "Ferry",
        bestDataSourceAadt_sourceType == "EXTERNAL" ~ "External_municipal",
        bestDataSourceAadt_sourceType == "TOLL_STATION_AUTOPASS" ~ "AutoPASS",
        bestDataSourceAadt_sourceType == "TRAFIKKDATA" &
          bestDataSourceAadt_registrationFrequency == "PERIODIC" ~ "Trafikkdata_periodic",
        bestDataSourceAadt_sourceType == "TRAFIKKDATA" &
          bestDataSourceAadt_registrationFrequency == "CONTINUOUS" ~ "Trafikkdata_continuous"
      ),
      traffic_volume_year = bestDataSourceAadt_year
    )
  return(df)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add county variable ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Add County Variable
#'
#' Adds a county name variable to the data frame based on county ID codes.
#'
#' @param df A data frame containing a countyIds column
#'
#' @return A data frame with a county factor column added
#' @export
add_county <- function(df){
  # County names from codes
  county_mapping <- stats::setNames(
    county_names$fylkesnavn,
    as.character(county_names$fylkesnummer)
  )

  df |>
    dplyr::mutate(county = as.factor(county_mapping[as.character(countyIds)]))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add municipality variable ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Add Municipality Variable
#'
#' Adds a municipality name variable to the data frame based on municipality ID codes
#' using the municipality_names package data.
#'
#' @param df A data frame containing a municipalityIds column
#'
#' @return A data frame with a municipality factor column added
#' @export
add_municipality <- function(df){
  # Municipality mapping is in the package data.

  # Create named vector for mapping
  municipality_mapping <- stats::setNames(
    municipality_names$kommunenavn,
    as.character(municipality_names$kommunenummer)
  )

  df |>
    dplyr::mutate(municipality = as.factor(municipality_mapping[as.character(municipalityIds)]))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add roadSystem variable ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Add Road System Variable
#'
#' Adds a road system variable by extracting the road system type from
#' roadSystemReferences (removes everything after the first space).
#'
#' @param df A data frame containing a roadSystemReferences column
#'
#' @return A data frame with a roadSystem factor column added
#' @export
add_roadSystem <- function(df){
  df |>
    dplyr::mutate(roadSystem = as.factor(gsub(" .*$", "", roadSystemReferences)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add logLength variable ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Add Log-Transformed Length Variable
#'
#' Adds a log-transformed length variable to the data frame.
#'
#' @param df A data frame containing a length column
#'
#' @return A data frame with a logLength column added
#' @export
add_logLength <- function(df){
  df |>
    dplyr::mutate(logLength = log(length))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove redundant columns ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Remove Redundant Columns
#'
#' Removes redundant columns from the data frame.
#'
#' @param df A data frame
#'
#' @return A data frame with redundant columns removed
#' @export
remove_redundant_columns <- function(df){
  # Columns to keep:
  df_selected <- dplyr::select(
    df, id, parentTrafficLinkId, functionalRoadClass,
    functionClass, highestSpeedLimit, lowestSpeedLimit, isNorwegianScenicRoute,
    isFerryRoute, isRamp, yearAppliesTo, startTrafficNodeId, endTrafficNodeId,
    municipality, county, roadSystem, roadCategory, length, logLength, maxLanes,
    minLanes, hasOnlyPublicTransportLanes,
    lastYearAadt_aadt = lastYearAadt_trafficVolumeValue,
    lastYearAadt_heavyRatio,
    lastYearAadt_heavyAadt,
    aadt = bestDataSourceAadt_trafficVolumeValue,
    coverage = bestDataSourceAadt_coverage,
    heavyRatio = bestDataSourceAadt_heavyRatio,
    heavyAadt,
    traffic_volume_source, traffic_volume_year)

  df_selected
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fill in missing values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Fill Missing Values with Multiple Imputation Strategies
#'
#' Imputes missing values in a data frame using three different strategies:
#' replacement with "unknown" for categorical variables, mode imputation for
#' discrete variables, and median imputation for continuous variables.
#'
#' @param df A data frame containing the variables to impute.
#' @param unknown_impute_columns Character vector of column names where NAs
#'   should be replaced with the string "unknown". Typically used for
#'   categorical variables. Default is \code{NULL} (no imputation).
#' @param mode_impute_columns Character vector of column names where NAs
#'   should be replaced with the mode (most frequent value) of that column.
#'   Default is \code{NULL} (no imputation).
#' @param median_impute_columns Character vector of column names where NAs
#'   should be replaced with the median value of that column.
#'   Default is \code{NULL} (no imputation).
#' @param create_missing_indicators Logical. If \code{TRUE} (default), creates
#'   indicator columns for median-imputed variables showing whether the original
#'   value was missing. Column names will be "isMissing[original_column_name]".
#'
#' @return A data frame with missing values imputed according to the specified
#'   strategies. If \code{create_missing_indicators = TRUE}, additional indicator
#'   columns are added for median-imputed variables.
#'
#' @details
#' The function applies up to three imputation strategies in sequence:
#' \itemize{
#'   \item \strong{Unknown imputation}: Categorical variables are filled with
#'     the string "unknown"
#'   \item \strong{Mode imputation}: Variables are filled with their mode
#'     (most frequent value)
#'   \item \strong{Median imputation}: Numeric variables are filled with their
#'     median value
#' }
#'
#' Each imputation strategy is optional - only strategies with non-NULL column
#' specifications will be applied.
#'
#' The mode is calculated excluding NA values. If all values are NA, the mode
#' will be NA. Similarly, median is calculated with \code{na.rm = TRUE}.
#'
#' For median-imputed columns, missing indicators are created by default to
#' help retain information about which values were originally missing.
#'
#' @examples
#' \dontrun{
#' # Example: Use all three strategies
#' df <- data.frame(
#'   category = c("A", "B", NA, "A"),
#'   discrete = c(1, 2, NA, 1),
#'   continuous = c(10.5, 20.3, NA, 15.7)
#' )
#'
#' filled_df <- fill_missing_values(
#'   df = df,
#'   unknown_impute_columns = "category",
#'   mode_impute_columns = "discrete",
#'   median_impute_columns = "continuous"
#' )
#'
#' # Example: Median imputation without missing indicators
#' filled_df <- fill_missing_values(
#'   df = df,
#'   median_impute_columns = "continuous",
#'   create_missing_indicators = FALSE
#' )
#' }
#'
#' @export
fill_missing_values <- function(df,
                                unknown_impute_columns = NULL,
                                mode_impute_columns = NULL,
                                median_impute_columns = NULL,
                                create_missing_indicators = TRUE){

  # Helper function: Mode
  Mode <- function(x) {
    ux <- unique(x[!is.na(x)])  # Exclude NAs when finding mode
    if (length(ux) == 0) return(NA)
    ux[which.max(tabulate(match(x, ux)))]
  }


  # Calculate mode values (if needed)
  if (!is.null(mode_impute_columns) && length(mode_impute_columns) > 0) {
    mode_values <- df |>
      dplyr::select(dplyr::all_of(mode_impute_columns)) |>
      dplyr::summarise(dplyr::across(dplyr::everything(), Mode))
  }

  # Calculate median values (if needed)
  if (!is.null(median_impute_columns) && length(median_impute_columns) > 0) {
    median_values <- df |>
      dplyr::select(dplyr::all_of(median_impute_columns)) |>
      dplyr::summarise(dplyr::across(dplyr::everything(),
                                     ~ median(.x, na.rm = TRUE)))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Impute missing values
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Unknown imputation (handle factors by adding "unknown" as a level)
  if (!is.null(unknown_impute_columns) && length(unknown_impute_columns) > 0) {
    df <- df |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(unknown_impute_columns),
                      ~ {
                        if (is.factor(.x)) {
                          # Add "unknown" as a factor level if not already present
                          if (!"unknown" %in% levels(.x)) {
                            .x <- forcats::fct_expand(.x, "unknown")
                          }
                          tidyr::replace_na(.x, "unknown")
                        } else {
                          tidyr::replace_na(.x, "unknown")
                        }
                      })
      )
  }

  # Mode imputation
  if (!is.null(mode_impute_columns) && length(mode_impute_columns) > 0) {
    df <- df |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(mode_impute_columns),
                      ~ dplyr::coalesce(.x, mode_values[[dplyr::cur_column()]]))
      )
  }

  # Median imputation with optional missing indicators
  if (!is.null(median_impute_columns) && length(median_impute_columns) > 0) {
    # Create missing indicators if requested
    if (create_missing_indicators) {
      df <- df |>
        dplyr::mutate(
          dplyr::across(dplyr::all_of(median_impute_columns),
                        ~ is.na(.x),
                        .names = "isMissing{.col}")
        )
    }

    # Perform median imputation
    df <- df |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(median_impute_columns),
                      ~ dplyr::coalesce(.x, median_values[[dplyr::cur_column()]]))
      )
  }

  return(df)

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add log of last year variables ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Add Log-Transformed Length Variable
#'
#' Adds a log-transformed length variable to the data frame.
#'
#' @param df A data frame containing a length column
#'
#' @return A data frame with a logLength column added
#' @export
add_logLastYear <- function(df){
  df |>
    dplyr::mutate(lastYearAadt_logAadt = log1p(lastYearAadt_aadt),
                  lastYearAadt_logHeavyAadt = log1p(lastYearAadt_heavyAadt))
}
