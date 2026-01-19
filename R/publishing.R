# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Upload AADT predictions to GitHub release (year-versioned) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Upload AADT predictions to GitHub release
#'
#' Uploads a data frame containing AADT model predictions to a GitHub release
#' as a JSON file. The function creates a year-based release (e.g., tag "2025")
#' and uploads the data as "aadt_model_result.json". Existing releases can be
#' overwritten, and test releases can be marked as pre-releases.
#'
#' @param df A data frame containing AADT model predictions to upload.
#' @param year Integer. The year for the release tag. If NULL (default), uses
#'   the current year.
#' @param overwrite Logical. If TRUE, allows overwriting an existing release
#'   for the specified year. If FALSE (default) and a release already exists,
#'   the function will stop with an error. Default is FALSE.
#' @param prerelease Logical. If TRUE, marks the release as a pre-release,
#'   which prevents it from being shown as the "latest" release on GitHub.
#'   Useful for test releases. Default is FALSE.
#'
#' @return Invisibly returns TRUE if successful. Prints status messages during
#'   execution.
#'
#' @details
#' The function requires a GitHub personal access token stored in the
#' environment variable `TRAFIKKDATA_GH_PKG_TOKEN`. This can be set in your
#' `.Renviron` file using `usethis::edit_r_environ()`.
#'
#' The function performs the following steps:
#' \enumerate{
#'   \item Checks if a release with the specified year tag already exists
#'   \item If it exists and `overwrite = TRUE`, deletes the old release and tag
#'   \item Creates a new release with the year as the tag
#'   \item Uploads the data frame as "aadt_model_result.json"
#'   \item Verifies the release is published (not draft)
#' }
#'
#' The uploaded file will be available at:
#' \code{https://github.com/trafikkdata/adt-model-results/releases/download/{year}/aadt_model_result.json}
#'
#' @section GitHub Token Setup:
#' To set up your GitHub token:
#' \enumerate{
#'   \item Run `usethis::edit_r_environ()`
#'   \item Add the line: `TRAFIKKDATA_GH_PKG_TOKEN=your_token_here`
#'   \item Save the file and restart R
#'   \item Verify with: `Sys.getenv("TRAFIKKDATA_GH_PKG_TOKEN")`
#' }
#'
#' @examples
#' \dontrun{
#' # Create test release (marked as pre-release)
#' upload_df_to_github_release(test_data, year = 9999,
#'                              prerelease = TRUE, overwrite = TRUE)
#'
#' # Upload production results for 2025
#' upload_df_to_github_release(aadt_results_2025, year = 2025)
#'
#' # Update existing 2025 release with revised predictions
#' upload_df_to_github_release(aadt_results_2025_v2, year = 2025,
#'                              overwrite = TRUE)
#' }
#'
#' @importFrom httr GET POST DELETE PATCH add_headers status_code content
#'   timeout upload_file
#' @importFrom jsonlite toJSON write_json
#'
#' @export
upload_df_to_github_release <- function(df, year = NULL, overwrite = FALSE,
                                        prerelease = FALSE) {

  # Determine year if not provided
  if (is.null(year)) {
    year <- as.integer(format(Sys.Date(), "%Y"))
    message("No year specified, using current year: ", year)
  }

  # Load GitHub token from environment variable
  github_token <- Sys.getenv("TRAFIKKDATA_GH_PKG_TOKEN")
  if (github_token == "") {
    stop("GitHub token not found. Make sure TRAFIKKDATA_GH_PKG_TOKEN is set in your environment.")
  }

  # Define repository and release details
  repo_owner <- "trafikkdata"
  repo_name <- "adt-model-results"
  release_tag <- as.character(year)  # Just the year as the tag
  release_name <- sprintf("AADT Model Results %d", year)

  # Add prerelease indicator to name and body
  if (prerelease) {
    release_name <- sprintf("%s (TEST)", release_name)
    release_body <- sprintf("TEST RELEASE - AADT model predictions for year %d.", year)
  } else {
    release_body <- sprintf("AADT model predictions for year %d.", year)
  }

  file_path <- "aadt_model_result.json"  # Fixed filename

  # Save DataFrame to JSON file
  df <- dplyr::select(df,
                      directedTrafficLinkId = id,
                      estimatedAadt, estimatedAadtStandardDeviation,
                      estimatedAadtHeavy, estimatedAadtHeavyStandardDeviation)
  jsonlite::write_json(df, file_path, auto_unbox = TRUE, pretty = TRUE)

  # Define headers for API requests
  headers <- add_headers(
    Authorization = paste("token", github_token),
    Accept = "application/vnd.github.v3+json"
  )

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 1: Check if release exists ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  release_url <- sprintf(
    "https://api.github.com/repos/%s/%s/releases/tags/%s",
    repo_owner, repo_name, release_tag
  )

  release_response <- GET(release_url, headers, timeout(60))

  if (status_code(release_response) == 200) {
    if (!overwrite) {
      stop(sprintf("Release for year %d already exists. Set overwrite=TRUE to replace it.", year))
    }

    release_data <- content(release_response)
    release_id <- release_data$id

    message(sprintf("Release for %d exists. Overwriting...", year))

    # Delete the existing release
    delete_release_url <- sprintf(
      "https://api.github.com/repos/%s/%s/releases/%s",
      repo_owner, repo_name, release_id
    )

    delete_release_response <- DELETE(delete_release_url, headers, timeout(60))

    if (status_code(delete_release_response) != 204) {
      warning("Failed to delete release: ", content(delete_release_response, "text"))
    } else {
      message("Existing release deleted successfully")
    }

    # Delete the tag
    delete_tag_url <- sprintf(
      "https://api.github.com/repos/%s/%s/git/refs/tags/%s",
      repo_owner, repo_name, release_tag
    )

    delete_tag_response <- DELETE(delete_tag_url, headers, timeout(60))

    if (status_code(delete_tag_response) == 204) {
      message("Tag deleted successfully")
    }
  } else {
    message(sprintf("Creating new release for year %d", year))
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 2: Create new release ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  release_payload <- list(
    tag_name = release_tag,
    name = release_name,
    body = release_body,
    draft = FALSE,
    prerelease = prerelease  # Marks as pre-release for test versions
  )

  create_release_url <- sprintf(
    "https://api.github.com/repos/%s/%s/releases",
    repo_owner, repo_name
  )

  create_release_response <- POST(
    create_release_url,
    headers,
    body = toJSON(release_payload, auto_unbox = TRUE),
    encode = "json",
    timeout(60)
  )

  if (status_code(create_release_response) != 201) {
    stop("Failed to create release: ", content(create_release_response, "text"))
  }

  release <- content(create_release_response)
  message("Release created successfully with ID: ", release$id)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Step 3: Upload file to release ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Get upload URL (remove template parameters)
  upload_url <- sub("\\{\\?name,label\\}", "", release$upload_url)

  # Upload the JSON file
  upload_response <- POST(
    upload_url,
    add_headers(
      Authorization = paste("token", github_token),
      Accept = "application/vnd.github.v3+json",
      `Content-Type` = "application/octet-stream"
    ),
    query = list(name = basename(file_path)),
    body = upload_file(file_path),
    timeout(60)
  )

  if (status_code(upload_response) == 201) {
    message("File uploaded successfully!")

    # Verify release is published (not draft)
    release_check_url <- sprintf(
      "https://api.github.com/repos/%s/%s/releases/%s",
      repo_owner, repo_name, release$id
    )

    release_check_response <- GET(release_check_url, headers, timeout(60))

    if (status_code(release_check_response) == 200) {
      release_check_data <- content(release_check_response)

      if (isTRUE(release_check_data$draft)) {
        # Update to remove draft status
        update_response <- PATCH(
          release_check_url,
          headers,
          body = toJSON(release_payload, auto_unbox = TRUE),
          encode = "json",
          timeout(60)
        )

        if (status_code(update_response) != 200) {
          warning("Failed to update release status: ", content(update_response, "text"))
        } else {
          message("Release published successfully")
        }
      }
    }
  } else {
    stop("Failed to upload file: ", content(upload_response, "text"))
  }

  # Clean up local JSON file (optional)
  # unlink(file_path)

  if (prerelease) {
    message("\n*** TEST RELEASE CREATED ***")
    message("This is marked as a pre-release and won't be used by external scripts.")
  }

  message(sprintf("\nRelease URL: https://github.com/%s/%s/releases/tag/%s",
                  repo_owner, repo_name, release_tag))
  message(sprintf("Download URL: https://github.com/%s/%s/releases/download/%s/%s",
                  repo_owner, repo_name, release_tag, file_path))

  invisible(TRUE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Usage examples ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # Testing (marked as pre-release, won't be picked up by external script):
# # Tag: "9999", URL: .../releases/download/9999/aadt_model_result.json
# upload_df_to_github_release(test_data, year = 9999, prerelease = TRUE, overwrite = TRUE)
#
# # Production release for 2025 (this becomes the "latest" release):
# # Tag: "2025", URL: .../releases/download/2025/aadt_model_result.json
# upload_df_to_github_release(aadt_results_2025, year = 2025, prerelease = FALSE)
#
# # Production release for 2024:
# # Tag: "2024", URL: .../releases/download/2024/aadt_model_result.json
# upload_df_to_github_release(aadt_results_2024, year = 2024, prerelease = FALSE)
