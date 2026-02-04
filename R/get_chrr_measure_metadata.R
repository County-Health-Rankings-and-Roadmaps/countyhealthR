#' Get Metadata for a County Health Rankings & Roadmaps measure of health
#'
#' Returns key information about a County Health Rankings & Roadmaps measure,
#' including measure ID, measure name, description, years used, factor name,
#' category name, focus area, and whether the measure is comparable across
#' states and years.
#'
#' @param measure A \code{character} specifying the measure. Can be either
#'   a \code{measure_id} or \code{measure_name}.
#' @param release_year A \code{numeric} specifying the release year for which
#'   to pull the metadata. Returns the most recent release year as default.
#' @return A tibble of measure metadata and prints a readable summary.
#' @export
#' @examples
#' \dontrun{
#' get_chrr_measure_metadata(21, 2024)
#' get_chrr_measure_metadata("Uninsured adults", 2022)
#' get_chrr_measure_metadata("High school graduation", 2025)
#' }
#' @export
get_chrr_measure_metadata <- function(measure, release_year = NULL) {

    # Compute most recent year dynamically
    most_recent <- max(as.integer(names(zenodo_year_records)))

    # If user didn’t specify, use most recent
    if (is.null(release_year)) {
      release_year <- most_recent
    }

  message(paste0("Loading CHR&R measure metadata for release year ", release_year))


  measure_map = get_measure_map()
  # --- Validate release_year dynamically ---
  valid_years <- unique(measure_map$year)
  if (!(release_year %in% valid_years)) {
    stop(
      "Invalid release_year: ", release_year, ". ",
      "Available years are: ", paste(sort(valid_years), collapse = ", "), "."
    )
  }


  metadata = measure_map %>%
    dplyr::filter(year == release_year) %>%
    dplyr::filter(measure_id == measure |
                    stringr::str_to_lower(measure_name) == stringr::str_to_lower(measure)
    ) %>%
    dplyr::slice(1)  # in case multiple matches



  # Print nicely
  if (nrow(metadata) == 0) {
    message(
      "No metadata found for the measure: ", measure, ". ",
      "Use list_chrr_measures(release_year = ", release_year, ") to see available measure_ids and measure_names for ", release_year, "."
    )
    return(dplyr::tibble())  # return empty tibble instead of NULL
  }

  cat("\nMeasure ID:       ", metadata$measure_id, "\n")
  cat("Measure Name:     ", metadata$measure_name, "\n")
  cat("Description:      ", metadata$description, "\n")
  cat("Years Used:       ", paste(metadata$years_used, collapse = ", "), "\n")
  cat("Factor Name:      ", metadata$factor_name, "\n")
  cat("Category Name:    ", metadata$category_name, "\n")
  cat("Focus Area Name:  ", metadata$focus_area_name, "\n")
  cat("Compare across states:", metadata$compare_states_text, "\n")
  cat("Compare across years: ", metadata$compare_years_text, "\n")

  message(print_zenodo_citation(year = release_year))

  # Return metadata as tibble but don't print it
  invisible(metadata)

}
