#' Get Metadata for a County Health Rankings & Roadmaps Measure
#'
#' Returns key information about a County Health Rankings & Roadmaps measure,
#' including measure ID, name, description, years used, factor, category, focus area,
#' and comparability information. This function now also silently returns additional
#' fields such as `display_precision`, `format_type`, and `direction`,
#' which may be useful for formatting or analysis.
#'
#' @param measure A \code{character} specifying the measure. Can be either
#'   a \code{measure_id} or \code{measure_name}.
#' @param release_year A \code{numeric} specifying the release year for which
#'   to pull the metadata. Defaults to the most recent release year if
#'   \code{NULL}.
#' @return A tibble of measure metadata. Key fields are printed as a summary,
#'   but additional fields for formatting and comparability are returned silently.
#'   Returned tibble columns include:
#'   \itemize{
#'     \item \code{year} – Release year
#'     \item \code{measure_id} – Unique measure identifier
#'     \item \code{measure_name} – Name of the measure
#'     \item \code{description} – Measure description
#'     \item \code{years_used} – Years for which the measure is reported
#'     \item \code{factor_name}, \code{category_name}, \code{focus_area_name} – Measure hierarchy
#'     \item \code{direction} – Score orientation: 1 means higher values are worse, -1 means higher values are better
#'     \item \code{display_precision} – Recommended number of decimal places for display
#'     \item \code{format_type} – Suggested format type:
#'       \itemize{
#'         \item 0 = rate
#'         \item 1 = percentage
#'         \item 2 = dollars
#'         \item 3 = ratio
#'         \item 4–9 = internal program-specific codes
#'       }
#'     \item \code{compare_states_text} – User-friendly text about comparability across states
#'     \item \code{compare_years_text} – User-friendly text about comparability across years
#'   }
#' @export
#' @examples
#' \dontrun{
#' # Return metadata (silent fields included)
#' md <- get_chrr_measure_metadata(21, 2024)
#' md$display_precision
#' md$format_type
#' md$direction
#' md$compare_states_text
#' }

get_chrr_measure_metadata <- function(measure, release_year = NULL) {

  .check_internet()
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
  cat("Compare across release years: ", metadata$compare_years_text, "\n")

  message(print_zenodo_citation(year = release_year))

  # Return metadata as tibble but don't print it
  invisible(metadata)

}
