#' Get Metadata for a County Health Rankings & Roadmaps Measure
#'
#' Returns structured metadata for a County Health Rankings & Roadmaps
#' (CHR&R) measure for a given release year. The metadata describes the
#' measure's position within the CHR&R Model of Health, which has evolved over time:
#' releases prior to 2025 follow the legacy model, while 2025 and later
#' releases use the new model of health. Metadata is year-specific, reflecting the
#' measure definitions and groupings used in that release.
#'
#' @param measure A \code{character} specifying the measure. Can be either
#'   a \code{measure_id} or \code{measure_name}.
#' @param release_year A \code{numeric} specifying the release year for which
#'   to pull the metadata. Defaults to the most recent release year if
#'   \code{NULL}.
#' @return
#' A tibble (class \code{tbl_df}, \code{tbl}, \code{data.frame})
#' containing one row of metadata for the requested measure.
#'
#' Columns include:
#' \describe{
#'   \item{year}{Numeric. CHR&R release year.}
#'   \item{measure_id}{Numeric. Unique identifier for the measure.}
#'   \item{measure_name}{Character. Official name of the measure.}
#'   \item{description}{Character. Text description of the measure definition.}
#'   \item{years_used}{Character. Data years used in calculating the measure.}
#'   \item{factor_name}{Character. Top-level grouping in the CHR&R model.}
#'   \item{category_name}{Character. Category within factor.}
#'   \item{focus_area_name}{Character. Focus area within category.}
#'   \item{direction}{Numeric. Indicates score orientation:
#'     \code{1} = higher values are worse,
#'     \code{-1} = higher values are better.}
#'   \item{display_precision}{Numeric. Recommended number of decimal places
#'     for reporting values.}
#'   \item{format_type}{Numeric. Suggested display format code:
#'     0 = rate, 1 = percentage, 2 = dollars, 3 = ratio,
#'     other values reserved for internal codes.}
#'   \item{compare_states_text}{Character. Notes about comparability across states.}
#'   \item{compare_years_text}{Character. Notes about comparability across release years.}
#' }

#' @examples
#' \donttest{
#' # Return metadata
#' md <- get_chrr_measure_metadata(21, 2024)
#' md$display_precision
#' md$format_type
#' md$direction
#' md$compare_states_text
#' }
#' @export
get_chrr_measure_metadata <- function(measure, release_year = NULL) {

  .check_internet()
  # Compute most recent year dynamically
    most_recent <- max(as.integer(names(zenodo_year_records)))

    # If user didn’t specify, use most recent
    if (is.null(release_year)) {
      release_year <- most_recent
    }

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

 return(metadata)

}
