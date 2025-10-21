#' Get Metadata for a CHRR Measure
#'
#' Returns key information about a County Health Rankings & Roadmaps measure,
#' including measure ID, measure name, description, years used, factor name,
#' category name, focus area, and whether the measure is comparable across
#' states and years.
#'
#' @param measure A \code{character} specifying the measure. Can be either
#'   a \code{measure_id} or \code{measure_name}.
#' @param release_year A \code{numeric} specifying the release year for which
#'   to pull the metadata.
#' @return A \code{tibble} with the metadata for the specified measure.
#' @examples
#' \dontrun{
#' get_chrr_measure_metadata("m_1234", 2024)
#' get_chrr_measure_metadata("Adult Smoking", 2024)
#' }
#' @export
get_chrr_measure_metadata <- function(measure, release_year) {

  # Load supporting CSVs from GitHub
  mea_years <- read_csv_github(file.path(DATA_DIR, "t_measure_years.csv")) %>%
    dplyr::select(year, measure_id, years_used)

  mea_compare <- read_csv_github(file.path(DATA_DIR, "t_measure.csv"))
  cat_names <- read_csv_github(file.path(DATA_DIR, "t_category.csv"))
  fac_names <- read_csv_github(file.path(DATA_DIR, "t_factor.csv"))
  foc_names <- read_csv_github(file.path(DATA_DIR, "t_focus_area.csv"))

  # Combine measure metadata
  mea_names <- mea_years %>%
    dplyr::full_join(mea_compare, by = c("measure_id", "year"))

  # Filter for requested release year
  measure_map <- mea_names %>%
    dplyr::filter(year == release_year) %>%
    dplyr::left_join(foc_names, by = c("measure_parent" = "focus_area_id", "year")) %>%
    dplyr::left_join(fac_names, by = c("focus_area_parent" = "factor_id", "year")) %>%
    dplyr::left_join(cat_names, by = c("factor_parent" = "category_id", "year")) %>%
    dplyr::select(
      measure_id, measure_name, years_used,
      factor_name, category_name, focus_area_name = focus_area_name,
      compare_states, compare_years, description
    )

  # Filter for the requested measure by ID or name
  result <- measure_map %>%
    dplyr::filter(measure_id == measure | measure_name == measure)

  if (nrow(result) == 0) {
    stop("No metadata found for measure: ", measure, " in release year: ", release_year)
  }

  return(result)
}
