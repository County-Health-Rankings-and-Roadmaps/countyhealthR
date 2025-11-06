#' List available CHR&R measures for a given release year
#'
#' Downloads metadata for measures and returns their IDs and names.
#'
#' @param release_year Numeric year. This corresponds to the year of CHR&R's annual data release.
#'
#' @return A tibble with measure_id, measure_name, and description.
#' @export
#'
#' @examples
#' \dontrun{
#' list_chrr_measures(2023)
#' }
list_chrr_measures <- function(release_year = 2023) {
  message("Loading measure metadata from Zenodo...")

  # Load the measure-year index from Zenodo root
  df <- read_csv_zenodo(filename = "t_measure_years.csv")

  # Validate that release_year exists
  valid_years <- unique(df$year)
  if (!(release_year %in% valid_years)) {
    stop(
      "Invalid release_year: ", release_year, ". ",
      "Available years are: ", paste(sort(valid_years), collapse = ", "), "."
    )
  }

  df %>%
    dplyr::filter(year == .env$release_year) %>%
    dplyr::select(measure_id, measure_name, description)
}
