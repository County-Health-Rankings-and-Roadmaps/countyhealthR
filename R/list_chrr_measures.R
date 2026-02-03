#' List available CHR&R measures for a given release year
#'
#' Downloads metadata for measures and returns their IDs and names.
#'
#' @param release_year Numeric year. This corresponds to the year of CHR&R's annual data release. Defaults to the most recent release year.
#'
#' @return A tibble with measure_id, measure_name, and description.
#' @export
#'
#' @examples
#' \dontrun{
#' list_chrr_measures(2023)
#' }
list_chrr_measures <- function(release_year = max(as.integer(names(zenodo_year_records)))) {
  message(paste0("Loading all CHR&R measures for release year ", release_year))

  df <- read_csv_zenodo(
    filename = "t_measure_years.csv",
    year = max(as.integer(names(zenodo_year_records)))
  )

  # Validate that release_year exists (if provided)
  if (!is.null(release_year)) {
    valid_years <- unique(df$year)
    if (!(release_year %in% valid_years)) {
      stop(
        "Invalid release_year: ", release_year, ". ",
        "Available years are: ", paste(sort(valid_years), collapse = ", "), "."
      )
    }

    df <- df %>% dplyr::filter(year == .env$release_year)
  }

  df %>%
    dplyr::select(measure_id, measure_name, description)
}
