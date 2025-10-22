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
  url <- glue::glue(
    "https://raw.githubusercontent.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/main/relational_data/t_measure_years.csv"
  )

  tmp <- tempfile(fileext = ".csv")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  df <- readr::read_csv(tmp, show_col_types = FALSE)

  df %>%
    dplyr::filter(year == .env$release_year) %>%
    dplyr::select(measure_id, measure_name, description)
}
