#' List available CHRR measures for a given year
#'
#' Downloads metadata for measures and returns their IDs and names.
#'
#' @param year Numeric year.
#'
#' @return A tibble with measure_id and measure_name.
#' @export
#'
#' @examples
#' \dontrun{
#' list_chrr_measures(2023)
#' }
list_chrr_measures <- function(year = 2023) {
  url <- glue::glue(
    "https://raw.githubusercontent.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/main/relational_data/t_measure_years.csv"
  )

  tmp <- tempfile(fileext = ".csv")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  df <- readr::read_csv(tmp, show_col_types = FALSE)

  df %>%
    filter(year == .env$year) %>%
    dplyr::select(measure_id, measure_name)
}
