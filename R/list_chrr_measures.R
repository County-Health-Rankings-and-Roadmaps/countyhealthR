#' List available CHR&R measures for a given release year
#'
#' Downloads metadata for a release year and returns measure identifiers,
#' names, and descriptions.
#'
#' @param release_year Numeric year corresponding to the year of
#' CHR&R's annual data release. Defaults to the most recent release
#' year if \code{NULL}.
#'
#' @return
#' A tibble (class \code{tbl_df}) with one row per measure for the
#' specified release year and the following columns:
#' \describe{
#'   \item{measure_id}{Numeric. Unique identifier for the measure.}
#'   \item{measure_name}{Character. Name of the measure.}
#'   \item{description}{Character. Brief description of the measure.}
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' measures <- list_chrr_measures(2023)
#' head(measures)
#' }
list_chrr_measures <- function(release_year = NULL) {

  .check_internet()
  # Compute most recent year dynamically
  most_recent <- max(as.integer(names(zenodo_year_records)))

  # If user didn’t specify, use most recent
  if (is.null(release_year)) {
    release_year <- most_recent
  }

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
