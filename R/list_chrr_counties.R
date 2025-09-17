#' List available counties for a given state
#'
#' @param state Character. State abbreviation (e.g. "WI") or full state name (e.g. "Wisconsin").
#' @return A tibble with columns: state, county, statecode, countycode, fipscode.
#' @export
#' @examples
#' \dontrun{
#' list_chrr_counties("WI")
#' list_chrr_counties("Wisconsin")
#' list_chrr_counties("wi")
#' list_chrr_counties("wisconsin")
#' }
list_chrr_counties <- function(state) {
  # Normalize input
  state <- stringr::str_trim(state)
  state <- stringr::str_to_title(state)

  # Handle abbreviations
  if (toupper(state) %in% state.abb) {
    state_abbrev <- toupper(state)
  } else if (state %in% state.name) {
    state_abbrev <- state.abb[match(state, state.name)]
  } else {
    stop("Input must be a valid state abbreviation or state name.")
  }

  # Download county FIPS file
  url <- "https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/inputs/county_fips.sas7bdat"
  tmp <- tempfile(fileext = ".sas7bdat")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  counties <- haven::read_sas(tmp)

  # Filter by normalized abbreviation
  df <- counties %>%
    dplyr::filter(state == !!state_abbrev) %>%
    dplyr::select(state, county, statecode, countycode, fipscode) %>%
    dplyr::arrange(county)

  tibble::as_tibble(df)
}
