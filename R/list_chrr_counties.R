#' List available counties for one or more states
#'
#' @param state Character vector. State abbreviation(s) (e.g. "WI") or full state name(s) (e.g. "Wisconsin").
#' @return A tibble with columns: state, county, statecode, countycode, fipscode.
#' @export
#' @examples
#' \dontrun{
#' list_chrr_counties("WI")
#' list_chrr_counties(c("WI", "MN"))
#' list_chrr_counties(c("Wisconsin", "Minnesota"))
#' list_chrr_counties(c("wi", "mn"))
#' }
list_chrr_counties <- function(state) {
  # Normalize input
  state <- stringr::str_trim(state)
  state <- stringr::str_to_title(state)

  # Map state names/abbreviations to abbreviations
  state_abbrevs <- purrr::map_chr(state, function(x) {
    if (toupper(x) %in% state.abb) {
      toupper(x)
    } else if (x %in% state.name) {
      state.abb[match(x, state.name)]
    } else {
      stop(paste0("Invalid state input: ", x))
    }
  })




  # Download county FIPS file
  url <- "https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/inputs/county_fips.sas7bdat"
  tmp <- tempfile(fileext = ".sas7bdat")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  counties <- haven::read_sas(tmp)

  # Filter by normalized abbreviation
  df <- counties %>%
    dplyr::filter(state %in% state_abbrevs) %>%
    dplyr::select(state, county, statecode, countycode, fipscode) %>%
    dplyr::arrange(county)

  tibble::as_tibble(df)
}
