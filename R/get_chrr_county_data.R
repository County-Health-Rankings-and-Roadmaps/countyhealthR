#' Get County-Level County Health Rankings & Roadmaps Data
#'
#' Returns all available County Health Rankings & Roadmaps (CHR&R) measure data
#' for a specified county, state, and release year. The function accepts flexible
#' state and county inputs and pulls the
#' corresponding county-level data from the Zenodo repository.
#'
#' On successful execution, the function prints the appropriate Zenodo citation
#' along with the resolved county, state, and release year of the returned data.
#'
#' @param state A \code{character} specifying the state. May be a full state name
#'   (e.g., \code{"Wisconsin"}), postal abbreviation (e.g., \code{"WI"}), or
#'   two-digit FIPS code (e.g., \code{"55"}).
#' @param county A \code{character} specifying the county. May be a county name
#'   (e.g., \code{"Dane"}) or a three-digit county FIPS code (e.g., \code{"025"}).
#'   County name matching is not case sensitive and ignores common suffixes such as "County,"
#' "Parish," "City," or "Borough."
#' @param release_year A \code{numeric} specifying the CHR&R release year to pull
#'   county-level data. Returns the most recent release year as default.
#'   Importantly, this is not the same as the year represented by the data;
#'   see the \code{years_used} column of the output for the data year(s).
#' @param refresh A \code{logical} indicating whether to force a fresh download
#'   from Zenodo even if cached data are available. Defaults to \code{FALSE}.
#'
#' @return A tibble containing county-level CHR&R measure values, confidence
#'   intervals (where available), numerators, denominators, and basic measure
#'   metadata for the specified county and release year. For more detailed
#'   metadata, use \code{get_chrr_measure_metadata()}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' get_chrr_county_data("WI", "dane county", 2024)
#' get_chrr_county_data("Wisconsin", "025", 2023)
#' get_chrr_county_data("55", "DANE", 2022)
#' }
get_chrr_county_data <- function(state,
                                 county,
                                 release_year = NULL,
                                 refresh = FALSE) {

  .check_internet()

  # Compute most recent year dynamically
  most_recent <- max(as.integer(names(zenodo_year_records)))

  # If user didn’t specify, use most recent
  if (is.null(release_year)) {
    release_year <- most_recent
  }

  ## ----------------------------
  ## normalize inputs using county_choices
  ## ----------------------------

  # normalize county_choices for matching
  county_choices_norm <- get_county_choices() %>%
    dplyr::mutate(
      statecode    = toupper(trimws(statecode)),
      countycode   = toupper(trimws(countycode)),
      state        = toupper(trimws(state)),
      state_name   = toupper(trimws(state_name)),
      county       = toupper(trimws(county)) %>%
        gsub("\\s+(COUNTY|PARISH|CITY|PLANNING REGION|BOROUGH|MUNICIPALITY|CENSUS AREA)$", "", ., ignore.case = TRUE),
      fipscode     = trimws(fipscode)
    )

  ## ----------------------------
  ## resolve state
  ## ----------------------------

  state_input <- toupper(trimws(as.character(state)))

  # numeric FIPS
  if (grepl("^\\d{1,2}$", state_input)) {
    state_matches <- county_choices_norm %>%
      dplyr::filter(statecode == sprintf("%02d", as.integer(state_input)))
  } else {
    # abbreviation or full name
    state_matches <- county_choices_norm %>%
      dplyr::filter(state == state_input | state_name == state_input)
  }

  if (nrow(state_matches) == 0) {
    stop("State not recognized: ", state)
  }

  # unique state FIPS
  state_fips_input <- unique(state_matches$statecode)[1]
  state_name_resolved = unique(state_matches$state_name)[1]

  ## ----------------------------
  ## resolve county
  ## ----------------------------

  county_input <- toupper(trimws(as.character(county))) %>%
    gsub("\\s+(COUNTY|PARISH|CITY|PLANNING REGION)$", "", ., ignore.case = TRUE)

  county_matches <- state_matches %>%
    dplyr::filter(countycode == county_input | county == county_input)

  if (nrow(county_matches) == 0) {
    stop(
      "County not found in ", state, " (FIPS ", state_fips_input, ").\n",
      "You can specify the county by either its three-digit FIPS code or its name (not case sensitive).\n",
      "Valid ", state, " counties:\n",
      paste0("  ", state_matches$countycode, " - ", state_matches$county, collapse = "\n")
    )
  }

  county_fips_input <- unique(county_matches$countycode)[1]
  county_name_resolved <- unique(county_matches$county)[1]



  ## ----------------------------
  ## load file and filter by state and countycode
  ## ----------------------------


  # read file
  df <- try(
    read_csv_zenodo(
      filename = paste0("t_measure_data_years_", release_year, ".csv"),
      year     = release_year,
      refresh  = refresh
    ),
    silent = TRUE
  )

  if (inherits(df, "try-error")) stop("Failed to read Zenodo CSV for year ", release_year)

  countydf = df %>%
    filter(state_fips == state_fips_input & county_fips == county_fips_input) %>%
    select(-years_used) #to avoid double when merged with measure_map next

  out = countydf %>% dplyr::left_join(get_measure_map(), by = c("year", "measure_id")) %>%
    dplyr::rename(release_year = year) %>%
    dplyr::select(
      state_fips,
      county_fips,
      measure_id,
      measure_name,
      description,
      raw_value,
      ci_low,
      ci_high,
      numerator,
      denominator,
      years_used,
      compare_years_text,
      compare_states_text
    )
  ## ----------------------------
  ## success messages
  ## ----------------------------

  message(
    "\n\n Returning CHR&R data for ",
    county_name_resolved, ", ",
    state_name_resolved,
    " (fipscode ", state_fips_input,
    county_fips_input,
    ") for release year ", release_year, ".\n\n",
    print_zenodo_citation(release_year)
  )


  return(out)

}

