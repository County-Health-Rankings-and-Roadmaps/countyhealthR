#' Get County-Level County Health Rankings & Roadmaps Data
#'
#' Returns County Health Rankings & Roadmaps (CHR&R) measure data
#' for a specified state and (optionally) county and release year.
#' Data are retrieved from the Zenodo repository and include measure
#' values and associated metadata.
#'
#' If a county is specified, county-level data are returned. If the
#' \code{county} argument is missing, \code{NA}, or \code{"000"},
#' state-level data are returned instead.
#'
#'
#' @param state \code{Character}. Specifies the state. May be a full
#'   state name (e.g., \code{"Wisconsin"}), postal abbreviation
#'   (e.g., \code{"WI"}), or two-digit FIPS code (e.g., \code{"55"}).
#' @param county \code{Character}. Specifies the county. May be a
#'   county name (e.g., \code{"Dane"}) or a three-digit county FIPS
#'   code (e.g., \code{"025"}). Matching is not case sensitive and
#'   ignores common suffixes such as "County," "Parish," "City,"
#'   "Borough," or similar. If missing, \code{NA}, or \code{"000"},
#'   state-level data are returned.
#' @param release_year \code{Numeric}. Specifies the CHR&R release
#'   year. Defaults to the most recent available release year.
#' @param refresh \code{Logical}. Indicates whether to force a fresh
#'   download from Zenodo even if cached data are available.
#'   Defaults to \code{FALSE}.
#' @param citation \code{Logical}. If \code{TRUE} (default), prints the
#'   appropriate Zenodo DOI for the requested release year which is useful for citation.
#'   Set to \code{FALSE} to suppress DOI output.
#' @param verbose \code{Logical}. If \code{TRUE} (default), additional information about the
#'   selected geography is displayed.
#'   Set to \code{FALSE} to return only the requested \code{data.frame}.
#'
#' @return
#' A tibble (class \code{tbl_df}, \code{tbl}, \code{data.frame})
#' containing CHR&R measure values for the specified geography
#' and release year.
#'
#' For county-level requests, the tibble includes one row per
#' measure for the specified county and contains the following columns:
#' \describe{
#'   \item{state_fips}{Character. Two-digit state FIPS code.}
#'   \item{county_fips}{Character. Three-digit county FIPS code.}
#'   \item{measure_id}{Character. Unique CHR&R measure identifier.}
#'   \item{measure_name}{Character. Measure name.}
#'   \item{description}{Character. Brief measure description.}
#'   \item{raw_value}{Numeric. Reported measure value.}
#'   \item{ci_low}{Numeric. Lower bound of confidence interval, if available.}
#'   \item{ci_high}{Numeric. Upper bound of confidence interval, if available.}
#'   \item{numerator}{Numeric. Measure numerator, if available.}
#'   \item{denominator}{Numeric. Measure denominator, if available.}
#'   \item{years_used}{Character. Years used in calculation of the measure.}
#'   \item{compare_years_text}{Character. Text describing temporal comparison.}
#'   \item{compare_states_text}{Character. Text describing state comparison.}
#' }
#'
#' For state-level requests, the structure is identical except that
#' \code{county_fips} is not included.
#'
#' The returned tibble represents the full set of CHR&R measures
#' available for the specified geography and release year.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # County-level example
#' dane <- get_chrr_county_data("WI", "Dane", 2024)
#' head(dane)
#'
#' # State-level example
#' wi <- get_chrr_county_data("WI", county = NULL, 2024)
#' head(wi)
#' }
get_chrr_county_data <- function(state,
                                 county = NULL,
                                 release_year = NULL,
                                 refresh = FALSE,
                                 citation = TRUE,
                                 verbose = TRUE) {

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
  ## determine if state-level request
  ## ----------------------------

  state_level <- (
    missing(county) ||
      is.null(county) ||
      is.na(county) ||
      toupper(trimws(as.character(county))) == "000"
  )

  ## ============================================================
  ## STATE-LEVEL PATH
  ## ============================================================

  if (state_level) {

    df <- try(
      read_csv_zenodo(
        filename = paste0("t_state_data_", release_year, ".csv"),
        year     = release_year,
        refresh  = refresh
      ),
      silent = TRUE
    )

    if (inherits(df, "try-error")) {
      stop("Failed to read Zenodo state CSV for year ", release_year)
    }

    statedf <- df %>%
      dplyr::filter(state_fips == state_fips_input)

    out <- statedf %>%
      dplyr::left_join(get_measure_map(), by = c("year", "measure_id")) %>%
      dplyr::rename(release_year = year) %>%
      dplyr::select(
        .data$state_fips,
        .data$measure_id,
        .data$measure_name,
        .data$description,
        .data$raw_value,
        .data$ci_low,
        .data$ci_high,
        .data$numerator,
        .data$denominator,
        .data$years_used,
        .data$compare_years_text,
        .data$compare_states_text
      )

    if (isTRUE(citation)) {
      message(print_zenodo_citation(release_year))
    }

    if (isTRUE(verbose)) {
      message(
        "\n\n Returning CHR&R data for ",
        state_name_resolved,
        " (fipscode ", state_fips_input,
        ") for release year ", release_year, ".\n\n"
      )
    }

    return(out)
  }




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
    dplyr::filter(state_fips == state_fips_input & county_fips == county_fips_input) %>%
    dplyr::select(-years_used) #to avoid double when merged with measure_map next

  out = countydf %>% dplyr::left_join(get_measure_map(), by = c("year", "measure_id")) %>%
    dplyr::rename(release_year = year) %>%
    dplyr::select(
      .data$state_fips,
      .data$county_fips,
      .data$measure_id,
      .data$measure_name,
      .data$description,
      .data$raw_value,
      .data$ci_low,
      .data$ci_high,
      .data$numerator,
      .data$denominator,
      .data$years_used,
      .data$compare_years_text,
      .data$compare_states_text
    )

  if (isTRUE(citation)) {
    message(print_zenodo_citation(release_year))
  }

  if (isTRUE(verbose)) {
    message(
      "\n\n Returning CHR&R data for ",
      county_name_resolved, ", ",
      state_name_resolved,
      " (fipscode ", state_fips_input,
      county_fips_input,
      ") for release year ", release_year, ".\n\n"
    )
  }


  return(out)

}

