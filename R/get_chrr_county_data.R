get_chrr_county_data <- function(state, county, release_year, refresh = FALSE) {

  ## ----------------------------
  ## normalize inputs using county_choices
  ## ----------------------------

  # normalize county_choices for matching
  county_choices_norm <- county_choices %>%
    dplyr::mutate(
      statecode    = toupper(trimws(statecode)),
      countycode   = toupper(trimws(countycode)),
      state        = toupper(trimws(state)),
      state_name   = toupper(trimws(state_name)),
      county       = toupper(trimws(county)) %>%
        gsub("\\s+(COUNTY|PARISH|CITY|PLANNING REGION|BOROUGH|MUNICIPALITY)$", "", ., ignore.case = TRUE),
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

  ## ----------------------------
  ## resolve county
  ## ----------------------------

  county_input <- toupper(trimws(as.character(county))) %>%
    gsub("\\s+(COUNTY|PARISH|CITY|PLANNING REGION)$", "", ., ignore.case = TRUE)

  county_matches <- state_matches %>%
    dplyr::filter(countycode == county_input | county == county_input)

  if (nrow(county_matches) == 0) {
    stop(
      "County not found in ", state, " (FIPS ", state_fips, ").\n",
      "Valid counties include:\n",
      paste0("  ", state_matches$countycode, " - ", state_matches$county, collapse = "\n")
    )
  }

  county_fips_input <- unique(county_matches$countycode)[1]


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

  df %>%
      filter(state_fips == state_fips_input & county_fips == county_fips_input)

}
