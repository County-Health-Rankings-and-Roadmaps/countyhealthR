
###############################################
# this will not work until i've properly pulled in the t_state data!!!!

#' Retrieve County Health Rankings (CHRR) data
#'
#' @param geography Character. One of "county", "state", or "US".
#' @param measures Character or numeric vector of measure IDs.
#' @param years Integer or character vector of release years (e.g., 2023). If NULL, all available years will be returned.
#' @param counties Character vector of counties (5-digit FIPS or names like "Dane County", "Dane, WI"). Optional. Default is to return values for all counties.
#' @param state Character vector of state names or abbreviations. Optional. Defaults is to return values for all states.
#' @return A tibble of CHRR measure data.
#' @export
#' @examples
#' \dontrun{
#' # All measures for Dane County in 2023
#' get_chrr_data(geography = "county", counties = "Dane County", years = 2023)
#'
#' # All counties for Unemployment in 2023
#' get_chrr_data(geography = "county", measures = "Unemployment", years = 2023)
#'
#' # All years for WI state-level Unemployment
#' get_chrr_data(geography = "state", state = "WI", measures = "Unemployment")
#'
#' # National data for 2023
#' get_chrr_data(geography = "US", measures = "Unemployment", years = 2023)
#' }



get_chrr_data <- function(geography,
                          measures = NULL,
                          years = NULL,
                          counties = NULL,
                          state = NULL) {

  # ---- validate geography ----
  if (length(geography) != 1 || !tolower(geography) %in% c("county","state","us")) {
    stop("`geography` must be a single value: 'county', 'state', or 'US'.")
  }
  geography <- tolower(geography)

  # ---- determine years ----
  if (is.null(years)) {
    if (exists("list_chrr_years")) {
      years <- list_chrr_years()
    } else {
      stop("`years` is NULL and helper `list_chrr_years()` not found.")
    }
  }
  years <- as.character(years)

  # ---- normalize measures ----
  if (!is.null(measures)) measures <- as.character(measures)

  # ---- prepare county FIPS ----
  county_fips_vec <- NULL
  counties_df <- NULL
  if (geography == "county") {
    # download and read FIPS lookup
    url_cnty <- "https://raw.githubusercontent.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/main/inputs/county_fips.sas7bdat"
    tmp_cnty <- tempfile(fileext = ".sas7bdat")
    utils::download.file(url_cnty, tmp_cnty, mode = "wb", quiet = TRUE)
    counties_df <- haven::read_sas(tmp_cnty)

    names_lc <- tolower(names(counties_df))
    if ("fipscode" %in% names_lc) {
      fips_col <- names(counties_df)[which(names_lc == "fipscode")[1]]
      counties_df$fipscode <- as.character(counties_df[[fips_col]])
    } else if (all(c("statecode","countycode") %in% names_lc)) {
      sc <- names(counties_df)[which(names_lc == "statecode")[1]]
      cc <- names(counties_df)[which(names_lc == "countycode")[1]]
      counties_df$fipscode <- sprintf("%02d%03d", as.integer(counties_df[[sc]]), as.integer(counties_df[[cc]]))
    }
    counties_df$fipscode <- stringr::str_pad(stringr::str_trim(counties_df$fipscode), width = 5, pad = "0")

    county_name_idx <- which(grepl("county", names_lc) & !grepl("code", names_lc))
    county_name_col <- names(counties_df)[county_name_idx[1]]
    state_col <- if ("state" %in% names_lc) names(counties_df)[which(names_lc=="state")[1]] else NA
    counties_df$county_clean <- stringr::str_to_lower(stringr::str_remove_all(counties_df[[county_name_col]], "\\b[Cc]ounty\\b"))

    state_from_input <- function(s){
      s_trim <- stringr::str_trim(s)
      if (toupper(s_trim) %in% state.abb) return(toupper(s_trim))
      nm <- stringr::str_to_title(s_trim)
      if (nm %in% state.name) return(state.abb[match(nm, state.name)])
      return(NA_character_)
    }

    # ---- if counties=NULL, return all counties ----
    if (is.null(counties)) {
      county_fips_vec <- counties_df$fipscode
    } else {
      # match user counties to FIPS
      found_fips <- character(0)
      for (u in counties){
        u_trim <- stringr::str_trim(u)
        if (grepl("^\\d+$", u_trim)) {
          found_fips <- c(found_fips, stringr::str_pad(u_trim, 5, "left","0"))
          next
        }
        county_clean <- stringr::str_to_lower(stringr::str_remove_all(u_trim,"\\b[Cc]ounty\\b"))
        matches <- counties_df[counties_df$county_clean==county_clean, , drop=FALSE]
        if (nrow(matches)==0) matches <- counties_df[stringr::str_detect(counties_df$county_clean, fixed(county_clean)), , drop=FALSE]
        if (nrow(matches)>0) found_fips <- c(found_fips, unique(matches$fipscode))
      }
      if (length(found_fips)==0) stop("No county FIPS codes matched your `counties` input.")
      county_fips_vec <- unique(found_fips)
    }
  }

  # ---- build state abbreviation vector ----
  state_abbrev_vec <- NULL
  if (geography == "state"){
    if (is.null(state)) {
      # return all states automatically
      state_abbrev_vec <- state.abb
    } else {
      state_abbrev_vec <- purrr::map_chr(state, function(s){
      s_trim <- stringr::str_trim(s)
      if (toupper(s_trim) %in% state.abb) return(toupper(s_trim))
      nm <- stringr::str_to_title(s_trim)
      if (nm %in% state.name) return(state.abb[match(nm, state.name)])
      stop(paste0("Invalid state input: ", s))
    })
  }}

  # ---- download & filter by year / measure / geography ----
  out_list <- list()
  for (y in years){
    url_y <- glue::glue("https://raw.githubusercontent.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/main/relational_data/{y}/t_measure_data_{y}.csv")
    tmp_y <- tempfile(fileext = ".csv")
    ok <- tryCatch({
      utils::download.file(url_y, tmp_y, mode="wb", quiet=TRUE)
      TRUE
    }, error=function(e){warning("Failed to download year ",y); FALSE})
    if (!ok) next

    df <- tryCatch({
      readr::read_csv(tmp_y, show_col_types = FALSE)
    }, error=function(e){return(NULL)})
    if (is.null(df)) next

    df$fips <- paste0(df$state_fips, df$county_fips)
    if ("measure_id" %in% names(df)) df$measure_id <- as.character(df$measure_id)

    if (!is.null(county_fips_vec) && "fips" %in% names(df)) df <- df[df$fips %in% county_fips_vec, , drop=FALSE]
    if (!is.null(state_abbrev_vec) && "state_fips" %in% names(df)) df <- df[df$state_fips %in% state_abbrev_vec, , drop=FALSE]
    if (!is.null(measures) && "measure_id" %in% names(df)) df <- df[df$measure_id %in% measures, , drop=FALSE]

    if (nrow(df)>0) {df$year <- y; out_list[[as.character(y)]] <- df}
  }

  result <- dplyr::bind_rows(out_list)
  if (nrow(result)==0) warning("Query returned 0 rows. Check years/measures/locations.")
  tibble::as_tibble(result)
}
