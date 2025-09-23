#' Get CHRR measure data (years, measures, counties)
#'
#' Must supply at least two of the three arguments: `years`, `measures`, `counties`.
#'
#' @param years Integer or character vector of release years (e.g. 2023). If NULL and the other two are supplied, the function will search all available years.
#' @param measures Character or numeric vector of measure IDs.
#' @param counties Character vector of county inputs. Accepts 5-digit FIPS (e.g. "55025" or 55025), county names ("Dane County", "Dane", "Dane, WI"), case-insensitive.
#' @return A tibble with the requested CHRR measure data (columns from the relational csv + `year`).
#' @export
#' @examples
#' \dontrun{
#' # Year + county
#' get_chrr_data(years = 2023, counties = "Dane County")
#'
#' # Year + measure
#' get_chrr_data(years = 2023, measures = c("Premature_death"))
#'
#' # County + measure (all years)
#' get_chrr_data(counties = "Dane County", measures = "Premature_death")
#' }
get_chrr_data <- function(years = NULL, measures = NULL, counties = NULL) {
  # ---- require at least two of three ----
  n_supplied <- sum(!is.null(years), !is.null(measures), !is.null(counties))
  if (n_supplied < 2) {
    stop("You must provide at least two of the following: years, measures, counties.")
  }

  # ---- normalize measures ----
  if (!is.null(measures)) measures <- as.character(measures)

  # ---- determine years to search ----
  if (is.null(years)) {
    # try to use helper list_chrr_years() if available
    if (exists("list_chrr_years", mode = "function")) {
      years <- list_chrr_years()
    } else {
      stop("`years` is NULL and helper `list_chrr_years()` not found. Provide `years` or add the helper to your environment.")
    }
  }
  years <- as.character(years)

  # ---- prepare county FIPS if counties provided ----
  county_fips_vec <- NULL
  if (!is.null(counties)) {
    # download county fips SAS file
    url_cnty <- "https://raw.githubusercontent.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/main/inputs/county_fips.sas7bdat"
    tmp_cnty <- tempfile(fileext = ".sas7bdat")
    utils::download.file(url_cnty, tmp_cnty, mode = "wb", quiet = TRUE)
    counties_df <- haven::read_sas(tmp_cnty)

    # try to identify columns: fipscode, county, state
    names_lc <- tolower(names(counties_df))

    # fipscode detection / creation
    if ("fipscode" %in% names_lc) {
      fips_col <- names(counties_df)[which(names_lc == "fipscode")[1]]
      counties_df$fipscode <- as.character(counties_df[[fips_col]])
    } else if (all(c("statecode", "countycode") %in% names_lc)) {
      sc <- names(counties_df)[which(names_lc == "statecode")[1]]
      cc <- names(counties_df)[which(names_lc == "countycode")[1]]
      # ensure numeric -> pad
      counties_df$fipscode <- sprintf("%02d%03d", as.integer(counties_df[[sc]]), as.integer(counties_df[[cc]]))
    } else {
      # fallback: search for any column with "fips" in name
      idx <- which(grepl("fips", names_lc))
      if (length(idx) >= 1) {
        counties_df$fipscode <- as.character(counties_df[[idx[1]]])
      } else {
        stop("Could not find or construct a FIPS code column in county_fips.sas7bdat")
      }
    }
    # normalize to 5-digit strings
    counties_df$fipscode <- stringr::str_pad(stringr::str_trim(as.character(counties_df$fipscode)), width = 5, side = "left", pad = "0")

    county_name_idx <- which(grepl("county", names_lc) & !grepl("code", names_lc))
    if (length(county_name_idx) >= 1) {
      county_name_col <- names(counties_df)[county_name_idx[1]]
    } else {
      stop("Could not find a county name column in county_fips.sas7bdat")
    }

    # state abbreviation column (common name 'state' in the dataset)
    state_col <- if ("state" %in% names_lc) names(counties_df)[which(names_lc == "state")[1]] else NA

    # create a cleaned county name column for matching (remove trailing "County", lowercase)
    counties_df$county_clean <- stringr::str_to_lower(
      stringr::str_trim(
        stringr::str_remove_all(counties_df[[county_name_col]], regex("\\b[Cc]ounty\\b"))
      )
    )

    # helper to convert possible state input into abbreviation
    state_from_input <- function(s) {
      s_trim <- stringr::str_trim(s)
      if (toupper(s_trim) %in% state.abb) return(toupper(s_trim))
      nm <- stringr::str_to_title(s_trim)
      if (nm %in% state.name) return(state.abb[match(nm, state.name)])
      return(NA_character_)
    }

    # build fips vector from user-supplied counties
    user_counties <- as.character(counties)
    found_fips <- character(0)

    for (u in user_counties) {
      u_trim <- stringr::str_trim(u)

      # numeric-like -> assume FIPS
      if (grepl("^\\d+$", u_trim)) {
        f <- stringr::str_pad(u_trim, width = 5, side = "left", pad = "0")
        found_fips <- c(found_fips, f)
        next
      }

      # detect "County, ST" or "County, State"
      if (grepl(",", u_trim)) {
        parts <- strsplit(u_trim, ",")[[1]]
        county_part <- stringr::str_trim(parts[1])
        state_part  <- stringr::str_trim(parts[2])
        state_abbr  <- state_from_input(state_part)

        county_clean <- stringr::str_to_lower(stringr::str_remove_all(county_part, regex("\\b[Cc]ounty\\b")))
        candidates <- counties_df
        if (!is.na(state_abbr) && !is.na(state_col)) {
          candidates <- candidates[candidates[[state_col]] == state_abbr, , drop = FALSE]
        }
        matches <- candidates[candidates$county_clean == county_clean, , drop = FALSE]
        if (nrow(matches) == 0) {
          # try partial match
          matches <- candidates[stringr::str_detect(candidates$county_clean, fixed(county_clean)), , drop = FALSE]
        }
        if (nrow(matches) > 0) {
          found_fips <- c(found_fips, unique(matches$fipscode))
          next
        }
      }

      # no comma -> match by county name (try exact then partial)
      county_clean <- stringr::str_to_lower(stringr::str_remove_all(u_trim, regex("\\b[Cc]ounty\\b")))
      matches <- counties_df[counties_df$county_clean == county_clean, , drop = FALSE]
      if (nrow(matches) == 0) {
        matches <- counties_df[stringr::str_detect(counties_df$county_clean, fixed(county_clean)), , drop = FALSE]
      }
      if (nrow(matches) > 0) {
        found_fips <- c(found_fips, unique(matches$fipscode))
        next
      }

      # if we reach here, no match for this user entry
      warning("No county match for input: '", u, "'. Try 'Dane County' or 'Dane, WI' or 5-digit FIPS.")
    } # end loop user_counties

    found_fips <- unique(found_fips)
    if (length(found_fips) == 0) stop("No county FIPS codes matched your `counties` input.")
    county_fips_vec <- found_fips
  } # end if counties provided

  # ---- download & filter per year ----
  out_list <- list()
  for (y in years) {
    url_y <- glue::glue("https://raw.githubusercontent.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/main/relational_data/{y}/t_measure_data_{y}.csv")
    tmp_y <- tempfile(fileext = ".csv")
    ok <- tryCatch({
      utils::download.file(url_y, tmp_y, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      warning("Failed to download year ", y, ": ", conditionMessage(e))
      FALSE
    })

    if (!ok) next

    df <- tryCatch({
      readr::read_csv(tmp_y, show_col_types = FALSE)
    }, error = function(e) {
      warning("Failed to read CSV for year ", y, ": ", conditionMessage(e))
      return(NULL)
    })
    if (is.null(df)) next

    # normalize df fips and measure_id for safe matching
    df$fips <- paste0(df$state_fips, df$county_fips)

    if ("measure_id" %in% names(df)) df$measure_id <- as.character(df$measure_id)

    # apply filters
    if (!is.null(county_fips_vec) && "fips" %in% names(df)) {
      df <- df[df$fips %in% county_fips_vec, , drop = FALSE]
    }

    if (!is.null(measures) && "measure_id" %in% names(df)) {
      measures_char <- as.character(measures)
      df <- df[df$measure_id %in% measures_char, , drop = FALSE]
    }

    if (nrow(df) > 0) {
      df$year <- as.character(y)
      out_list[[as.character(y)]] <- df
    }
  } # end years loop

  result <- dplyr::bind_rows(out_list)

  if (nrow(result) == 0) {
    warning("Query returned 0 rows. Check that your combinations of years / measures / counties exist in the repository.")
  }

  tibble::as_tibble(result)
}
