#' Retrieve County Health Rankings & Roadmaps measure data from 'Zenodo'
#'
#' @description
#' Downloads and filters County Health Rankings & Roadmaps (CHR&R) data directly
#' from the 'Zenodo' archive
#' (\doi{10.5281/zenodo.18157681}).
#' Users provide the measure ID, geography type, and release year as inputs.
#' The function returns data for the specified measure across the specified geography
#' for the given release year. It mimics the style and behavior of
#' \code{tidycensus::get_decennial()}.
#'
#' @param geography A string specifying the level of geography to return.
#'   Options are:
#'   \itemize{
#'     \item \code{"county"} – returns all county-level estimates.
#'     \item \code{"state"} – returns all state-level estimates (excluding national).
#'     \item \code{"national"} – returns only national-level estimates (\code{state_fips == "00"}).
#'   }
#'
#' @param measure Either:
#'   \itemize{
#'     \item A numeric \code{measure_id} (e.g., \code{21}, \code{85}, etc.), or
#'     \item A character string matching a \code{measure_name}
#'       (case-insensitive).
#'   }
#'   Use the \code{list_chrr_measures()} function to print all available
#'   \code{measure_id}s and \code{measure_name}s for a given release year.
#'
#' @param release_year A \code{numeric} specifying the CHR&R release year to pull data.
#' Defaults to the most recent release year if \code{NULL}.
#'
#' @param refresh Logical. If TRUE, forces re-download of data
#'   even if a cached version is available.
#'
#' @details
#' The function first reads the measure-year index to
#' identify available measures for the specified year, then automatically retrieves
#' the correct data file:
#' \itemize{
#'   \item For \code{geography = "county"}, data are pulled from
#'     \code{t_measure_data_years_[YEAR].csv}.
#'   \item For \code{geography = "state"} or \code{"national"}, data are pulled from
#'     \code{t_state_data_years_[YEAR].csv}.
#' }
#'
#' For national data, rows where \code{state_fips == "00"} are retained.
#' For state-level data, rows where \code{state_fips != "00"} are retained.
#'
#' @return
#' A tibble containing filtered CHRR data for the requested geography, measure,
#' and release year. Typical columns include:
#' \code{state_fips}, \code{county_fips}, \code{measure_id},
#' \code{measure_name}, \code{release_year}, and \code{value}.
#'
#' @examples
#' \dontrun{
#' # Get county-level data for measure 21 (high school graduation) in 2023
#' county_data <- get_chrr_measure_data(
#'                   geography = "county",
#'                   measure = 21,
#'                   release_year = 2023)
#'
#' # Get county-level data for access to exercise (132) in 2025
#' county_data <- get_chrr_measure_data(
#'                   geography = "county",
#'                   measure = "access to exercise",
#'                   release_year = 2023)
#'
#' # Get state-level data for "Insufficient Sleep" in 2022
#' state_data <- get_chrr_measure_data(
#'                   geography = "state",
#'                   measure = "insufficient sleep",
#'                   release_year = 2022)
#'
#' # Get national-level data for "Uninsured" in 2024
#' nat_data <- get_chrr_measure_data(
#'                   geography = "national",
#'                   measure = "uninsured",
#'                   release_year = 2024)
#' }
#'
#' @importFrom dplyr filter %>% select mutate
#' @importFrom readr read_csv
#' @importFrom stringr str_detect str_to_lower
#' @export




get_chrr_measure_data <- function(geography = c("county", "state", "national"),
                          measure,
                          release_year = NULL,
                          refresh = FALSE) {
  # Validate geography argument
  #geography <- match.arg(geography)

  .check_internet()

  # Compute most recent year dynamically
  most_recent <- max(as.integer(names(zenodo_year_records)))

  # If user didn’t specify, use most recent
  if (is.null(release_year)) {
    release_year <- most_recent
  }

  # --- Load measure-year index (shared file on Zenodo) ---
  measure_index <- get_measure_map()

  # --- Validate release_year dynamically ---
  valid_years <- unique(measure_index$year)
  if (!(release_year %in% valid_years)) {
    stop(
      "Invalid release_year: ", release_year,
      ". Available years are: ", paste(sort(valid_years), collapse = ", "), "."
    )
  }


  # --- Load measure-year index ---
  measure_info <- measure_index %>%
    dplyr::filter(year == !!release_year)

  # Match the measure
  if (is.numeric(measure)) {
    var_info <- measure_info %>% dplyr::filter(measure_id == measure)
  } else {
    var_info <- measure_info %>%
      dplyr::filter(str_detect(str_to_lower(measure_name), str_to_lower(measure)))
  }

  if (nrow(var_info) == 0) {
    stop(
      "No matching measure found for ", release_year, ". ",
      "Use list_chrr_measures(release_year = ", release_year,") to see available measure_ids and measure_names for ", release_year, "."
    )
  }
  if (nrow(var_info) > 1) {
    message("Multiple matches found, returning first match:\n")
    print(var_info)
    var_info <- var_info[1, ]
  }

  var_id <- var_info$measure_id

  measure_name_resolved = var_info$measure_name


  # --- Load the data file depending on geography ---
  file_name <- if (geography == "county") {
    paste0("t_measure_data_years_", release_year, ".csv")
  } else {
    paste0("t_state_data_years_", release_year, ".csv")
  }

  # --- Use vroom for large county files, readr for smaller ones ---
  if (geography == "county") {
    file_path <- file.path(
      prepare_zenodo_data(release_year, refresh),
      file_name
    )

    # Ensure file is downloaded (Zenodo-native behavior)
    read_csv_zenodo(
      filename = file_name,
      year     = release_year,
      refresh  = refresh
    )


    # Specify column types for speed
    col_types <- readr::cols(
      state_fips  = readr::col_character(),
      county_fips = readr::col_character(),
      measure_id  = readr::col_integer(),
     #measure_name= readr::col_character(),
      year        = readr::col_integer(),
      raw_value       = readr::col_double(),
     .default    = readr::col_guess()  # everything else is guessed automatically
    )

    df <- read_csv_zenodo(
      filename = file_name,
      year     = release_year,
      refresh  = refresh
    )


  } else {
    # Small file, read_csv is fine
    df <- read_csv_zenodo(
      filename = file_name,
      year     = release_year,
      refresh  = refresh
    )

  }

  # --- Filter by measure ---
  df_out <- df %>% filter(measure_id == var_id)

  # --- Filter by geography if needed ---
  if (geography == "national") {
    df_out <- df_out %>% filter(state_fips == "00")
  } else if (geography == "state") {
    df_out <- df_out %>% filter(state_fips != "00")
  }

  data_years = measure_info %>%
    dplyr::filter(measure_name == measure_name_resolved) %>%
    dplyr::select(.data$years_used)

  # --- Rename year column to release_year ---
  if ("year" %in% names(df_out)) {
    df_out <- df_out %>%
      dplyr::rename(release_year = year) %>%
      dplyr::select(
        .data$state_fips,
        .data$county_fips,
        .data$raw_value,
        .data$numerator,
        .data$denominator,
        .data$ci_low,
        .data$ci_high
      )
  }

  message(
    "\n\nReturning CHR&R data for ",
    measure_name_resolved, " (measure ID #", var_info$measure_id, ")\n",
    "at the ", geography, "-level for release year ",
    release_year, " (data years: ",  var_info$years_used, ").", "\n",
    var_info$compare_states_text, ". ",
    var_info$compare_years_text, ".\n\n",
    print_zenodo_citation(release_year)
  )

  return(df_out)
}
