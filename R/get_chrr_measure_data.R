#' Retrieve County Health Rankings & Roadmaps measure data from GitHub
#'
#' @description
#' Downloads and filters County Health Rankings & Roadmaps (CHR&R) data directly
#' from the Zenodo archive
#' (<https://zenodo.org/records/17419267>).
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
#' @param release_year A numeric year corresponding to a CHR&R release year folder.
#'   Must match one of the year-specific subfolders available in the GitHub
#'   repository.
#'
#' @details
#' The function first reads the measure-year index (\code{t_measure_years.csv}) to
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
                          release_year,
                          refresh = FALSE) {
  # Validate geography argument
  geography <- match.arg(geography)

  # --- Load measure-year index (shared file on Zenodo) ---
  measure_index <- read_csv_zenodo(filename = "t_measure_years.csv")

  # --- Validate release_year dynamically ---
  valid_years <- unique(measure_index$year)
  if (!(release_year %in% valid_years)) {
    stop(
      "Invalid release_year: ", release_year,
      ". Available years are: ", paste(sort(valid_years), collapse = ", "), "."
    )
  }


  # --- Load measure-year index ---
  measure_info <- read_csv_zenodo(filename = "t_measure_years.csv") %>%
    dplyr::filter(year == !!release_year)

  # Match the measure
  if (is.numeric(measure)) {
    var_info <- measure_info %>% filter(measure_id == measure)
  } else {
    var_info <- measure_info %>%
      filter(str_detect(str_to_lower(measure_name), str_to_lower(measure)))
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

  # --- Load the data file depending on geography ---
  file_name <- if (geography == "county") {
    paste0("t_measure_data_years_", release_year, ".csv")
  } else {
    paste0("t_state_data_years_", release_year, ".csv")
  }

  # --- Use vroom for large county files, readr for smaller ones ---
  if (geography == "county") {
    data_dir <- prepare_zenodo_data(release_year, refresh)
    file_path <- file.path(data_dir, file_name)

    if (!file.exists(file_path)) {
      stop("County data file not found: ", file_path)
    }

    # Specify column types for speed
    col_types <- cols(
      state_fips  = col_character(),
      county_fips = col_character(),
      measure_id  = col_integer(),
     #measure_name= col_character(),
      year        = col_integer(),
      raw_value       = col_double(),
     .default    = col_guess()  # everything else is guessed automatically
    )

    message("Loading county data...")
    df <- vroom(file_path, delim = ",", col_types = col_types, progress = FALSE)

  } else {
    # Small file, read_csv is fine
    df <- read_csv_zenodo(release_year, file_name)
  }

  # --- Filter by measure ---
  df_out <- df %>% filter(measure_id == var_id)

  # --- Filter by geography if needed ---
  if (geography == "national") {
    df_out <- df_out %>% filter(state_fips == "00")
  } else if (geography == "state") {
    df_out <- df_out %>% filter(state_fips != "00")
  }

  # --- Rename year column to release_year ---
  if ("year" %in% names(df_out)) {
    df_out <- df_out %>% dplyr::rename(release_year = year)
  }

  return(df_out)
}
