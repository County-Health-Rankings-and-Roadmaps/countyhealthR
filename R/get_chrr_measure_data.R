#' Retrieve County Health Rankings & Roadmaps measure data from GitHub
#'
#' @description
#' Downloads and filters County Health Rankings & Roadmaps (CHR&R) data directly
#' from the measure calculations GitHub repository
#' (<https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs>).
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
                          release_year) {
  # Validate geography argument
  geography <- match.arg(geography)

  # GitHub base URL (raw)
  base_url <- "https://raw.githubusercontent.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/main/relational_data/"

  # Load the relational data index
  index_url <- paste0(base_url, "t_measure_years.csv") # or whichever file lists measure_id/name
  measure_info <- readr::read_csv(index_url, show_col_types = FALSE)

  # --- Subset measure info by year first ---
  measure_info <- measure_info %>% dplyr::filter(year == !!release_year)

  # Match the measure by ID or by partial measure_name (case-insensitive)
  if (is.numeric(measure)) {
    var_info <- measure_info %>% dplyr::filter(measure_id == measure)
  } else {
    var_info <- measure_info %>%
      dplyr::filter(str_detect(str_to_lower(measure_name), str_to_lower(measure)))
  }

  if (nrow(var_info) == 0) stop("No matching measure found.")
  if (nrow(var_info) > 1) {
    message("Multiple matches found, returning first match:\n")
    print(var_info)
    var_info <- var_info[1, ]
  }

  var_id <- var_info$measure_id

  # --- Determine which data file to use ---
  year_folder <- paste0(base_url, release_year, "/")

  if (geography == "county") {
    data_url <- paste0(year_folder, "t_measure_data_years_", release_year, ".csv")
  } else if (geography %in% c("state", "national")) {
    data_url <- paste0(year_folder, "t_state_data_years_", release_year, ".csv")
  }

  # --- Load the appropriate dataset ---
  df <- readr::read_csv(data_url, show_col_types = FALSE)

  # --- Filter to the selected measure and geography ---
  df_out <- df %>% dplyr::filter(measure_id == var_id)

  if (geography == "national") {
    df_out <- df_out %>% dplyr::filter(state_fips == "00")
  } else if (geography == "state") {
    df_out <- df_out %>% dplyr::filter(state_fips != "00")
  }

  return(df_out)
}
