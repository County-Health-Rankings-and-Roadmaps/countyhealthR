#' Get Metadata for a County Health Rankings & Roadmaps measure of health
#'
#' Returns key information about a County Health Rankings & Roadmaps measure,
#' including measure ID, measure name, description, years used, factor name,
#' category name, focus area, and whether the measure is comparable across
#' states and years.
#'
#' @param measure A \code{character} specifying the measure. Can be either
#'   a \code{measure_id} or \code{measure_name}.
#' @param release_year A \code{numeric} specifying the release year for which
#'   to pull the metadata.
#' @return A tibble of measure metadata (invisibly) and prints a readable summary.
#' @export
#' @examples
#' \dontrun{
#' get_chrr_measure_metadata(21, 2024)
#' get_chrr_measure_metadata("Uninsured adults", 2022)
#' get_chrr_measure_metadata("High school graduation", 2025)
#' }
#' @export
get_chrr_measure_metadata <- function(measure, release_year) {

  message("Loading measure metadata from Zenodo...")

  # Load supporting CSVs from Zenodo
  mea_years   <- read_csv_zenodo(filename = "t_measure_years.csv") %>%
    dplyr::select(year, measure_id, years_used)
  mea_compare <- read_csv_zenodo("t_measure.csv")
  cat_names <- read_csv_zenodo("t_category.csv")
  fac_names <- read_csv_zenodo("t_factor.csv")
  foc_names <- read_csv_zenodo("t_focus_area.csv")

  # --- Validate release_year dynamically ---
  valid_years <- unique(mea_years$year)
  if (!(release_year %in% valid_years)) {
    stop(
      "Invalid release_year: ", release_year, ". ",
      "Available years are: ", paste(sort(valid_years), collapse = ", "), "."
    )
  }

  # Combine measure metadata
  mea_names <- mea_years %>%
    dplyr::full_join(mea_compare, by = c("measure_id", "year"))

  # Filter by release_year
  metadata <- mea_names %>%
    dplyr::filter(year == release_year) %>%
    dplyr::left_join(foc_names, by = c("measure_parent" = "focus_area_id", "year")) %>%
    dplyr::left_join(fac_names, by = c("focus_area_parent" = "factor_id", "year")) %>%
    dplyr::left_join(cat_names, by = c("factor_parent" = "category_id", "year")) %>%
    dplyr::select(
      measure_id, measure_name, years_used, factor_name, category_name, focus_area_name,
      compare_states, compare_years, description
    ) %>%
    dplyr::filter(measure_id == measure |
                    stringr::str_to_lower(measure_name) == stringr::str_to_lower(measure)
                  ) %>%
    dplyr::slice(1) %>%  # in case multiple matches
    dplyr::mutate(
      year_comparison_note = dplyr::case_when(
        compare_years == -1 ~ "With caution",
        compare_years == 0  ~ "No",
        compare_years == 1  ~ "Yes",
        compare_years == 2  ~ "With caution",
        TRUE ~ ""
      ),
      state_comparison_note = dplyr::case_when(
        compare_states == -1 ~ "With caution",
        compare_states == 0  ~ "No",
        compare_states == 1  ~ "Yes",
        compare_states == 2  ~ "With caution",
        TRUE ~ ""
      )
    )

  # Print nicely
  if (nrow(metadata) == 0) {
    message(
      "No metadata found for the measure: ", measure, ". ",
      "Use list_chrr_measures(release_year = ", release_year, ") to see available measure_ids and measure_names for ", release_year, "."
    )
    return(invisible(NULL))
  }

  cat("\nMeasure ID:       ", metadata$measure_id, "\n")
  cat("Measure Name:     ", metadata$measure_name, "\n")
  cat("Description:      ", metadata$description, "\n")
  cat("Years Used:       ", paste(metadata$years_used, collapse = ", "), "\n")
  cat("Factor Name:      ", metadata$factor_name, "\n")
  cat("Category Name:    ", metadata$category_name, "\n")
  cat("Focus Area Name:  ", metadata$focus_area_name, "\n")
  cat("Compare across states:", metadata$state_comparison_note, "\n")
  cat("Compare across years: ", metadata$year_comparison_note, "\n")

  invisible(metadata) # still return tibble for programmatic use
}
