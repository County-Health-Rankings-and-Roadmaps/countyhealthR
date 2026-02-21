#' @importFrom dplyr filter mutate select arrange slice
#' @importFrom stringr str_detect str_to_lower
#' @importFrom rlang .data
#' @importFrom magrittr %>%

utils::globalVariables(c(".env", "county", "countycode", "description",
                         "fipscode", "measure_id", "measure_name",
                         "state.abb", "state.name", "state_fips",
                         "statecode", "year"))


.onLoad <- function(libname, pkgname) {
  # Example: set package options
  options(countyhealthR.verbose = TRUE)
}


.onAttach <- function(libname, pkgname) {
  packageStartupMessage("countyhealthR loaded. Use list_chrr_measures() to see all measures of health available for a specified release_year.")
}
