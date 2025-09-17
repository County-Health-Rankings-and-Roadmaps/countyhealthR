#' List available CHRR data years
#'
#' Uses the GitHub API to find available years in the CHRR measure calcs repo.
#'
#' @return A vector of available years (numeric).
#' @export
#'
#' @examples
#' \dontrun{
#' list_chrr_years()
#' }
list_chrr_years <- function() {
  url <- "https://api.github.com/repos/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/contents/relational_data"
  res <- httr::GET(url)
  httr::stop_for_status(res)
  content <- httr::content(res)

  years <- vapply(content, function(x) x$name, character(1))
  years <- years[grepl("^[0-9]{4}$", years)]
  as.integer(years)
}
