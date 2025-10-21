# ---- GitHub Helpers ----

#' Construct the raw GitHub URL for a file
#'
#' @param path Path to the file within the repo
#' @return Full raw URL as a string
#' @keywords internal
raw_url <- function(path) {
  paste0(
    "https://raw.githubusercontent.com/",
    GITHUB_OWNER, "/", GITHUB_REPO, "/", GITHUB_BRANCH, "/", path
  )
}

#' Construct headers for GitHub API requests
#'
#' @return Named vector of HTTP headers
#' @keywords internal
api_headers <- function() {
  h <- c(
    Accept = "application/vnd.github+json",
    "User-Agent" = "county-health-dashboard-mvp"
  )
  if (!is.na(GITHUB_TOKEN) && nzchar(GITHUB_TOKEN)) {
    h <- c(Authorization = paste("Bearer", GITHUB_TOKEN), h)
  }
  h
}

#' Read a CSV directly from a GitHub repo
#'
#' @param path Path to the CSV file within the repo
#' @return A tibble
#' @keywords internal
read_csv_github <- function(path) {
  url <- raw_url(path)
  readr::read_csv(url, show_col_types = FALSE, progress = FALSE)
}
