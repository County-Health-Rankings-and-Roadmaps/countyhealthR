# R/cache_helpers.R

#' Reset countyhealthR cached data
#'
#' Deletes all locally cached Zenodo files.
#' Useful for forcing fresh downloads or debugging cloud installs.
#'
#' @export
countyhealthR_reset_cache <- function() {
  cache_dir <- rappdirs::user_cache_dir("countyhealthR_data")
  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    message("countyhealthR cache cleared.")
  } else {
    message("No cache found.")
  }
}
