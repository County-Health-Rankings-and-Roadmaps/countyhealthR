# ---- Zenodo Helpers ----

# Base cache directory
CACHE_DIR <- file.path(rappdirs::user_cache_dir("countyhealthR_data"), "Cache")

# Base Zenodo record ID (latest version)
ZENODO_RECORD_ID <- "17419266"
ZENODO_BASE_URL <- paste0("https://zenodo.org/records/", ZENODO_RECORD_ID, "/files/")

# Prepare data for a given year (downloads ZIP if needed)
prepare_zenodo_data <- function(release_year, refresh = FALSE) {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

  zip_name <- paste0(release_year, ".zip")
  zip_url  <- paste0(ZENODO_BASE_URL, zip_name)
  zip_path <- file.path(CACHE_DIR, zip_name)
  extract_dir <- file.path(CACHE_DIR, as.character(release_year))

  # Refresh cache if requested
  if (refresh && dir.exists(extract_dir)) {
    message("Refreshing cached data for ", release_year, "...")
    unlink(c(zip_path, extract_dir), recursive = TRUE)
  }

  # Download ZIP if missing or refresh
  if (!file.exists(zip_path)) {
    message("Downloading CHR&R ", release_year, " ZIP from Zenodo...")
    tryCatch(
      utils::download.file(zip_url, zip_path, mode = "wb", timeout = 600),
      error = function(e) stop("Failed to download ZIP: ", zip_url)
    )
  } else {
    message("Using cached archive for ", release_year)
  }

  # Unzip if needed
  if (!dir.exists(extract_dir) || refresh) {
    message("Unzipping ", zip_name, "...")
    utils::unzip(zip_path, exdir = extract_dir)
  }

  # Detect CSV subfolder (sometimes ZIP has nested YEAR folder)
  files <- list.files(extract_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) stop("No CSV files found in ZIP for ", release_year)

  csv_dir <- unique(dirname(files))
  if (length(csv_dir) > 1) {
    warning("Multiple subdirectories detected. Using first.")
    csv_dir <- csv_dir[1]
  }

  return(csv_dir)
}

#' Read a CSV from Zenodo
#'
#' Handles both year-specific data files (inside YEAR.zip) and root-level metadata files.
#'
#' @param filename Name of the CSV file to read
#' @param year Numeric year if the file is year-specific (e.g., 2022)
#' @param refresh Logical; if TRUE, re-download
#' @return A tibble
#' @keywords internal
read_csv_zenodo <- function(filename, year = NULL, refresh = FALSE) {
  cache_dir <- file.path(rappdirs::user_cache_dir("countyhealthR_data"), "Cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(year)) {
    # Year-specific file: unzip YEAR.zip first
    data_dir <- prepare_zenodo_data(year, refresh)
    file_path <- file.path(data_dir, filename)

    if (!file.exists(file_path)) {
      stop("Year-specific file not found inside ZIP: ", filename)
    }

  } else {
    # Root-level metadata file
    file_path <- file.path(cache_dir, filename)
    root_url <- paste0("https://zenodo.org/records/17537523/files/", filename)

    if (!file.exists(file_path) || refresh) {
      message("Downloading root-level file from Zenodo...")
      utils::download.file(root_url, file_path, mode = "wb", quiet = FALSE)
    }
  }

  readr::read_csv(file_path, show_col_types = FALSE)
}
