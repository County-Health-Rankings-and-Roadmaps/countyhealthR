# ---- Zenodo Helpers ----

#' Download and prepare CHR&R data for a given release year from Zenodo
#'
#' @param release_year Numeric year (e.g. 2022)
#' @param refresh Logical; if TRUE, re-download even if cached
#' @return Path to the extracted data directory for that year
#' @keywords internal
prepare_zenodo_data <- function(release_year, refresh = FALSE) {
  if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

  zip_name <- paste0(release_year, ".zip")
  zip_url  <- paste0(ZENODO_BASE_URL, zip_name)
  zip_path <- file.path(CACHE_DIR, zip_name)
  extract_dir <- file.path(CACHE_DIR, as.character(release_year))

  # Optionally refresh cache
  if (refresh && dir.exists(extract_dir)) {
    message("Refreshing cached data for ", release_year, "...")
    unlink(c(zip_path, extract_dir), recursive = TRUE)
  }

  # Download if not cached
  if (!file.exists(zip_path)) {
    message("Downloading CHR&R ", release_year, " data from Zenodo...")
    utils::download.file(
      url = zip_url,
      destfile = zip_path,
      mode = "wb",
      quiet = FALSE,
      timeout = 600
    )
  } else {
    message("Using cached archive for ", release_year)
  }

  # Unzip if needed
  if (!dir.exists(extract_dir)) {
    message("Unzipping data for ", release_year, " (first time only)...")
    utils::unzip(zip_path, exdir = extract_dir)
  }

  # 🔍 Detect where the CSV files actually are
  files <- list.files(extract_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    stop("No CSV files found after unzipping ", release_year, ". Check the ZIP structure.")
  }

  # If files are nested (e.g., extract_dir/2022/...), return that subfolder
  csv_dir <- unique(dirname(files))
  # If multiple subfolders, pick the first (warn if inconsistent)
  if (length(csv_dir) > 1) {
    warning("Multiple subdirectories detected in ", release_year, ". Using first.")
    csv_dir <- csv_dir[1]
  }

  return(csv_dir)
}


#' Read a CSV from the unzipped Zenodo folder
#'
#' @param release_year Numeric year (e.g. 2022)
#' @param filename Name of the CSV file to read
#' @return A tibble
#' @keywords internal
read_csv_zenodo <- function(filename, year = NULL, refresh = FALSE) {
  cache_dir <- file.path(rappdirs::user_cache_dir("countyhealthR_data"), "Cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(year)) {
    # Year-specific ZIP (always YEAR.zip)
    zip_name <- paste0(year, ".zip")
    zip_url  <- paste0("https://zenodo.org/records/17537523/files/", zip_name)
    zip_path <- file.path(cache_dir, zip_name)
    extract_dir <- file.path(cache_dir, as.character(year))

    # Download ZIP if missing or refresh
    if (!file.exists(zip_path) || refresh) {
      message("Downloading ", zip_name, " from Zenodo...")
      utils::download.file(zip_url, zip_path, mode = "wb", quiet = FALSE, timeout = 600)
    } else {
      message("Using cached archive for ", year)
    }

    # Unzip if not already extracted or refresh
    if (!dir.exists(extract_dir) || refresh) {
      message("Unzipping ", zip_name, "...")
      utils::unzip(zip_path, exdir = extract_dir)
    }

    # File path inside extracted folder
    file_path <- file.path(extract_dir, filename)
    if (!file.exists(file_path)) {
      # sometimes ZIP contains a subfolder named YEAR
      subfolder_path <- file.path(extract_dir, as.character(year), filename)
      if (file.exists(subfolder_path)) {
        file_path <- subfolder_path
      } else {
        stop("File not found inside ZIP: ", filename)
      }
    }

  } else {
    # Root-level files (t_measure_years.csv, t_measure.csv, etc.)
    file_path <- file.path(cache_dir, filename)
    url <- paste0("https://zenodo.org/records/17537523/files/", filename)

    if (!file.exists(file_path) || refresh) {
      message("Downloading root-level file from Zenodo...")
      utils::download.file(url, file_path, mode = "wb", quiet = FALSE)
    }
  }

  readr::read_csv(file_path, show_col_types = FALSE)
}
