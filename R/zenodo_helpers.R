# ---- Zenodo Helpers ----

# DOI that always points to the *latest* version of CHRR relational data on zenodo
ZENODO_DOI <- "10.5281/zenodo.17419266"

# Function: dynamically fetch the latest Zenodo record ID via the API
get_latest_zenodo_id <- function(doi = ZENODO_DOI) {
  api_url <- paste0("https://zenodo.org/api/records/?q=doi:", doi)
  tmpfile <- tempfile(fileext = ".json")

  tryCatch({
    utils::download.file(api_url, tmpfile, quiet = TRUE)
    json <- jsonlite::fromJSON(tmpfile)

    # Extract record ID for latest version
    if (!is.null(json$hits$hits[[1]]$id)) {
      latest_id <- json$hits$hits[[1]]$id
      return(latest_id)
    } else {
      warning("Could not retrieve latest Zenodo record ID; using fallback ID.")
      return("17537523") # fallback to your known latest version
    }
  }, error = function(e) {
    warning("Error fetching latest Zenodo ID from API: ", e$message)
    return("17537523") # fallback
  })
}

# Cache the ID once per session
ZENODO_RECORD_ID <- get_latest_zenodo_id()

# Build the base URL dynamically
ZENODO_BASE_URL <- paste0("https://zenodo.org/records/", ZENODO_RECORD_ID, "/files/")

# Cache directory for storing downloaded data
CACHE_DIR <- file.path(rappdirs::user_cache_dir("countyhealthR_data"), "Cache")

# List of known metadata files stored at the Zenodo root
ROOT_LEVEL_FILES <- c(
  "t_measure.csv",
  "t_measure_years.csv",
  "t_factor.csv",
  "t_focus_area.csv",
  "t_category.csv"
)


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

  # Detect where CSVs actually are
  files <- list.files(extract_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    stop("No CSV files found after unzipping ", release_year, ". Check the ZIP structure.")
  }

  csv_dir <- unique(dirname(files))
  if (length(csv_dir) > 1) {
    warning("Multiple subdirectories detected in ", release_year, ". Using first.")
    csv_dir <- csv_dir[1]
  }

  return(csv_dir)
}


#' Read a CSV from Zenodo (root-level or year-specific)
#'
#' @param filename The CSV file name
#' @param year Optional numeric year (for year-specific files)
#' @param refresh Logical; if TRUE, re-download even if cached
#' @return A tibble
#' @keywords internal
read_csv_zenodo <- function(filename, year = NULL, refresh = FALSE) {
  cache_dir <- CACHE_DIR
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Case 1: Root-level metadata files ---
  if (filename %in% ROOT_LEVEL_FILES) {
    file_path <- file.path(cache_dir, filename)
    file_url  <- paste0(ZENODO_BASE_URL, filename)

    if (!file.exists(file_path) || refresh) {
      message("Downloading shared metadata file from Zenodo...")
      utils::download.file(file_url, file_path, mode = "wb", quiet = FALSE)
    }

    return(readr::read_csv(file_path, show_col_types = FALSE))
  }

  # --- Case 2: Year-specific files ---
  if (is.null(year)) {
    stop("Must provide `year` for year-specific data files like county/state/national data.")
  }

  zip_name <- paste0(year, ".zip")
  zip_url  <- paste0(ZENODO_BASE_URL, zip_name)
  zip_path <- file.path(cache_dir, zip_name)
  extract_dir <- file.path(cache_dir, as.character(year))

  # Download year ZIP if needed
  if (!file.exists(zip_path) || refresh) {
    message("Downloading ", zip_name, " from Zenodo...")
    utils::download.file(zip_url, zip_path, mode = "wb", quiet = FALSE, timeout = 600)
  } else {
    message("Using cached archive for ", year)
  }

  # Unzip if not yet extracted
  if (!dir.exists(extract_dir) || refresh) {
    message("Unzipping ", zip_name, "...")
    utils::unzip(zip_path, exdir = extract_dir)
  }

  file_path <- file.path(extract_dir, filename)
  if (!file.exists(file_path)) {
    subfolder_path <- file.path(extract_dir, as.character(year), filename)
    if (file.exists(subfolder_path)) {
      file_path <- subfolder_path
    } else {
      stop("File not found in ", year, ".zip: ", filename)
    }
  }

  readr::read_csv(file_path, show_col_types = FALSE)
}
