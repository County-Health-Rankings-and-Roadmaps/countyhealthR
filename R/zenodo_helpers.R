### Zenodo year records
zenodo_year_records <- c(
  "2010" = "18157682",
  "2011" = "18157793",
  "2012" = "18331277",
  "2013" = "18331501",
  "2014" = "18331638",
  "2015" = "18331640",
  "2016" = "18331650",
  "2017" = "18331653",
  "2018" = "18331964",
  "2019" = "18331968",
  "2020" = "18331971",
  "2021" = "18331979",
  "2022" = "18331986",
  "2023" = "18331991",
  "2024" = "18331995",
  "2025" = "18332002"
)



### resolve zenodo record
resolve_zenodo_record <- function(year, concept_doi = "10.5281/zenodo.18157681") {

  query <- paste0("county health rankings ", year)

  if (!is.null(concept_doi)) {
    query <- paste(query, concept_doi)
  }

  url <- paste0(
    "https://zenodo.org/api/records?q=",
    URLencode(query),
    "&sort=mostrecent"
  )

  resp <- jsonlite::fromJSON(url)

  if (length(resp$hits$hits) == 0) {
    stop("No Zenodo records found for year ", year)
  }

  # Take most recent release
  resp$hits$hits[[1]]$id
}

### Read CSV from Zenodo (robust)
read_csv_zenodo <- function(filename, year = NULL, refresh = FALSE) {
  cache_dir <- file.path(rappdirs::user_cache_dir("countyhealthR_data"), "Cache")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if(!is.null(year)) {
    year <- as.character(year)
    year_dir <- prepare_zenodo_data(year, refresh)
    record_id <- attr(year_dir, "zenodo_record_id")
    file_path <- file.path(year_dir, filename)
    file_url <- paste0("https://zenodo.org/records/", record_id, "/files/", filename, "?download=1")

    # Print citation
    print_zenodo_citation(year)

    if(!file.exists(file_path) || refresh) {
      message("Downloading ", filename, " (", year, ") from Zenodo...")
      download_zenodo_file(file_url, file_path)
    }
  } else {
    file_path <- file.path(cache_dir, filename)
    root_url <- paste0("https://zenodo.org/records/18157793/files/", filename, "?download=1")
    if(!file.exists(file_path) || refresh) {
      message("Downloading root-level file from Zenodo...")
      download_zenodo_file(root_url, file_path)
    }
  }

  readr::read_csv(file_path, show_col_types = FALSE)
}


### prepare zenodo data
prepare_zenodo_data <- function(release_year, refresh = FALSE) {

  release_year <- as.character(release_year)

  cache_dir <- file.path(
    rappdirs::user_cache_dir("countyhealthR_data"),
    "Cache"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  year_dir <- file.path(cache_dir, release_year)

  # Handle refresh
  if (refresh && dir.exists(year_dir)) {
    message("Refreshing cached data for ", release_year, "...")
    unlink(year_dir, recursive = TRUE)
  }

  dir.create(year_dir, showWarnings = FALSE)

  # Resolve Zenodo record (pinned or evolving)
  record_id <- zenodo_year_records[[release_year]]

  if (is.null(record_id)) {
    message("Resolving latest Zenodo release for year ", release_year, "...")
    record_id <- resolve_zenodo_record(
      year = release_year,
      concept_doi = "10.5281/zenodo.18157681"
    )
  }

  # Store record ID for downstream use
  attr(year_dir, "zenodo_record_id") <- record_id

  return(year_dir)
}


### Robust download function using httr
download_zenodo_file <- function(file_url, file_path, retries = 3, timeout_sec = 300) {
  for(i in seq_len(retries)) {
    try({
      resp <- httr::GET(
        file_url,
        httr::write_disk(file_path, overwrite = TRUE),
        httr::timeout(timeout_sec)
      )
      httr::stop_for_status(resp)

      # Verify that file is non-empty
      if(file.exists(file_path) && file.info(file_path)$size > 0) {
        return(TRUE)
      } else stop("Downloaded file is empty.")
    }, silent = TRUE)

    if(i < retries) message("Retrying download... attempt ", i + 1)
  }
  stop("Failed to download ", file_url, " after ", retries, " attempts.")
}




# print zenodo citation
print_zenodo_citation <- function(year, zenodo_record_id = NULL, concept_doi = "10.5281/zenodo.18157681") {
  year <- as.character(year)
  zenodo_record_id <- zenodo_year_records[[year]]

  if (!is.null(zenodo_record_id)) {
    message(
      "Citation for CHR&R data (", year, "):\n",
      "County Health Rankings & Roadmaps. Zenodo. DOI: https://doi.org/", concept_doi,
      " (Zenodo record ID: ", zenodo_record_id, ")"
    )
  } else {
    # fallback if the year isn't in the vector
    message(
      "Citation for CHR&R data (", year, "):\n",
      "County Health Rankings & Roadmaps. Zenodo. DOI: https://doi.org/", concept_doi
    )
  }
}


# get county and state names for each look up and printing

get_county_list <- function() {
  url <- "https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/inputs/county_fips.sas7bdat"
  temp <- tempfile(fileext = ".sas7bdat")
  download.file(url, temp, mode = "wb")  # wb = write binary
  counties <- haven::read_sas(temp)

  return(counties)
}

# add the full state name in
state_lookup <- data.frame(
  state = state.abb,
  state_name = state.name,
  stringsAsFactors = FALSE
)
#need DC too
state_lookup <- rbind(
  state_lookup,
  data.frame(
    state = "DC",
    state_name = "Washington, D.C.",
    stringsAsFactors = FALSE
  )
)

county_choices <- get_county_list() %>%
  dplyr::left_join(state_lookup, by = "state")


