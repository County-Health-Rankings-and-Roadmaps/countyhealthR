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
    utils::URLencode(query),
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
read_csv_zenodo <- function(
    filename,
    year = NULL,
    refresh = FALSE,
    required_cols = NULL
) {

  cache_dir <- file.path(
    rappdirs::user_cache_dir("countyhealthR_data"),
    "Cache"
  )
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (!is.null(year)) {
    year <- as.character(year)
    year_dir <- prepare_zenodo_data(year, refresh)
    record_id <- attr(year_dir, "zenodo_record_id")
    file_path <- file.path(year_dir, filename)
    file_url <- paste0(
      "https://zenodo.org/records/",
      record_id,
      "/files/",
      filename,
      "?download=1"
    )


  } else {
    file_path <- file.path(cache_dir, filename)
    file_url <- paste0(
      "https://zenodo.org/records/18157793/files/",
      filename,
      "?download=1"
    )
  }

  download_if_needed <- function() {
    message("Downloading ", filename, " from Zenodo...")
    download_zenodo_file(file_url, file_path)
  }

  # Download if missing or refresh requested
  if (!file.exists(file_path) || refresh) {
    download_if_needed()
  }

  # Attempt to read
  dat <- tryCatch(
    {
      readr::read_csv(file_path, show_col_types = FALSE, progress = FALSE)
    },
    error = function(e) {
      message("Failed to read ", filename, ". Removing cached file.")
      unlink(file_path)
      stop("Failed to parse ", filename, call. = FALSE)
    }
  )

  # 🔍 Validate structure
  if (!is.null(required_cols)) {
    missing <- setdiff(required_cols, names(dat))
    if (length(missing) > 0) {
      message(
        "Cached file ", filename,
        " is invalid (missing columns: ",
        paste(missing, collapse = ", "),
        "). Re-downloading."
      )

      unlink(file_path)
      download_if_needed()

      dat <- readr::read_csv(
        file_path,
        show_col_types = FALSE,
        progress = FALSE
      )

      missing <- setdiff(required_cols, names(dat))
      if (length(missing) > 0) {
        stop(
          "Zenodo file ", filename,
          " does not contain expected columns: ",
          paste(missing, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  dat
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

get_county_list <- function(refresh = FALSE) {
  data_dir <- tools::R_user_dir("countyhealthR", which = "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

  local_file <- file.path(data_dir, "county_fips.sas7bdat")

  if (!file.exists(local_file) || refresh) {
    url <- "https://github.com/County-Health-Rankings-and-Roadmaps/chrr_measure_calcs/raw/main/inputs/county_fips.sas7bdat"
    message("Downloading county FIPS file...")
    utils::download.file(url, local_file, mode = "wb", quiet = TRUE)
  }

  haven::read_sas(local_file)
}


get_county_choices <- function(refresh = FALSE) {
  state_lookup <- data.frame(
    state = c(state.abb, "DC"),
    state_name = c(state.name, "Washington, D.C."),
    stringsAsFactors = FALSE
  )

  get_county_list(refresh) %>%
    dplyr::left_join(state_lookup, by = "state")
}


########################################################
# measure map

# load the names datasets that are not year, county, or measure specific (ie these are always loaded)

get_measure_map <- function(refresh = FALSE) {
  cat_names <- read_csv_zenodo("t_category.csv",
                               year = max(as.integer(names(zenodo_year_records))),
                               refresh = refresh)
  fac_names <- read_csv_zenodo("t_factor.csv",
                               year = max(as.integer(names(zenodo_year_records))),
                               refresh = refresh)
  foc_names <- read_csv_zenodo("t_focus_area.csv",
                               year = max(as.integer(names(zenodo_year_records))),
                               refresh = refresh)

  mea_years <- read_csv_zenodo("t_measure_years.csv",
                               year = max(as.integer(names(zenodo_year_records))),
                               refresh = refresh) %>%
    dplyr::select(year, measure_id, years_used)

  mea_compare <- read_csv_zenodo("t_measure.csv",
                                 year = max(as.integer(names(zenodo_year_records))),
                                 refresh = refresh)

  mea_names <- mea_years %>%
    dplyr::full_join(mea_compare, by = c("measure_id", "year"))

  mea_names %>%
    dplyr::left_join(foc_names, by = c("measure_parent" = "focus_area_id", "year")) %>%
    dplyr::left_join(fac_names, by = c("focus_area_parent" = "factor_id", "year")) %>%
    dplyr::left_join(cat_names, by = c("factor_parent" = "category_id", "year")) %>%
    dplyr::mutate(
      compare_years_text = dplyr::case_when(
        compare_years == -1 ~ "Comparability across years is unknown",
        compare_years ==  0 ~ "Not comparable across years",
        compare_years ==  1 ~ "Comparable across years",
        compare_years ==  2 ~ "Use caution when comparing across years",
        TRUE ~ ""
      ),
      compare_states_text = dplyr::case_when(
        compare_states == -1 ~ "Comparability across states is unknown",
        compare_states ==  0 ~ "Not comparable across states",
        compare_states ==  1 ~ "Comparable across states",
        compare_states ==  2 ~ "Use caution when comparing across states",
        TRUE ~ ""
      )
    ) %>%
    dplyr::select(
      year, measure_id, measure_name, description, years_used,
      compare_years_text, compare_states_text,
      factor_name, category_name
    )
}


