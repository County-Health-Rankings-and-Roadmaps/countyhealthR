# tests/testthat/test-downloads.R

library(testthat)

context("CHR&R data downloads and internal references")

# Skip on CRAN and offline
skip_if_offline <- function(host = "zenodo.org") {
  if (inherits(try(curl::nslookup(host), silent = TRUE), "try-error")) {
    skip(paste("Offline or cannot reach", host))
  }
}

# ---------------------------------------------
# Internal county list
# ---------------------------------------------
test_that("internal county list loads correctly", {
  x <- countyhealthR:::.get_county_list_internal()

  expect_true(is.data.frame(x))
  expect_gt(nrow(x), 3000)
  expect_named(x, c("statecode", "countycode", "state", "county", "fipscode"))
})


# ---------------------------------------------
# get_chrr_county_data
# ---------------------------------------------
test_that("get_chrr_county_data works for multiple input types", {
  skip_on_cran()
  skip_if_offline()

  examples <- list(
    list(state = "WI", county = "dane county", release_year = 2024),
    list(state = "Wisconsin", county = "025", release_year = 2023),
    list(state = "55", county = "DANE", release_year = 2022),
    list(state = "wi", county = "Dane "),                 # release_year missing
    list(state = "WI", county = NULL, release_year = 2024),
    list(state = "Wisconsin", county = "000", release_year = 2023),
    list(state = "55", release_year = 2018),               # county missing
    list(state = "WI")                                     # both county & year missing
  )

  for (ex in examples) {

    df <- do.call(countyhealthR::get_chrr_county_data, ex)

    expect_s3_class(df, "data.frame")
    expect_gt(nrow(df), 0)

    # state_fips and measure_name should always exist
    expect_true(all(c("state_fips", "measure_name") %in% names(df)))

    # county_fips should only exist for county-level requests
    if (!is.null(ex$county) &&
        !is.na(ex$county) &&
        toupper(ex$county) != "000") {
      expect_true("county_fips" %in% names(df))
    }
  }
})

# ---------------------------------------------
# get_chrr_measure_data
# ---------------------------------------------
test_that("get_chrr_measure_data returns valid data for different geographies", {
  skip_on_cran()
  skip_if_offline()

  # County-level
  county_data <- countyhealthR::get_chrr_measure_data(
    geography = "county",
    measure = 21,
    release_year = 2023
  )
  expect_s3_class(county_data, "data.frame")
  expect_gt(nrow(county_data), 0)
  expect_true("county_fips" %in% names(county_data))

  # State-level
  state_data <- countyhealthR::get_chrr_measure_data(
    geography = "state",
    measure = "insufficient sleep",
    release_year = 2022
  )
  expect_s3_class(state_data, "data.frame")
  expect_gt(nrow(state_data), 0)
  expect_true("state_fips" %in% names(state_data))

  # National-level
  nat_data <- countyhealthR::get_chrr_measure_data(
    geography = "national",
    measure = "uninsured",
    release_year = 2024
  )
  expect_s3_class(nat_data, "data.frame")
  expect_gt(nrow(nat_data), 0)
  expect_true("raw_value" %in% names(nat_data))

  # Verify that default release_year is working
  default_releaseyr <- countyhealthR::get_chrr_measure_data(
    geography = "national",
    measure = "unemployment"
  )
  expect_s3_class(default_releaseyr, "data.frame")
  expect_gt(nrow(default_releaseyr), 0)
  expect_true("raw_value" %in% names(default_releaseyr))
})

# ---------------------------------------------
# get_chrr_measure_metadata
# ---------------------------------------------
test_that("get_chrr_measure_metadata returns metadata correctly", {
  skip_on_cran()
  skip_if_offline()

  meta1 <- countyhealthR::get_chrr_measure_metadata(21, 2024)
  meta2 <- countyhealthR::get_chrr_measure_metadata("Uninsured adults", 2022)
  meta3 <- countyhealthR::get_chrr_measure_metadata("High school graduation", 2025)
  meta4 <- countyhealthR::get_chrr_measure_metadata("social associations")


  for (meta in list(meta1, meta2, meta3, meta4)) {
    expect_s3_class(meta, "data.frame")
    expect_gt(nrow(meta), 0)
    expect_true("measure_name" %in% names(meta))
  }
})

# ---------------------------------------------
# list_chrr_measures
# ---------------------------------------------
test_that("list_chrr_measures lists available measures", {
  skip_on_cran()
  skip_if_offline()

  measures1 <- countyhealthR::list_chrr_measures(2023)
  measures2 <- countyhealthR::list_chrr_measures(release_year = "2019")
  measures3 <- countyhealthR::list_chrr_measures()

  for (msr in list(measures1, measures2, measures3)) {
    expect_s3_class(msr, "data.frame")
    expect_gt(nrow(msr), 0)
    expect_true(all(c("measure_id", "measure_name") %in% names(msr)))
  }
})
