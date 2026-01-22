# tests/testthat/test-downloads.R

test_that("CHR&R data downloads successfully from Zenodo", {
  skip_on_cran()  # Skip this test on CRAN
  skip_if_offline(host = "zenodo.org")  # Skip if no internet connection

  # Try downloading a small test file (e.g. measure metadata)
  expect_no_error({
    meta <- countyhealthR::get_chrr_measure_metadata(measure = 11, release_year = 2021)
  })

  expect_true(nrow(meta) > 0)
  expect_true("measure_name" %in% names(meta))
})

test_that("get_chrr_measure_data returns data for a known measure and year", {
  skip_on_cran()
  skip_if_offline(host = "zenodo.org")

  df <- countyhealthR::get_chrr_measure_data(
    geography = "state",
    measure = 9,  # Example measure ID
    release_year = 2022
  )

  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 0)
  expect_true(all(c("statecode", "value") %in% names(df)))
})
