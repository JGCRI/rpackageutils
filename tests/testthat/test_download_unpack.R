# License:  BSD 2-Clause, see LICENSE and DISCLAIMER files


context("download_unpack")

test_that("filename_from_url() returns expected:", {

  fn <- filename_from_url('https://fakesite/test.csv?download=1', 'csv')

  testthat::expect_identical(fn, 'test.csv')
})


test_that("download_unpack_zip() is successful:", {

  temp_dir <- tempdir()
  data_url <- 'https://zenodo.org/record/3856417/files/test.zip?download=1'

  testthat::expect_equal(download_unpack_zip(temp_dir, data_url), 0)

})


fetch_unpack_data <- function(data_directory, url_list=DATA_VERSION_URL_LIST)
test_that("fetch_unpack_data() is successful:", {

  temp_dir <- tempdir()
  data_url <- 'https://zenodo.org/record/3856417/files/test.zip?download=1'

  testthat::expect_equal(fetch_unpack_data(temp_dir, url_list=DATA_VERSION_URL_LIST), 0)

})
