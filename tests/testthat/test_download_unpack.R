# License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# test common functions

context("download_unpack")

test_that("filename_from_url() returns expected:", {

  fn <- filename_from_url('https://fakesite/test.csv?download=1', 'csv')

  testthat::expect_identical(fn, 'test.csv')
})


test_that("download_unpack_zip() returns error:", {

  error_string <- "cannot open URL 'b'"
  warning_string <- "URL 'http://b/': status was 'Couldn't resolve host name'"

  testthat::expect_error(download_unpack_zip('a', 'b'), error_string, ignore.case = TRUE)

})
