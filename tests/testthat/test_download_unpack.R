# License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# test common functions

context("download_unpack")

test_that("filename_from_url() returns expected:", {

  fn <- filename_from_url('https://fakesite/test.csv?download=1', 'csv')

  testthat::expect_identical(fn, 'test.csv')
})
