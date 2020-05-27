# License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# test remote read functions

context("remote_read")

test_that("remote_read() throws errors:", {

  testthat::expect_error(remote_read('https://fakesite/test.rda?download=1'),
                         'this function only supports data table import for files saved as a .txt or .csv')

})

test_that("remote_read() works:", {

  dat <- remote_read("https://zenodo.org/record/3856417/files/test_with-header.csv?download=1")
  testthat::expect_true(is.data.frame(dat))

})
