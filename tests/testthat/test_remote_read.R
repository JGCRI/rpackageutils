# License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# test remote read functions

context("remote_read")

test_that("remote_read() throws errors:", {

  testthat::expect_error(remote_read('https://fakesite/test.rda?download=1'),
                         'this function only supports data table import for files saved as a .txt or .csv')
  testthat::expect_error(remote_read('https://fakesite/test.csv'),
                         'url incorrectly formatted, it must end with "download=1"')

})

test_that("remote_read() works:", {

  # TODO replacethe url with a link to a smaller data frame to cut down on how long this test takes.
  dat <- remote_read("https://zenodo.org/record/3779281/files/rcmip-emissions-annual-means-v4-0-0.csv?download=1")
  testthat::expect_true(is.data.frame(dat))

})
