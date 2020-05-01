# License:  BSD 2-Clause, see LICENSE and DISCLAIMER files

# test common functions

context("common")

test_that("version_as_character() produces character class:", {

  version_class <- version_as_character() %>%
    class()

  testthat::expect_identical(version_class, 'character')
})
