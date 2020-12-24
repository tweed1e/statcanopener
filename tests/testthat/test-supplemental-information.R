context("test-supplemenmtal-information")

# so no data access at some times.
# From 12 midnight Eastern Standard Time until 8:30am, certain methods will not return data.

test_that("getCodeSets doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getCodeSets()
  ))
})
