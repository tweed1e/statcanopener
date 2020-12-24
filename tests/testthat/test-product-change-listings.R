context("test-product-change-listings")

test_that("getChangedSeriesList doesn't return http error", {
  expect_false(httr::http_error(getChangedSeriesList()))
})

test_that("getChangedCubeList doesn't return http error", {
  expect_false(httr::http_error(getChangedCubeList()))
})
