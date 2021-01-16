context("test-product-change-listings")

test_that("getChangedSeriesList doesn't return http error", {
  skip_on_cran()
  skip_if_offline()
  expect_false(httr::http_error(getChangedSeriesList()))
})

test_that("getChangedCubeList doesn't return http error", {
  skip_on_cran()
  skip_if_offline()
  expect_false(httr::http_error(getChangedCubeList("2018-01-01")))
})
