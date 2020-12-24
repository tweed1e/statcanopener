context("test-cube-series-metadata")

# so no data access at some times.
# From 12 midnight Eastern Standard Time until 8:30am, certain methods will not return data.

test_that("getCubeMetadata doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(getCubeMetadata(35100003)))
})

test_that("getAllCubesList doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(getAllCubesList()))
})

test_that("getAllCubesListLite doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(getAllCubesListLite()))
})

test_that("getSeriesInfoFromCubePidCoord doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(getSeriesInfoFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")))
})

test_that("getSeriesInfoFromVector doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(getSeriesInfoFromVector(32164132)))
})
