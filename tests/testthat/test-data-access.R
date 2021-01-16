context("test-data-access")

# so no data access at some times.
# From 12 midnight Eastern Standard Time until 8:30am, certain methods will not return data.

test_that("getChangedSeriesDataFromCubePidCoord doesn't return an http_error", {
  skip_on_cran()
  skip_if_offline()
  # can't test this because it returns error if nothing has changed,
  # and I can't guarantee anything is going to change
  # expect_false(httr::http_error(
  #   getChangedSeriesDataFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")
  # ))
})

test_that("getChangedSeriesDataFromVector doesn't return an http_error", {
  skip_on_cran()
  skip_if_offline()

  # can't test this because it returns error if nothing has changed,
  # and I can't guarantee anything is going to change
  # expect_false(httr::http_error(
  #   getChangedSeriesDataFromVector(32164132)
  # ))
})


test_that("getDataFromCubePidCoordAndLatestNPeriods doesn't return an http_error", {
  skip_on_cran()
  skip_if_offline()
  expect_false(httr::http_error(
    getDataFromCubePidCoordAndLatestNPeriods(35100003, "1.12.0.0.0.0.0.0.0.0", 10)
  ))
})



test_that("getDataFromVectorsAndLatestNPeriods doesn't return an http_error", {
  skip_on_cran()
  skip_if_offline()
  expect_false(httr::http_error(
    getDataFromVectorsAndLatestNPeriods(32164132, 3)
  ))
})



response <- getBulkVectorDataByRange(32164132)

test_that("getBulkVectorDataByRange doesn't return an http_error", {
  skip_on_cran()
  skip_if_offline()
  expect_false(httr::http_error(response))
})

# test_that("extract_vector works", {
#   # skip_on_cran()
#   # skip_if_offline()
#
#   vector_content <- httr::content(response) # some error
#   expect_error(extract_vector(vector_content), )
#
# })





test_that("getFullTableDownloadCSV doesn't return an http_error", {
  skip_on_cran()
  skip_if_offline()
  expect_false(httr::http_error(
    getFullTableDownloadCSV(35100003, "fr")
  ))
})




test_that("getFullTableDownloadSDMX doesn't return an http_error", {
  skip_on_cran()
  skip_if_offline()
  expect_false(httr::http_error(
    getFullTableDownloadSDMX(35100003)
  ))
})
