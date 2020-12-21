
context("test-data-access")

test_that("getChangedSeriesDataFromCubePidCoord doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getChangedSeriesDataFromCubePidCoord(35100003, "1.12.0.0.0.0.0.0.0.0")
  ))

})

test_that("getChangedSeriesDataFromVector doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getChangedSeriesDataFromVector(32164132)
  ))

})


test_that("getDataFromCubePidCoordAndLatestNPeriods doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getDataFromCubePidCoordAndLatestNPeriods(35100003, "1.12.0.0.0.0.0.0.0.0", 10)
  ))

})



test_that("getDataFromVectorsAndLatestNPeriods doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getDataFromVectorsAndLatestNPeriods("74804", 5)
  ))

})



test_that("getBulkVectorDataByRange doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getBulkVectorDataByRange("74804")
  ))

})



test_that("getFullTableDownloadCSV doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getFullTableDownloadCSV(35100003, "fr")
  ))

})




test_that("getFullTableDownloadSDMX doesn't return an http_error", {
  # skip_on_cran()
  # skip_if_offline()
  expect_false(httr::http_error(
    getFullTableDownloadSDMX(35100003)
  ))

})

