
context("test-web-data-service")

test_that("getBulkVectorDataByRange returns an error if vector is not a string or a list of strings", {
  expect_error(
    getBulkVectorDataByRange(vectors = function(x) { "hi" }), "must be a character or integer"
  )
  expect_error(
    getBulkVectorDataByRange(vectors = list("372648")), "must be a character or integer"
  )
  expect_error(
    getBulkVectorDataByRange(vectors = data.frame("372648")), "must be a character or integer"
  )
})

test_that("getBulkVectorDataByRange returns an error if vector is not a positive integer", {
  expect_error(
    getBulkVectorDataByRange(vectors = "employment"), "must be an integer"
  )
  expect_error(
    getBulkVectorDataByRange(vectors = "-234568"), "must be an integer >= 0"
  )
  expect_error(
    getBulkVectorDataByRange(vectors = "0.0436278"), "must be an integer"
  )
  expect_error(
    getBulkVectorDataByRange(vectors = "432789.463"), "must be an integer"
  )
})

test_that("stc_time returns string", {
  expect_type(stc_time("2019-01-01"), "character")
})


test_that("getBulkVectorDataByRange returns an error if vector is not a positive integer", {
  expect_error(
    getBulkVectorDataByRange(vectors = "employment"), "must be an integer"
  )
})

test_that("getBulkVectorDataByRange returns the right output format.", {
  # skip_on_cran()
  # skip_if_offline()
  vectors <- c("113413955","113411623")
  output <- getBulkVectorDataByRange(vectors = vectors)

  # expect_is(output, "tbl_df")
  # expect_is(output, "tbl")
  # expect_is(output, "data.frame")

})


# skip if offline?
# skip on cran?
# expect output to be json?

# expect_is(output, "tbl_df")
# expect_is(output$name, "character")
# expect_is(output$id, "character")
# stc_time
# getBulkVectorDataByRange
