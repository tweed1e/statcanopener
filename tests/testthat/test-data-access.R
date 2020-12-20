
context("test-data-access")

test_that("getBulkVectorDataByRange returns an error if vector is not a string or numeric", {
  expect_error(
    getBulkVectorDataByRange(vector_ids = function(x) { "hi" }), "must be a character or numeric"
  )
  expect_error(
    getBulkVectorDataByRange(vector_ids = list("372648")), "must be a character or numeric"
  )
  expect_error(
    getBulkVectorDataByRange(vector_ids = data.frame("372648")), "must be a character or numeric"
  )
})

test_that("getBulkVectorDataByRange returns an error if vector is not a positive integer", {
  expect_error(
    getBulkVectorDataByRange(vector_ids = "employment"), "must be an integer"
  )
  expect_error(
    getBulkVectorDataByRange(vector_ids = "-234568"), "must be an integer >= 0"
  )
  expect_error(
    getBulkVectorDataByRange(vector_ids = "0.0436278"), "must be an integer"
  )
  expect_error(
    getBulkVectorDataByRange(vector_ids = "432789.463"), "must be an integer"
  )
})


test_that("getBulkVectorDataByRange returns an error if vector is not a positive integer", {
  expect_error(
    getBulkVectorDataByRange(vector_ids = "employment"), "must be an integer"
  )
})

test_that("getBulkVectorDataByRange returns the right output format.", {
  # skip_on_cran()
  # skip_if_offline()
  vectors <- c("113413955","113411623")
  output <- getBulkVectorDataByRange(vector_ids = vectors, "2015-01-01", "2015-12-31")

  # expect_is(output, "tbl_df")
  # expect_is(output, "tbl")
  # expect_is(output, "data.frame")

})

# should be in utils
test_that("stc_time returns string", {
  expect_type(stc_time("2019-01-01"), "character")
})


# skip if offline?
# skip on cran?
# expect output to be json?

# expect_is(output, "tbl_df")
# expect_is(output$name, "character")
# expect_is(output$id, "character")

