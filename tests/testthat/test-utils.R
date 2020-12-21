
#########

context("test-check_vector_id")

test_that("check_vector_id returns an error if vector is not a string or numeric", {
  expect_error(
    check_vector_id(function(x) { "hi" }), "must be a character or numeric"
  )
  expect_error(
    check_vector_id(list("372648")), "must be a character or numeric"
  )
  expect_error(
    check_vector_id(data.frame("372648")), "must be a character or numeric"
  )
})

test_that("check_vector_id returns an error if vector is not a positive integer", {
  expect_error(
    check_vector_id("employment"), "positive integer between 1 and 10 digits"
  )
  expect_error(
    check_vector_id("-234568"), "positive integer between 1 and 10 digits"
  )
  expect_error(
    check_vector_id("0.0436278"), "positive integer between 1 and 10 digits"
  )
  expect_error(
    check_vector_id("432789.463"), "positive integer between 1 and 10 digits"
  )
})

test_that("check_vector_id returns an error if the vector is more than 10 digits", {
  expect_error(
    check_vector_id("12345678901"), "positive integer between 1 and 10 digits"
  )
})

test_that("check_vector_id returns true if it satisfies the conditions", {
  expect_true(check_vector_id("432684"))
  expect_true(check_vector_id("v432684"))
  expect_true(check_vector_id(432684))
})

#########

context("test-check_product_id")

test_that("check_product_id returns an error if the id is not either 8 or 10 digits", {
  expect_error(check_product_id("12345678901"), "integer of length 8 or 10")
})

test_that("check_product_id returns an error if the id is not either a string or numeric", {
  expect_error(check_product_id(function(x) { "hi" }), "character or numeric")
  expect_error(check_product_id(list("372648")), "character or numeric")
  expect_error(check_product_id(data.frame("372648")), "character or numeric")
})

test_that("check_product_id returns true if it satisfies the conditions", {
  expect_true(check_product_id("1234567890"))
  expect_true(check_product_id(1234567890))
})


#########

context("test-check_periods")

test_that("check_coordinate returns true if it does not satisfy the conditions", {
  expect_error(check_coordinate("1.12.0.0.0.0.0.0.0"), "10 dimensions")
  expect_error(check_coordinate("1.12"), "10 dimensions")
  expect_error(check_coordinate(1120000000), "Coordinate must be a string")
})

test_that("check_coordinate returns true if it satisfies the conditions", {
  expect_true(check_coordinate("1.12.0.0.0.0.0.0.0.0"))
})

#########

context("test-check_periods")

test_that("check_periods returns an error if the vector is not a positive integer", {
  expect_error(check_periods(-1), "a positive integer")
  expect_error(check_periods(1.1), "must be an integer")
})


test_that("check_periods returns true if it satisfies the conditions", {
  expect_true(check_periods(1))
})



#########

context("test-stc_time")

test_that("stc_time returns string", {
  expect_type(stc_time("2019-01-01"), "character")
})

test_that("stc_time returns correct format", {
  expect_equal(stc_time("2019-01-01"), "2019-01-01T00:00")
})
