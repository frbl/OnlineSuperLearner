context("mock_function")

test_that("it returns the correct string when the parameter is true", {
  expect_equal(mock_function(TRUE), "param was true")
})

test_that("it returns the correct string when the parameter is false", {
  expect_equal(mock_function(FALSE), "param was false")
})

test_that("it returns the correct string when no params are provided", {
  expect_equal(mock_function(), "param was true")
})
