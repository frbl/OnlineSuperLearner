context("ML.H2O.R")

described.class <- ML.H2O

context(" get_checkpoint")
test_that("it should return the model ID if a checkpoint is available", {
  #subject <- described.class$new() 
  #m.fit <- list()
  #m.fit['coef@model_id'] = 1
  #result <- subject$get_checkpoint(m.fit)
  #expect_equal(result, 1)
  skip('Unclear how to test')
})

test_that("it should return null if no checkpointis proovided", {
  subject <- described.class$new() 
  result <- subject$get_checkpoint(NULL)
  expect_null(result)
})

test_that("it should return null if m.fit has no coeff", {
  subject <- described.class$new() 
  m.fit <- list(a = 1, b= 2)
  result <- subject$get_checkpoint(m.fit)
  expect_null(result)
})
