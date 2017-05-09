context("ML.Base.R")
described.class <- ML.Base

context(" initialize")
subject <- described.class$new()
test_that("it should inherit the logis fit class", {
  expect_true(is.a(subject, 'logisfitR6'))
})

test_that("it should initialize the model as an empty list", {
  expect_true(is.a(subject$get_model,'list'))
})

context(" set_model")
test_that("it should set the model with the newly provided one", {
 subject <- described.class$new()
 model <- described.class$new()
 expect_true(is.a(subject$get_model, 'list'))
 subject$set_model(model)
 expect_equal(subject$get_model, model)
})

context(" get_model")
test_that("it should return the correct model", {
 subject <- described.class$new()
 model <- described.class$new()
 expect_true(is.a(subject$get_model, 'list'))
 subject$set_model(model)
 expect_equal(subject$get_model, model)
})

