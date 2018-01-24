context("ML.Base.R")
described.class <- ML.Base

context(" initialize")
subject <- described.class$new()
test_that("it should inherit the logis fit class", {
  expect_true(is.a(subject, 'logisfitR6'))
})

context(" fitfunname")
test_that("it should define a default fitfunname", {
  expect_false(is.null(subject$fitfunname))
  expect_equal(subject$fitfunname, 'ml.base')
})

context(" lmclass")
test_that("it should define a default lmclass", {
  expect_false(is.null(subject$lmclass))
  expect_equal(subject$lmclass, 'ML.BaseR6')
})

context("perform_prediction")
test_that("it should call the private do.predict function", {
  subject <- described.class$new()
  expected <- 'performed prediction'
  stub(subject$perform_prediction, 'private$do.predict', function(...) expected)
  expect_equal(subject$perform_prediction(123), expected)
})

context("perform_fit")
test_that("it should call the private do.fit function", {
  subject <- described.class$new()
  expected <- 'performed fit'
  stub(subject$perform_fit, 'private$do.fit', function(...) expected)
  expect_equal(subject$perform_fit(123), expected)
})

test_that("it should call the private do.fit function which should throw by default", {
  subject <- described.class$new()
  expect_error(subject$perform_fit(123), 'The fit method needs to be inherited')
})

context("perform_update")
test_that("it should call the private do.update function", {
  subject <- described.class$new()
  expected <- 'performed update'
  stub(subject$perform_update, 'private$do.update', function(...) expected)
  expect_equal(subject$perform_update(123), expected)
})

context(" create_formula")
test_that("it should create the correct formula based on the provided Y A and W", {
  subject <- described.class$new()
  Y <- 'outcome'
  W <- c('input1', 'input2')
  result <- subject$create_formula(W, Y)
  expect_equal(result, outcome ~ input1 + input2)
})
