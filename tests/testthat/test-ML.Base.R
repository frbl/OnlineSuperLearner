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
