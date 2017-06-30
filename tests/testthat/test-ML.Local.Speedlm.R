context("ML.Local.Speedlm.R")
described.class <- ML.Local.Speedlm
subject <- described.class$new()

context(" initialize")
test_that("it should initialize a new speed lm model", {
  subject <- described.class$new()
  expect_true(is(subject, 'ML.Local.Speedlm'))
})

context(" fitfunname")
test_that("it should define a fitfunname", {
  expect_false(is.null(subject$fitfunname)) 
  expect_equal(subject$fitfunname, 'speedlm-local') 
})

context(" lmclass")
test_that("it should define an lmclass", {
  expect_false(is.null(subject$lmclass)) 
  expect_equal(subject$lmclass, 'speedlm') 
})
