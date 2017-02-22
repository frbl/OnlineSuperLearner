context("Global.R")
context(" Implements / extends the correct functions")
test_that("it should define the expit function correctly", {
  expect_true(identical(deparse(expit), deparse(plogis)))
})
test_that("it should define the logit function correctly", {
  expect_true(identical(deparse(logit), deparse(qlogis)))
})
