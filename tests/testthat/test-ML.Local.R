context("ML.Local.R")
context(" createFormula")
test_that("it should create the correct formula based on the provided Y A and W", {
  subject <- ML.Local$new()
  Y <- 'outcome'
  A <- 'intervention'
  W <- c('input1', 'input2')
  result <- subject$createFormula(Y,A,W)
  expect_equal(result, 'outcome ~ intervention + input1 + input2' )
})

test_that("it should be able to deal with an empty intervention", {
  subject <- ML.Local$new()
  Y <- 'outcome'
  A <- c()
  W <- c('input1', 'input2')
  result <- subject$createFormula(Y,A,W)
  expect_equal(result, 'outcome ~ input1 + input2' )
})
