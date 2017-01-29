context("OnlineSuperLearner")
context(" Initialize")
test_that("it should throw an error when the provided X and Y have different dimensions", {
  expected.msg <- 'Dimensions of X and Y are not the same'
  X= data.frame(X=rnorm(10), A=rbinom(10, 1, .5))
  Y= seq(9)
  expect_that(OnlineSuperLearner(X=X, Y=Y), throws_error(expected.msg))
})

test_that("it should not throw with correct parameters", {
  X= data.frame(X=rnorm(10), A=rbinom(10, 1, .5))
  Y= seq(10)
  result <- OnlineSuperLearner(X=X, Y=Y)
  expect_that(result, is_a('OnlineSuperLearner'))
})
context(" Run")
