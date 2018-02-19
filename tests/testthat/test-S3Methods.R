library(mockery)
context("S3Methods")
#==========================================================

context(" fit.OnlineSuperLearner")
#==========================================================
test_that("it should throw if the list of formulaee is not a list", {
  data <- data.frame(X = seq(10), Y = seq(10))
  formulae <- 'not a list'
  algorithms <- c('a','b')
  expect_error(fit.OnlineSuperLearner(formulae = formulae, data = data, algorithms = algorithms), 
               "Argument 'formulae' is neither of nor inherits class list: character")
})

test_that("it should throw if the list of formulae does not contain randomvariables", {
  data <- data.frame(X = seq(10), Y = seq(10))
  formulae <- list(a='a', b='b')
  algorithms <- c('a','b')
  expect_error(fit.OnlineSuperLearner(formulae = formulae, data = data, algorithms = algorithms), 
               "Argument 'rv' is neither of nor inherits class RandomVariable: character")
})

test_that("it should not throw if all arguments are correct", {
  data <- data.frame(W = seq(10), A=rbinom(10,1,0.5), Y = seq(10))
  algorithms <- list()
  algorithms <- append(algorithms, list(list(algorithm = 'ML.Local.Speedlm',
                       params = list(nbins = c(10, 20) , online = FALSE))))

  W <- RandomVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  A <- RandomVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RandomVariable$new(formula = Y ~ A + W, family = 'binomial')
  formulae <- c(W, A, Y)

  ## Mock the fit function to spare us some time
  stub(fit.OnlineSuperLearner, 'osl$fit', 
    function(data, ...) {
      expect_is(data, 'Data.Static')
      called <<- TRUE
    }
  )

  called <<- FALSE
  expect_error(result <- fit.OnlineSuperLearner(formulae = formulae, data = data, algorithms = algorithms), NA)
  expect_true(called)
  expect_is(result, 'OnlineSuperLearner')
})


context(" predict.OnlineSuperLearner")
#==========================================================
expected_Y = 'therv_for_Y'
expected_X = 'therv_for_X'
subject <- list(get_random_variables = list('Y' = expected_Y, 'X' = expected_X))
class(subject) <- 'OnlineSuperLearner'
data <- data.frame(X = seq(10), Y = seq(10))
data <- Data.Static$new(dataset = data)

test_that("it should throw if the provided Y is a list, but not long enough", {
  subject <- mock('osl')
  class(subject) <- 'OnlineSuperLearner'
  data <- Data.Base$new()
  expect_error(predict(subject, newdata = data, Y = list()),
               'There should be at least one entry in the outcomes specified')

})

test_that("it should work with a single randomVariable as outcome", {
  Y <- RandomVariable$new(formula = Y ~ A + W, family = 'binomial')

  ## We stub the loss function to return the data it gets, so we can check that it received the correct data
  stub(predict.OnlineSuperLearner, 'object$predict', 
    function(data, randomVariables, ...) {
      expect_is(randomVariables, 'list')
      expect_equal(randomVariables[[1]], Y)
      called <<- TRUE
    }
  )

  ## Stub the getrv function
  called <<- FALSE
  predict(subject, newdata = data, Y = Y)
  expect_true(called)
})

test_that("it should work with a single randomVariable string as outcome", {
  Y = 'Y'

  ## We stub the loss function to return the data it gets, so we can check that it received the correct data
  stub(predict.OnlineSuperLearner, 'object$predict', 
    function(data, randomVariables, ...) {
      expect_equal(randomVariables, expected_Y)
      called <<- TRUE
    }
  )

  ## Stub the getrv function
  called <<- FALSE
  predict(subject, newdata = data, Y = Y)
  expect_true(called)
})

test_that("it should work with a list of randomVariables as outcome", {
  Y <- list(
    RandomVariable$new(formula = Y ~ A + W, family = 'binomial'),
    RandomVariable$new(formula = Y ~ A + W, family = 'binomial')
  )

  stub(predict.OnlineSuperLearner, 'object$predict', 
    function(data, randomVariables, ...) {
      expect_is(randomVariables, 'list')
      for(i in 1:length(randomVariables)) {
        expect_equal(randomVariables[[i]], Y[[i]])
      }
      called <<- TRUE
    }
  )

  called <<- FALSE
  predict(subject, newdata = data, Y = Y)
  expect_true(called)
})

test_that("it should work with a list of randomVariable strings as outcome", {
  Y <- list('X', 'Y')

  stub(predict.OnlineSuperLearner, 'object$predict', 
    function(data, randomVariables, ...) {
      expect_is(randomVariables, 'list')
      expect_equal(randomVariables[[1]], expected_X)
      expect_equal(randomVariables[[2]], expected_Y)
      called <<- TRUE
    }
  )

  called <<- FALSE
  predict(subject, newdata = data, Y = Y)
  expect_true(called)
})

test_that("it should work without a randomVariable", {

})

context(" summary.OnlineSuperLearner")
#==========================================================
test_that("it should call the info function of the OSL object", {
  expected = 'thisistheexpectedvalueoftheinfofunction'
  subject <- list(info = {
    expected
  })

  class(subject) <- 'OnlineSuperLearner'
  result <- summary(subject)
  expect_equal(result, expected)
})

test_that("it should throw if the provided object is not an OnlineSuperLearner instance", {
  subject <- list(info = { 'result' })
  class(subject) <- 'NotAnOsl'
  expect_error(summary.OnlineSuperLearner(subject), 
               'The provided object is not an online superlearner instance')
})
