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

test_that("it should throw if the list of formulae does not contain relevantvariables", {
  data <- data.frame(X = seq(10), Y = seq(10))
  formulae <- list(a='a', b='b')
  algorithms <- c('a','b')
  expect_error(fit.OnlineSuperLearner(formulae = formulae, data = data, algorithms = algorithms), 
               "Argument 'rv' is neither of nor inherits class RelevantVariable: character")

})

test_that("it should throw when the provided bounds are not a boolean or a list", {
  data <- data.frame(W = seq(10), A=rbinom(10,1,0.5), Y = seq(10))
  W <- RelevantVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  A <- RelevantVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RelevantVariable$new(formula = Y ~ A + W, family = 'binomial')
  formulae <- c(W, A, Y)
  stub(fit.OnlineSuperLearner, 'smg_factory$fabricate', function(...) { throw('stop_execution') })

  expect_error(fit.OnlineSuperLearner(formulae = formulae, data = data, bounds = 'test'),
              'Bounds should either be a boolean, or a list of bounds.')
  expect_error(fit.OnlineSuperLearner(formulae = formulae, data = data, bounds = NULL),
              'Bounds should either be a boolean, or a list of bounds.')
})

test_that("it should not throw when the provided bounds are a boolean or a list", {
  data <- data.frame(W = seq(10), A=rbinom(10,1,0.5), Y = seq(10))
  W <- RelevantVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  A <- RelevantVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RelevantVariable$new(formula = Y ~ A + W, family = 'binomial')
  formulae <- c(W, A, Y)

  ## Stub a weird error which we can test. If we throw this, we know everything went through
  stub(fit.OnlineSuperLearner, 'PreProcessor$new', function(bounds) { 
    expect_is(bounds, 'list')
    throw('stop_execution') 
  })

  expect_error(fit.OnlineSuperLearner(formulae = formulae, data = data, bounds = TRUE),
              'stop_execution')

  expect_error(fit.OnlineSuperLearner(formulae = formulae, data = data, bounds = list()),
              'stop_execution')
})


test_that("it should not throw if all arguments are correct", {
  data <- data.frame(W = seq(10), A=rbinom(10,1,0.5), Y = seq(10))
  algorithms <- list()
  algorithms <- append(algorithms, list(list(algorithm = 'ML.NeuralNet',
                       params = list(nbins = c(10, 20) , online = FALSE))))

  W <- RelevantVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  A <- RelevantVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RelevantVariable$new(formula = Y ~ A + W, family = 'binomial')
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

test_that("it should create bounds when bounds is TRUE", {
  skip('Not yet tested')
})

test_that("it should not create bounds when bounds is a list of bounds, but it should normalize", {
  skip('Not yet tested')
})

test_that("it should not normalize when bounds is FALSE", {
  skip('Not yet tested')
})

context(" predict.OnlineSuperLearner")
#==========================================================
theresult <- list(osl.estimator = data.table(Y = c(1,2,3)))
expected_Y = 'therv_for_Y'
expected_X = 'therv_for_X'
subject <- list(get_relevant_variables = list('Y' = expected_Y, 'X' = expected_X))
class(subject) <- 'OnlineSuperLearner'
data <- data.frame(X = seq(10), Y = seq(10))
data <- Data.Static$new(dataset = data)

test_that("it should work with a single relevantVariable string as outcome and call the retrieve function", {
  Y = 'Y'

  ## We stub the loss function to return the data it gets, so we can check that it received the correct data
  stub(predict.OnlineSuperLearner, 'object$predict', 
    function(data, relevantVariables, sample, ...) {
      expect_false(sample)
      expect_equal(relevantVariables, expected_Y)
      called <<- TRUE
      return(list(denormalized = theresult, normalized= 'Somethingelse'))
    }
  )

  stub(predict.OnlineSuperLearner, 'object$retrieve_list_of_relevant_variables', 
    function(relevant_variables) {
      expect_equal(relevant_variables, Y)
      called2 <<- TRUE
      return(expected_Y)
    }
  )

  ## Stub the getrv function
  called <<- FALSE
  called2 <<- FALSE
  result <- predict(subject, newdata = data, Y = Y)
  expect_true(called)
  expect_true(called2)
  expect_equal(result, theresult)
})

test_that("it should work without a relevantVariable", {
  stub(predict.OnlineSuperLearner, 'object$predict', 
    function(data, relevantVariables, sample, ...) {
      expect_false(sample)
      called <<- TRUE
      return(list(denormalized = theresult, normalized= 'Somethingelse'))
    }
  )

  called <<- FALSE
  result <- predict(subject, newdata = data)
  expect_true(called)
  expect_equal(result, theresult)
})

context(" sampledata.OnlineSuperLearner")
#==========================================================
expected_Y = 'therv_for_Y'
expected_X = 'therv_for_X'
subject <- list(get_relevant_variables = list('Y' = expected_Y, 'X' = expected_X))
class(subject) <- 'OnlineSuperLearner'
data <- data.frame(X = seq(10), Y = seq(10))
data <- Data.Static$new(dataset = data)

test_that("it should work with a single relevantVariable string as outcome and call the retrieve function", {
  Y = 'Y'

  ## We stub the loss function to return the data it gets, so we can check that it received the correct data
  stub(sampledata.OnlineSuperLearner, 'object$predict', 
    function(data, relevantVariables, sample, ...) {
      expect_equal(relevantVariables, expected_Y)
      expect_true(sample)
      called <<- TRUE
      return(list(denormalized = theresult, normalized= 'Somethingelse'))
    }
  )

  stub(sampledata.OnlineSuperLearner, 'object$retrieve_list_of_relevant_variables', 
    function(relevant_variables) {
      expect_equal(relevant_variables, Y)
      called2 <<- TRUE
      return(expected_Y)
    }
  )

  ## Stub the getrv function
  called <<- FALSE
  called2 <<- FALSE
  result <- sampledata(subject, newdata = data, Y = Y)
  expect_true(called)
  expect_true(called2)
  expect_equal(result, theresult)
})

test_that("it should work without a relevantVariable", {
  stub(sampledata.OnlineSuperLearner, 'object$predict', 
    function(data, relevantVariables, sample, ...) {
      expect_true(sample)
      called <<- TRUE
      return(list(denormalized = theresult, normalized= 'Somethingelse'))
    }
  )

  called <<- FALSE
  result <- sampledata(subject, newdata = data)
  expect_true(called)
  expect_equal(result, theresult)
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
