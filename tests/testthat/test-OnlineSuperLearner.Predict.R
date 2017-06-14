library(mockery)

context("OnlineSuperLearner.Predict.R")
described.class <- OnlineSuperLearner.Predict
#--------------------------------------------------------------#
context(" initialize")
test_that("it should accept no pre_processor to be passed in", {
 expect_error(described.class$new(pre_processor = NULL), NA)
})

test_that("it should throw if the pre_processor passed in is not a preprocessor", {
 expect_error(described.class$new(pre_processor = 'not a preprocessor'), 
              "Argument 'pre_processor' is neither of nor inherits class PreProcessor")
})

test_that("it should initialize with pre_processor", {
 pre_processor <- PreProcessor$new(bounds = list(1))
 result <- described.class$new(pre_processor = pre_processor)
 expect_true(is(result, 'OnlineSuperLearner.Predict'))
})

context(" predict")
test_that("it should return NA if no estimators were fit", {
  data <- data.frame(a=c(1,2,3),b= c(3,2,1))
  OnlineSuperLearner_mock <- list(is_fitted=FALSE)
  subject <- described.class$new(pre_processor = NULL)
  expect_equal(subject$predict(osl = OnlineSuperLearner_mock, data, randomVariables = c('a')), NA)
})

test_that("it should throw if all estimators are disabled", {
  data <- data.frame(a=c(1,2,3),b= c(3,2,1))
  OnlineSuperLearner_mock <- list(is_fitted=TRUE)
  subject <- described.class$new(pre_processor = NULL)
  expect_error(subject$predict(osl = OnlineSuperLearner_mock, data, randomVariables = c('a'),
                               all_estimators = FALSE, discrete = FALSE, continuous = FALSE), 
               'At least one option should be selected: discrete, all_estimators, or continuous')
})


test_that("it should call the dosl if enabled", {
  # Not elaborating because the stubbing doesn't seem to work the way I expect it to work...
  #osl <- list(is_fitted=TRUE, fits_dosl = TRUE)
  #mocked_function = function() {return('called')}
  #subject <- described.class$new(pre_processor = NULL)
  #stub(subject$predict, 'self$predict_dosl', 'stub has been called')

  #print(subject$predict)
  #result <- subject$predict(osl = osl, data, randomVariables = c('a'),
                #all_estimators = FALSE, discrete = TRUE, continuous = FALSE,
                #sample = FALSE, plot = FALSE, denormalize = FALSE)
  #print(result)
  
})

context(" predict_osl")
test_that("it should update the predictions according to the osl_weights", {
  randomVariables <- list(list(getY='Y', getFamily='gaussian'))

  nobs <- 10
  subject <- described.class$new(pre_processor = NULL)

  df <- data.table(Y=rep(1, nobs))
  predictions <- list(a=df, b=df+1, c=df+2)
  weights <- c(0.2, 0.5, 0.3)
  osl_weights <- data.frame(Y= weights)

  result <- subject$predict_osl(osl_weights = osl_weights, predictions = predictions, 
                      randomVariables = randomVariables, denormalize = FALSE)
  
  expected = as.numeric(c(1,2,3) %*% weights)
  expect_true(all(result == expected))
})

test_that("it should update the predictions and normalize if specified", {
  randomVariables <- list(list(getY='Y', getFamily='gaussian'))

  nobs <- 10
  expected <- 123
  pre_processor <- list(denormalize = function(data) {return(rep(expected, length(data)))})
  class(pre_processor) <- c(class(pre_processor), 'PreProcessor')
  subject <- described.class$new(pre_processor = pre_processor)

  df <- data.table(Y=rep(1, nobs))
  predictions <- list(a=df, b=df+1, c=df+2)
  weights <- c(0.2, 0.5, 0.3)
  osl_weights <- data.frame(Y= weights)

  result <- subject$predict_osl(osl_weights = osl_weights, predictions = predictions, 
                      randomVariables = randomVariables, denormalize = TRUE)
  
  expect_equal(result, expected)
})

context(" predict_dosl")
test_that("it should should predict using the dosl provided", {
  randomVariables <- list(
                          list(getY='W', getX='X', getFamily='gaussian'),
                          list(getY='A', getX='X', getFamily='binomial'),
                          list(getY='Y', getX='X', getFamily='gaussian')
                          )
  true_sample <- TRUE
  true_plot <- FALSE
  subject <- described.class$new(pre_processor = NULL)

  true_data <- data.table(W=0.1231, A = 1, Y = 7.1)

  # Create some mock objects
  W_mock_estimator = list(predict = function(data, sample, subset, plot) {
   expect_equal(subset, 'W') 
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   return(list(W = data[, subset, with=FALSE]))
  })

  A_mock_estimator = list(predict = function(data, sample, subset, plot) {
   expect_equal(subset, 'A') 
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   return(list(A = data[, subset, with=FALSE]))
  })

  Y_mock_estimator = list(predict = function(data, sample, subset, plot) {
   expect_equal(subset, 'Y') 
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   return(list(Y = data[, subset, with=FALSE]))
  })

  dosl <- list(W = W_mock_estimator, A = A_mock_estimator, Y = Y_mock_estimator)

  result <- subject$predict_dosl(dosl = dosl, data = true_data, sample = true_sample, plot = true_plot,
                                randomVariables = randomVariables, denormalize = FALSE)
  expect_true(is(result, 'data.table'))
  expect_equal(result, true_data[, c('W','A','Y'), with=FALSE])
})

test_that("it should predict and normalize if set", {
  randomVariables <- list(
                          list(getY='W', getX='X', getFamily='gaussian'),
                          list(getY='A', getX='X', getFamily='binomial'),
                          list(getY='Y', getX='X', getFamily='gaussian')
                          )
  expected <- 123
  true_sample <- TRUE
  true_plot <- FALSE
  true_data <- data.table(W=0.1231, A = 1, Y = 7.1)

  pre_processor <- list(denormalize = function(data) {return(rep(expected, length(data)))})
  class(pre_processor) <- c(class(pre_processor), 'PreProcessor')
  subject <- described.class$new(pre_processor = pre_processor)

  # Create some mock objects
  W_mock_estimator = list(predict = function(data, sample, subset, plot) {
   expect_equal(subset, 'W') 
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   return(list(W = data[, subset, with=FALSE]))
  })

  A_mock_estimator = list(predict = function(data, sample, subset, plot) {
   expect_equal(subset, 'A') 
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   return(list(A = data[, subset, with=FALSE]))
  })

  Y_mock_estimator = list(predict = function(data, sample, subset, plot) {
   expect_equal(subset, 'Y') 
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   return(list(Y = data[, subset, with=FALSE]))
  })

  dosl <- list(W = W_mock_estimator, A = A_mock_estimator, Y = Y_mock_estimator)

  result <- subject$predict_dosl(dosl = dosl, data = true_data, sample = true_sample, plot = true_plot,
                                randomVariables = randomVariables, denormalize = TRUE)
  expect_equal(result, c(expected, expected, expected)
)
  
})

context(" predict_using_all_estimators")
test_that("it should call all estimators and estimate using those estimators", {
  randomVariables <- list(
                          list(getY='W', getX='X', getFamily='gaussian'),
                          list(getY='A', getX='X', getFamily='binomial'),
                          list(getY='Y', getX='X', getFamily='gaussian')
                          )
  expected_W = 1
  expected_A = 2
  expected_Y = 3

  true_sample <- TRUE
  true_plot <- FALSE
  subject <- described.class$new(pre_processor = NULL)

  true_data <- data.table(W = .1231, A = 1, Y = 7.1)

  mock_estimator = list(predict = function(data, sample, plot) {
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   result <- list(W = rep(1,nrow(data)), A = rep(2,nrow(data)), Y = rep(3,nrow(data)))
  })

  sl_lib <- list(mock_estimator, mock_estimator, mock_estimator, mock_estimator)
  result <- subject$predict_using_all_estimators(data = true_data, sl_library = sl_lib,
                                                 sample = true_sample, plot = true_plot, denormalize = FALSE)

  # Each estimator should have an outcome
  expect_equal(length(result), length(sl_lib))

  # Each estimate should contain all RV's
  lapply(result, function(entry){
    expect_equal(entry$W, expected_W)
    expect_equal(entry$A, expected_A)
    expect_equal(entry$Y, expected_Y)
  })

})

test_that("it should call all estimators and estimate using those estimators and do normalization", {
  randomVariables <- list(
                          list(getY='W', getX='X', getFamily='gaussian'),
                          list(getY='A', getX='X', getFamily='binomial'),
                          list(getY='Y', getX='X', getFamily='gaussian')
                          )
  expected = 123

  true_sample <- TRUE
  true_plot <- FALSE
  pre_processor <- list(denormalize = function(data) {return(rep(expected, length(data)))})
  class(pre_processor) <- c(class(pre_processor), 'PreProcessor')
  subject <- described.class$new(pre_processor = pre_processor)

  true_data <- data.table(W = .1231, A = 1, Y = 7.1)

  mock_estimator = list(predict = function(data, sample, plot) {
   expect_equal(data, true_data) 
   expect_equal(sample, true_sample) 
   expect_equal(plot, true_plot) 
   result <- list(W = rep(1,nrow(data)), A = rep(2,nrow(data)), Y = rep(3,nrow(data)))
  })

  sl_lib <- list(mock_estimator, mock_estimator, mock_estimator, mock_estimator)
  result <- subject$predict_using_all_estimators(data = true_data, sl_library = sl_lib,
                                                 sample = true_sample, plot = true_plot, denormalize = TRUE)

  # Each estimator should have an outcome
  expect_equal(length(result), length(sl_lib))

  # Each estimate should contain all RV's
  lapply(result, function(entry){ expect_true(all(entry == expected)) })

})
