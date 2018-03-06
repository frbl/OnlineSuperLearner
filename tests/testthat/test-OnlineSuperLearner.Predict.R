library(mockery)

context("OnlineSuperLearner.Predict.R")
described.class <- OnlineSuperLearner.Predict

glob_relevantVariables <- list(
  list(getY='W', getX='X', getFamily='gaussian'),
  list(getY='A', getX='X', getFamily='binomial'),
  list(getY='Y', getX='X', getFamily='gaussian')
)

glob_data <- data.table(W=c(1,2,3),A= c(3,2,1), Y=c(1,0,1))
glob_current_result <- list(denormalized = list(), normalized = list())
glob_sample <- TRUE
glob_plot <- FALSE

context(" initialize")
#==========================================================
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
#==========================================================
test_that("it should return NA if no estimators were fit", {
  cur.data <- data.frame(a=c(1,2,3),b= c(3,2,1))
  OnlineSuperLearner_mock <- list(is_fitted=FALSE)
  subject <- described.class$new(pre_processor = NULL)
  expect_equal(subject$predict(osl = OnlineSuperLearner_mock, cur.data, relevantVariables = c('a')), NA)
})

test_that("it should throw if all estimators are disabled", {
  cur.data <- data.frame(a=c(1,2,3),b= c(3,2,1))
  OnlineSuperLearner_mock <- list(is_fitted=TRUE)
  subject <- described.class$new(pre_processor = NULL)
  expect_error(subject$predict(osl = OnlineSuperLearner_mock, cur.data, relevantVariables = c('a'),
                               all_estimators = FALSE, discrete = FALSE, continuous = FALSE), 
               'At least one option should be selected: discrete, all_estimators, or continuous')
})

test_that("it should throw if estimator are enabled but not trained", {
  cur.data <- data.frame(a=c(1,2,3),b= c(3,2,1))
  OnlineSuperLearner_mock <- list(is_fitted=TRUE, fits_dosl = FALSE)

  subject <- described.class$new(pre_processor = NULL)
  expect_error(subject$predict(osl = OnlineSuperLearner_mock, cur.data, relevantVariables = c('a'),
                               all_estimators = FALSE, discrete = TRUE, continuous = FALSE), 
               'At least one option should be selected: discrete, all_estimators, or continuous')

  OnlineSuperLearner_mock <- list(is_fitted=TRUE, fits_osl = FALSE)

  subject <- described.class$new(pre_processor = NULL)
  expect_error(subject$predict(osl = OnlineSuperLearner_mock, cur.data, relevantVariables = c('a'),
                               all_estimators = FALSE, discrete = FALSE, continuous = TRUE), 
               'At least one option should be selected: discrete, all_estimators, or continuous')
})


test_that("it should call the dosl with the correct parameters if enabled", {
  # Not elaborating because the stubbing doesn't seem to work the way I expect it to work...
  osl <- list(is_fitted=TRUE, fits_dosl = TRUE)
  subject <- described.class$new(pre_processor = NULL)
  cur.predictions <- list(normalized = 1, denormalized = 2)
  cur.sample <- TRUE
  cur.plot <- TRUE

  stub(subject$predict, 'self$predict_dosl',
    function(dosl, data, relevantVariables, current_result, sample, plot) {
      expect_equal(dosl, osl$get_dosl)
      expect_equal(data, glob_data)
      expect_equal(relevantVariables, c('a'))
      expect_equal(sample, cur.sample)
      expect_equal(plot, cur.plot)
      called <<- TRUE
      return(cur.predictions)
    }
  )

  called <<- FALSE
  subject$predict(
    osl = osl,
    data = glob_data, 
    relevantVariables = c('a'),
    all_estimators = FALSE,
    discrete = TRUE,
    continuous = FALSE,
    sample = cur.sample,
    plot = cur.plot
  )
  expect_true(called)
})

test_that("it should put the results in the correct lists", {
  # Not elaborating because the stubbing doesn't seem to work the way I expect it to work...
  osl <- list(is_fitted=TRUE, fits_dosl = TRUE)
  subject <- described.class$new(pre_processor = NULL)
  cur.predictions <- list(normalized = 1, denormalized = 2)

  stub(subject$predict, 'self$predict_dosl', function(...) {
    called <<- TRUE
    return(cur.predictions)
  })

  called <<- FALSE
  subject$predict(
    osl = osl,
    data = cur.data, 
    relevantVariables = c('a'),
    all_estimators = FALSE,
    discrete = TRUE,
    continuous = FALSE,
    sample = FALSE,
    plot = FALSE
  )
  expect_true(called)
  
})


context(" predict_osl")
#==========================================================
test_that("it should update the predictions according to the osl_weights", {
  nobs <- 10
  subject <- described.class$new(pre_processor = NULL)
  cur.sl_library <- list(c = list(predict = function(...){
      should_not_be_called <<- TRUE
      return(df + 2)
  }))

  df <- data.table(
    W=rep(1, nobs),
    A=rep(1, nobs),
    Y=rep(1, nobs)
  ) 

  ## Each entry is an algorithm
  predictions <- list(
    normalized = list(a=df, b=df+1, c=df+2),
    denormalized = list(a=df, b=df+1, c=df+2)
  )

  weights <- c(a = 0.2, b = 0.5, c=0.3)
  osl_weights <- data.frame(
    W = weights,
    A = weights,
    Y = weights
  ) %>% as.matrix

  should_not_be_called <<- FALSE
  result <- subject$predict_osl(
    data = data, 
    osl_weights = osl_weights,
    current_result = predictions, 
    sl_library = cur.sl_library, 
    relevantVariables = glob_relevantVariables,
    weight_threshold = 0.01,
    sample = FALSE,
    plot = FALSE
  )
  
  expect_false(should_not_be_called)
  expected = as.numeric(c(1,2,3) %*% weights) 
  expect_true(all(result$normalized$osl.estimator == expected))
  expect_true(all(result$denormalized$osl.estimator== expected))
})

test_that("it should calculate new predictions if not all are available", {
  nobs <- 10
  subject <- described.class$new(pre_processor = NULL)

  df <- data.table(
    W=rep(1, nobs),
    A=rep(1, nobs),
    Y=rep(1, nobs)
  ) 


  ## Each entry is an algorithm !! Note the missing c
  predictions <- list(
    normalized = list(a=df, b=df+1),
    denormalized = list(a=df, b=df+1)
  )

  weights <- c(a = 0.2, b = 0.5, c=0.3)
  osl_weights <- data.frame(
    W = weights,
    A = weights,
    Y = weights
  ) %>% as.matrix

  cur.sl_library <- list(c = list(predict = function(...){
      called <<- TRUE
      return(df + 2)
  }))

  called <<- FALSE
  result <- subject$predict_osl(
    data = data, 
    osl_weights = osl_weights,
    current_result = predictions, 
    sl_library = cur.sl_library, 
    relevantVariables = glob_relevantVariables,
    weight_threshold = 0.01,
    sample = FALSE,
    plot = FALSE
  )
  
  expected = as.numeric(c(1,2,3) %*% weights) 
  expect_true(called)
  expect_true(all(result$normalized$osl.estimator == expected))
  expect_true(all(result$denormalized$osl.estimator == expected))
})

test_that("it should return the whole data set, not just the osl estimed ones", {
  nobs <- 10
  subject <- described.class$new(pre_processor = NULL)

  df <- data.table(
    W=rep(1, nobs),
    A=rep(1, nobs),
    Y=rep(1, nobs)
  ) 



  ## Each entry is an algorithm !! Note the missing c
  predictions <- list(
    normalized = list(a=df, b=df+1),
    denormalized = list(a=df, b=df+1)
  )

  weights <- c(a = 0.2, b = 0.5, c=0.3)
  osl_weights <- data.frame(
    W = weights,
    A = weights,
    Y = weights
  ) %>% as.matrix

  cur.sl_library <- list(c = list(predict = function(...){
      called <<- TRUE
      return(df + 2)
  }))

  called <<- FALSE
  result <- subject$predict_osl(
    data = data, 
    osl_weights = osl_weights,
    current_result = predictions, 
    sl_library = cur.sl_library, 
    relevantVariables = glob_relevantVariables,
    weight_threshold = 0.01,
    sample = FALSE,
    plot = FALSE
  )
  
  expected = as.numeric(c(1,2,3) %*% weights) 
  expect_true(called)
  expect_named(result, c('normalized', 'denormalized'))
  expect_named(result$normalized, c('a', 'b', 'c', 'osl.estimator'))
  expect_named(result$denormalized, c('a', 'b', 'c', 'osl.estimator'))
  expect_true(all(lapply(result$normalized, nrow) == nobs))
  expect_true(all(lapply(result$denormalized, nrow) == nobs))
})

context(" predict_dosl")
#==========================================================

# Create some mock objects
W_mock_estimator = list(predict = function(data, sample, subset, plot) {
  expect_equal(subset, 'W') 
  expect_equal(data, glob_data) 
  expect_equal(sample, glob_sample) 
  expect_equal(plot, glob_plot) 
  return(list(W = data[, subset, with=FALSE]))
}, get_name = 'a')

A_mock_estimator = list(predict = function(data, sample, subset, plot) {
  expect_equal(subset, 'A') 
  expect_equal(data, glob_data) 
  expect_equal(sample, glob_sample) 
  expect_equal(plot, glob_plot) 
  return(list(A = data[, subset, with=FALSE]))
}, get_name = 'b')

Y_mock_estimator = list(predict = function(data, sample, subset, plot) {
  expect_equal(subset, 'Y') 
  expect_equal(data, glob_data) 
  expect_equal(sample, glob_sample) 
  expect_equal(plot, glob_plot) 
  return(list(Y = data[, subset, with=FALSE]))
}, get_name = 'c')

dosl <- list(W = W_mock_estimator, A = A_mock_estimator, Y = Y_mock_estimator)

test_that("it should should predict using the dosl provided", {
  subject <- described.class$new(pre_processor = NULL)



  result <- subject$predict_dosl(dosl = dosl, data = glob_data, current_result = glob_current_result, sample = glob_sample, plot = glob_plot,
                                relevantVariables = glob_relevantVariables)

  expect_true(is(result, 'list'))
  expect_named(result, c('denormalized', 'normalized'), ignore.order = TRUE)

  lapply(result, function(norm_denorm_result) {
    lapply(norm_denorm_result$normalized, function(current_result) {
      expect_true(is(current_result, 'data.table'))
      expect_equal(current_result, glob_data[, c('W','A','Y'), with=FALSE])
    })
  })
})

test_that("it should not predict if the predictions have been done before", {
  nobs <- 10

  alg_a = list(predict = function(...) {should_not_be_called <<- TRUE }, get_name = 'a')
  alg_b = list(predict = function(...) {should_not_be_called <<- TRUE }, get_name = 'b')
  alg_c = list(predict = function(...) {should_not_be_called <<- TRUE }, get_name = 'c')

  df <- data.table(
    W=rep(1, nobs),
    A=rep(1, nobs),
    Y=rep(1, nobs)
  ) 

  ## Each entry is an algorithm
  predictions <- list(
    normalized = list(a=df, b=df+1, c=df+2),
    denormalized = list(a=df, b=df+1, c=df+2)
  )

  should_not_be_called <<- FALSE
  subject <- described.class$new(pre_processor = NULL)
  result <- subject$predict_dosl(
    dosl = dosl,
    data = glob_data,
    current_result = predictions,
    sample = glob_sample,
    plot = glob_plot,
    relevantVariables = glob_relevantVariables
  )

  expect_false(should_not_be_called)
})

test_that("it should predict and normalize if set", {
  expected <- 123

  # Mock the preprocessor
  pre_processor <- list(
    denormalize = function(data) {
      result <- rep(expected, length(data)) %>%
        as.list %>%
        setDT 
      names(result) <-  names(glob_data)
      return(result)
    }
  )
  class(pre_processor) <- c(class(pre_processor), 'PreProcessor')
  subject <- described.class$new(pre_processor = pre_processor)

  result <- subject$predict_dosl(dosl = dosl, data = glob_data,  current_result = glob_current_result, sample = glob_sample, plot = glob_plot,
                                relevantVariables = glob_relevantVariables)

  expect_named(result, c('denormalized', 'normalized'), ignore.order = TRUE)
  expect_equal(result$denormalized$dosl.estimator, data.table(W=expected, A=expected, Y=expected))
})

test_that("it should not redo the prediction if it has been done already", {
  
})

test_that("it should return the whole data set, not just the dosl estimed ones", {
  nobs <- 10
  subject <- described.class$new(pre_processor = NULL)

  df <- data.table(
    W=rep(1, nobs),
    A=rep(1, nobs),
    Y=rep(1, nobs)
  ) 

  ## Each entry is an algorithm !! Note the missing c
  predictions <- list(
    normalized = list(a=df, b=df+1, c = df + 3),
    denormalized = list(a=df, b=df+1, c = df + 3)
  )

  result <- subject$predict_dosl(
    dosl = dosl,
    data = glob_data,
    current_result = predictions,
    sample = FALSE,
    plot = FALSE,
    relevantVariables = glob_relevantVariables  
  )
  
  expect_named(result, c('normalized', 'denormalized'))
  expect_named(result$normalized, c('a', 'b', 'c', 'dosl.estimator'))
  expect_named(result$denormalized, c('a', 'b', 'c', 'dosl.estimator'))
  expect_true(all(lapply(result$normalized, nrow) == nobs))
  expect_true(all(lapply(result$denormalized, nrow) == nobs))
})

#context(" predict_using_all_estimators")
#==========================================================
test_that("it should call all estimators and estimate using those estimators", {
  expected_W = 1
  expected_A = 2
  expected_Y = 3

  subject <- described.class$new(pre_processor = NULL)

  mock_estimator = list(predict = function(data, sample, plot) {
   expect_equal(data, glob_data) 
   expect_equal(sample, glob_sample) 
   expect_equal(plot, glob_plot) 
   result <- list(W = rep(expected_W,nrow(glob_data)),
                  A = rep(expected_A,nrow(glob_data)),
                  Y = rep(expected_Y,nrow(glob_data)))
  })

  sl_lib <- list(a = mock_estimator, b = mock_estimator, c = mock_estimator, d = mock_estimator)
  result <- subject$predict_using_all_estimators(
    data = glob_data,
    sl_library = sl_lib,
    sample = glob_sample,
    plot = glob_plot
  )

  # Each estimator should both have a normalized and denormalized outcome
  expect_length(result, 2)
  expect_named(result, c('denormalized', 'normalized'), ignore.order = TRUE)

  lapply(result, function(current_result) {
           current_result <- result[[1]]
    # Each estimator should have an outcome
    expect_equal(length(current_result), length(sl_lib))

    # Each estimate should contain all RV's
    lapply(current_result, function(entry){
      expect_equal(entry$W, rep(expected_W, nrow(glob_data)))
      expect_equal(entry$A, rep(expected_A, nrow(glob_data)))
      expect_equal(entry$Y, rep(expected_Y, nrow(glob_data)))
    })
  })

})
