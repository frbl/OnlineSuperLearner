context("OnlineSuperLearner")
#==========================================================
described.class <- OnlineSuperLearner

context(" initialize")
#==========================================================
mylist <- c(SMG.Mock$new())
SMG <- SummaryMeasureGenerator$new(SMG.list = mylist)

random_variables <- list(list(getY='W'), list(getY='A'), list(getY='Y'))
subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)

test_that("it should initialize", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables), NA)
})

test_that("it should throw if the provided verbosity is incorrect", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables, verbose = '123'), 
               "Argument 'verbose' is non-logical: character", fixed = TRUE) 
})

test_that("it should throw if the provided SMG is not an SMG", {
  expect_error(described.class$new(summaryMeasureGenerator = '123', random_variables = random_variables), 
               "Argument 'summaryMeasureGenerator' is neither of nor inherits class SummaryMeasureGenerator: character", fixed = TRUE) 
})

test_that("it should throw if the provided should_fit_osl is not a boolean", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables, should_fit_osl = glm), 
               "Argument 'should_fit_osl' is not a vector: function", fixed = TRUE) 
})

test_that("it should throw if the provided should_fit_dosl is not a boolean", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables, should_fit_dosl = glm), 
               "Argument 'should_fit_dosl' is not a vector: function", fixed = TRUE) 
})

test_that("it should initialize the CV_risk", {
  expect_false(is.null(subject$get_cv_risk_count))
  expect_equal(subject$get_cv_risk_count, 0) 
})

test_that("it should initialize the CrossValidationRiskCalculator", {
  expect_false(is.null(subject$get_cv_risk_calculator))
  expect_is(subject$get_cv_risk_calculator, 'CrossValidationRiskCalculator') 
})

test_that("it should initialize the DataSplitter", {
  result <- subject$get_data_splitter
  expect_false(is.null(result))
  expect_is(result, 'DataSplitter') 
})

test_that("it should initialize the fabricated models", {
  result <- subject$get_estimators
  expect_false(is.null(result))
  expect_is(result, 'list') 
})

test_that("it should initialize the fabricated descriptions", {
  result <- subject$get_estimator_descriptions
  expect_false(is.null(result))
  expect_is(result, 'character') 
  expect_length(result, 2) 
  expect_equal(result[1], 'ML.Local.lm') 
  expect_equal(result[2], 'ML.H2O.glm') 
})

test_that("it should should check if all provided estimators are online", {
  ## Not testing this. It call s the density estimation function with the self$get_estimators variable. It is tested in that class.
})

test_that("it should should create a data cache for non online super learner tasks", {
  result <- subject$get_data_cache
  expect_false(is.null(result))
  expect_is(result, 'DataCache') 
})

test_that("it should initialize the weightedCombinationComputers", {
  result <- subject$get_weighted_combination_computers
  expect_false(is.null(result))
  expect_is(result, 'list') 
})

test_that("it should initialize the OSL predictor", {
  result <- subject$get_online_super_learner_predict
  expect_false(is.null(result))
  expect_is(result, 'OnlineSuperLearner.Predict') 
})

test_that("it should initialize the historical cv risk variable", {
  result <- subject$get_historical_cv_risk
  expect_false(is.null(result))
  expect_is(result, 'list') 
})

context(" set_verbosity")
#==========================================================
test_that("it should set the correct verbosity", {
  subject$set_verbosity(TRUE)
  expect_true(subject$get_verbosity)
  subject$set_verbosity(FALSE)
  expect_false(subject$get_verbosity)
})            

context(" fit")
#==========================================================
test_that("it should throw if the provided initial_datasize is not an int", {
  data <- mock('data')
  expect_error(subject$fit(data, initial_data_size = 'a', max_iterations = 20, mini_batch_size = 20), "Argument 'initial_data_size' contains")
  expect_error(
    subject$fit(data, initial_data_size = -1, max_iterations = 20, mini_batch_size = 20),
    "Argument 'initial_data_size' is out of range [1,Inf]: -1", fixed= TRUE)
})

test_that("it should throw if the provided max_iterations are not ints", {
  data <- mock('data')
  expect_error(
    subject$fit(data, initial_data_size = 1, max_iterations = 'a', mini_batch_size = 20),
    "Argument 'max_iterations' contains")
  expect_error(
    subject$fit(data, initial_data_size = 1, max_iterations = -20, mini_batch_size = 20),
    "Argument 'max_iterations' is out of range [0,Inf]: -20", fixed= TRUE)
})

test_that("it should throw if the provided mini_batch_size are not ints", {
  data <- mock('data')
  expect_error(
    subject$fit(data, initial_data_size = 1, max_iterations = 20, mini_batch_size = 'a'),
    "Argument 'mini_batch_size' contains")
  expect_error(
    subject$fit(data, initial_data_size = 1, max_iterations = 20, mini_batch_size = -20),
    "Argument 'mini_batch_size' is out of range [1,Inf]: -20", fixed= TRUE)
})

test_that("it should throw if the provided data is not a data object", {
  data <- mock('data')
  expect_error(
    subject$fit(data, initial_data_size = 1, max_iterations = 1, mini_batch_size = 20),
    "Argument 'data' is neither of nor inherits class Data.Base: mock")
})

test_that("it should set the data in the summary_measure_generator", {
  thedata <- Data.Base$new()
  SMG <- list(setData = function(data) {
    expect_equal(data, thedata)
    throw('stopping_execution')
  })
  class(SMG) <- 'SummaryMeasureGenerator'

  random_variables <- list(list(getY='W'), list(getY='A'), list(getY='Y'))
  subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)

  ## Note that we catch the error, just in order to stop the execution function
  expect_error(subject$fit(thedata, initial_data_size = 1, max_iterations = 1, mini_batch_size = 20),
               'stopping_execution')
})

test_that("it should check if enough data is available, and it should call it with the correct (named) variables", {
  thedata <- Data.Base$new()
  SMG <- list(
    setData = function(data) { },
    checkEnoughDataAvailable = function(randomVariables) {
      variable_names <- sapply(random_variables, function(rv) rv$getY)
      expect_equal(unname(randomVariables), random_variables)
      expect_named(randomVariables, variable_names)
      throw('stopping_execution')
    }
  )
  class(SMG) <- 'SummaryMeasureGenerator'

  random_variables <- list(list(getY='W'), list(getY='A'), list(getY='Y'))
  subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)

  ## Note that we catch the error, just in order to stop the execution function
  expect_error(subject$fit(thedata, initial_data_size = 1, max_iterations = 1, mini_batch_size = 20),
               'stopping_execution')
})

test_that("it should call the train_library function with the data from the summary measure generator", {
  initial_data_size = 123
  thedata <- Data.Base$new()
  next_data <- data.table(a=seq(5), b=seq(5))
  SMG <- list(
    setData = function(data) { },
    checkEnoughDataAvailable = function(randomVariables) { },
    getNext = function(n) {
      expect_equal(n, initial_data_size) 
      return(next_data)
    }
  )
  class(SMG) <- 'SummaryMeasureGenerator'

  random_variables <- list(list(getY='W'), list(getY='A'), list(getY='Y'))
  subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)

  stub(subject$fit, 'self$train_library', 
    function(data_current) {
      expect_equal(data_current, next_data)
      called <<- TRUE
      throw('stopping_execution')
    }
  )
  called <<- FALSE

  ## Note that we catch the error, just in order to stop the execution function
  expect_error(subject$fit(thedata, initial_data_size = initial_data_size, 
                           max_iterations = 1, mini_batch_size = 20), 'stopping_execution')
  expect_true(called)
})

test_that("it should call the update_library function with the correct data", {
  initial_data_size = 123
  cur.max_iterations = 321
  cur.mini_batch_size = 23

  thedata <- Data.Base$new()
  next_data <- data.table(a=seq(5), b=seq(5))
  SMG <- list(
    setData = function(...) { },
    checkEnoughDataAvailable = function(...) { },
    getNext = function(...) { }
  )
  class(SMG) <- 'SummaryMeasureGenerator'

  random_variables <- list(list(getY='W'), list(getY='A'), list(getY='Y'))
  subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)

  stub(subject$fit, 'self$train_library', 
    function(...) {
      called1 <<- TRUE
    }
  )

  stub(subject$fit, 'self$update_library', 
    function(max_iterations, mini_batch_size) {
      expect_equal(max_iterations, cur.max_iterations)
      expect_equal(mini_batch_size, cur.mini_batch_size)
      called2 <<- TRUE
      throw('stopping_execution')
    }
  )
  called1 <<- FALSE
  called2 <<- FALSE

  ## Note that we catch the error, just in order to stop the execution function
  expect_error(subject$fit(thedata, initial_data_size = initial_data_size, 
                           max_iterations = cur.max_iterations, 
                           mini_batch_size = cur.mini_batch_size), 'stopping_execution')
  expect_true(called1)
  expect_true(called2)
})

test_that("it should return the cvrisk in the end", {
  initial_data_size = 123
  cur.max_iterations = 321
  cur.mini_batch_size = 23
  expected_risk = 42

  thedata <- Data.Base$new()
  next_data <- data.table(a=seq(5), b=seq(5))
  SMG <- list(
    setData = function(...) { },
    checkEnoughDataAvailable = function(...) { },
    getNext = function(...) { }
  )
  class(SMG) <- 'SummaryMeasureGenerator'

  random_variables <- list(list(getY='W'), list(getY='A'), list(getY='Y'))
  subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)

  stub(subject$fit, 'self$train_library', function(...) { })

  stub(subject$fit, 'self$update_library', function(...) { })
  stub(subject$fit, 'self$get_cv_risk', function() {
    called <<- TRUE
    expected_risk
  })

  called <<- FALSE

  ## Note that we catch the error, just in order to stop the execution function
  result <- subject$fit(thedata, initial_data_size = initial_data_size, 
                           max_iterations = cur.max_iterations, 
                           mini_batch_size = cur.mini_batch_size)
  expect_equal(result, expected_risk)
  expect_true(called)
})

# HERE
context(" predict")
#==========================================================
test_that("it should call the predict function of the discrete online super learner if discrete is true", {
})

test_that("it should call the predict function of the  online super learner if discrete is false", {
})


context(" retrieve_list_of_random_variables")
test_that("it should work with the different formats", {
  skip('Test this')
  ## See the specs for the S3 methods. Most can be copied from there.
})

test_that("it should throw if the provided Y is a list, but not long enough", {
  data <- Data.Base$new()
  expect_error(subject$retrieve_list_of_random_variables(random_variables = list()),
               'There should be at least one entry in the outcomes specified')

})

test_that("it should work with a single randomVariable as outcome", {
  Y <- RandomVariable$new(formula = Y ~ A + W, family = 'binomial')

  ## Stub the getrv function
  random_variables <- subject$retrieve_list_of_random_variables(random_variables = Y)
  expect_is(random_variables, 'list')
  expect_equal(random_variables[[1]], Y)
})

test_that("it should work with a single randomVariable string as outcome", {
  skip('Test this')
})

test_that("it should work with a list of randomVariables as outcome", {
  Y <- list(
    RandomVariable$new(formula = Y ~ A + W, family = 'binomial'),
    RandomVariable$new(formula = Y ~ A + W, family = 'binomial')
  )

  random_variables <- subject$retrieve_list_of_random_variables(random_variables = Y)
  expect_is(random_variables, 'list')
  for(i in 1:length(random_variables)) {
    expect_equal(random_variables[[i]], Y[[i]])
  }
})

test_that("it should work with a list of randomVariable strings as outcome", {
  skip('Test this')
})

context(' is_online')
#==========================================================
test_that("it should return true if all estimators are online", {
  SL.Library <- list()

  SL.Library <- append(SL.Library, list(list(algorithm = 'ML.H2O.randomForest',
                          params = list(online = TRUE))))

  SL.Library <- append(SL.Library, list(list(algorithm = 'condensier::speedglmR6',
                          params = list(online = TRUE))))

  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)
  expect_true(subject$is_online)
})

test_that("it should return false if any estimators is not online", {
  SL.Library <- list()
  SL.Library <- append(SL.Library, list(list(algorithm = 'ML.H2O.randomForest',
                          params = list(online = FALSE))))

  SL.Library <- append(SL.Library, list(list(algorithm = 'condensier::speedglmR6',
                          params = list(online = TRUE))))

  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)
  expect_false(subject$is_online)
})

context(' is_fitted') 
#==========================================================
test_that("it should return the fitted status of the osl", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)
  expect_false(subject$is_fitted)
})

context(' fits_osl') 
#==========================================================
test_that("it should return true if the object will fit the osl (also the default)", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)
  expect_true(subject$fits_osl)
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_osl = TRUE, random_variables = random_variables)
  expect_true(subject$fits_osl)
})

test_that("it should return false if the object will fit the osl", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_osl = FALSE, random_variables = random_variables)
  expect_false(subject$fits_osl)
})

context(' fits_dosl') 
#==========================================================
test_that("it should return true if the object will fit the dosl (also the default)", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)
  expect_true(subject$fits_dosl)
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = TRUE, random_variables = random_variables)
  expect_true(subject$fits_dosl)
})

test_that("it should return false if the object will fit the dosl", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = FALSE, random_variables = random_variables)
  expect_false(subject$fits_dosl)
})

context(' get_estimators') 
#==========================================================
test_that("it should return a list of density estimation objects, fabricated", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = FALSE, random_variables = random_variables)
  result <- subject$get_estimators
  expect_equal(length(result), length(SL.Library))
  expect_true(all(sapply(result, function(x) is.a(x, 'DensityEstimation'))))
})

context(' get_cv_risk') 
#==========================================================
test_that("it should return an empty list after initialization", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)
  result <- subject$get_cv_risk()
  expect_false(is.null(result))
  expect_equal(result, list())
})

context(" set_random_variables")
#==========================================================
test_that("it should store the random variables", {
  subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)
  expect_false(is.null(subject$get_random_variables))
  expect_length(subject$get_random_variables, length(random_variables))
})

test_that("it should name each of the random variables", {
  subject <- described.class$new(summaryMeasureGenerator = SMG, random_variables = random_variables)
  variable_names <- sapply(random_variables, function(rv) rv$getY)
  expect_named(subject$get_random_variables, variable_names)
})
 
context(' get_validity') 
#==========================================================
# Hard to test explicitly, it is called by the initialize function
 
context(' get_osl_sampler') 
#==========================================================
test_that("It should return an instance of the OnlineSuperLearner.SampleIteratively class", {
  result <- subject$get_osl_sampler
  expect_is(result, 'OnlineSuperLearner.SampleIteratively')
})

context(" fit_dosl")
#==========================================================
test_that("it should return false if the current osl does not fit a dosl", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost', 'ML.SVM', 'ML.NeuralNet', 'ML.randomForest')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = FALSE, random_variables = random_variables)
  expect_false(subject$fit_dosl())
})

test_that("it should return the correct estimators", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost', 'ML.SVM', 'ML.NeuralNet', 'ML.randomForest')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)

  cv_risk <- list(
    ML.Local.lm = data.table(W = 0.3, A = 0.3, Y = 0.3),
    ML.XGBoost = data.table(W = 0.1, A = 0.9, Y = 0.3),
    ML.SVM = data.table(W = 0.9, A = 0.1, Y = 0.3),
    ML.NeuralNet = data.table(W = 0.3, A = 0.9, Y = 0.1),
    ML.RandomForest = data.table(W = 0.9, A = 0.9, Y = 0.9)
  )
  ## w is best for w, a for A, y for Y

  stub(subject$fit_dosl, 'self$get_cv_risk',  function() { return(cv_risk)})
  expect_true(subject$fit_dosl())

  result <- lapply(subject$get_dosl, function(x) x$get_name)
  expect_equal(result$W, SL.Library[[2]])
  expect_equal(result$A, SL.Library[[3]])
  expect_equal(result$Y, SL.Library[[4]])
})

test_that("it should not pick itself", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost', 'ML.SVM', 'ML.NeuralNet', 'ML.randomForest')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)

  cv_risk <- list(
    ML.Local.lm = data.table(W = 0.3, A = 0.3, Y = 0.3),
    ML.XGBoost = data.table(W = 0.1, A = 0.9, Y = 0.3),
    ML.SVM = data.table(W = 0.9, A = 0.1, Y = 0.3),
    ML.NeuralNet = data.table(W = 0.3, A = 0.9, Y = 0.1),
    ML.RandomForest = data.table(W = 0.9, A = 0.9, Y = 0.9),
    dosl.estimator = data.table(W = 0.01, A = 0.01, Y = 0.01)
  )
  ## w is best for w, a for A, y for Y

  stub(subject$fit_dosl, 'self$get_cv_risk',  function() { return(cv_risk)})
  expect_true(subject$fit_dosl())

  result <- lapply(subject$get_dosl, function(x) x$get_name)
  expect_false('dosl.estimator' %in% result)
})

test_that("it should not pick the osl", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost', 'ML.SVM', 'ML.NeuralNet', 'ML.randomForest')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)

  cv_risk <- list(
    ML.XGBoost = data.table(W = 0.1, A = 0.9, Y = 0.3),
    ML.SVM = data.table(W = 0.9, A = 0.1, Y = 0.3),
    ML.NeuralNet = data.table(W = 0.3, A = 0.9, Y = 0.1),
    ML.RandomForest = data.table(W = 0.9, A = 0.9, Y = 0.9),
    osl.estimator = data.table(W = 0.01, A = 0.01, Y = 0.01)
  )
  ## w is best for w, a for A, y for Y

  stub(subject$fit_dosl, 'self$get_cv_risk',  function() { return(cv_risk)})
  expect_true(subject$fit_dosl())

  result <- lapply(subject$get_dosl, function(x) x$get_name)
  expect_false('osl.estimator' %in% result)
})

test_that("it should work with an NA dosl.estimator", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost', 'ML.SVM', 'ML.NeuralNet', 'ML.randomForest')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, random_variables = random_variables)

  cv_risk <- list(
    dosl.estimator = data.table(W = NA, A = NA, Y = NA),
    ML.XGBoost = data.table(W = 0.1, A = 0.9, Y = 0.3),
    ML.SVM = data.table(W = 0.9, A = 0.1, Y = 0.3),
    ML.NeuralNet = data.table(W = 0.3, A = 0.9, Y = 0.1),
    ML.RandomForest = data.table(W = 0.9, A = 0.9, Y = 0.9)
  )
  ## w is best for w, a for A, y for Y

  stub(subject$fit_dosl, 'self$get_cv_risk',  function() { return(cv_risk)})
  expect_true(subject$fit_dosl())

  result <- lapply(subject$get_dosl, function(x) x$get_name)
  expect_equal(result$W, SL.Library[[2]])
  expect_equal(result$A, SL.Library[[3]])
  expect_equal(result$Y, SL.Library[[4]])
})




















