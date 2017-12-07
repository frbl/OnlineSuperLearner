context("OnlineSuperLearner")
#==========================================================
described.class <- OnlineSuperLearner

context(" initialize")
#==========================================================
mylist <- c(SMG.Mock$new())
SMG <- SummaryMeasureGenerator$new(SMG.list = mylist)
subject <- described.class$new(summaryMeasureGenerator = SMG)

test_that("it should initialize", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG), NA)
})

test_that("it should throw if the provided verbosity is incorrect", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG, verbose = '123'), 
               "Argument 'verbose' is non-logical: character", fixed = TRUE) 
})

test_that("it should throw if the provided SMG is not an SMG", {
  expect_error(described.class$new(summaryMeasureGenerator = '123'), 
               "Argument 'summaryMeasureGenerator' is neither of nor inherits class SummaryMeasureGenerator: character", fixed = TRUE) 
})

test_that("it should throw if the provided should_fit_osl is not a boolean", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG, should_fit_osl = glm), 
               "Argument 'should_fit_osl' is not a vector: function", fixed = TRUE) 
})

test_that("it should throw if the provided should_fit_dosl is not a boolean", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG, should_fit_dosl = glm), 
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

# HERE ================================
context(" perform_sample")


context(" fit")
#==========================================================
test_that("it should throw if the provided datasize is not an int", {
  data <- mock('data')
  randomVariables <- mock('randomvariables')
  expect_error(subject$fit(data, randomVariables, initial_data_size = 'a', max_iterations = 20, mini_batch_size = 20), "Argument 'initial_data_size' contains")
  expect_error(
    subject$fit(data, randomVariables, initial_data_size = -1, max_iterations = 20, mini_batch_size = 20),
    "Argument 'initial_data_size' is out of range [1,Inf]: -1", fixed= TRUE)
})

test_that("it should throw if the provided iterations are note ints", {
  data <- mock('data')
  randomVariables <- mock('randomvariables')
  expect_error(
    subject$fit(data, randomVariables, initial_data_size = 1, max_iterations = 'a', mini_batch_size = 20),
    "Argument 'max_iterations' contains")
  expect_error(
    subject$fit(data, randomVariables, initial_data_size = 1, max_iterations = -20, mini_batch_size = 20),
    "Argument 'max_iterations' is out of range [0,Inf]: -20", fixed= TRUE)
})

test_that("it should throw if the provided data is not a data object", {
  data <- mock('data')
  randomVariables <- mock('randomvariables')
  expect_error(
    subject$fit(data, randomVariables, initial_data_size = 1, max_iterations = 1, mini_batch_size = 20),
    "Argument 'data' is neither of nor inherits class Data.Base: mock")
})

test_that("it should call the train_library function", {
  
})

test_that("it should call the update_library function", {
  
})

context(" predict")
#==========================================================
test_that("it should call the predict function of the discrete online super learner if discrete is true", {
})

test_that("it should call the predict function of the  online super learner if discrete is false", {
})

context(' is_online')
#==========================================================
test_that("it should return true if all estimators are online", {
  SL.Library <- list()

  SL.Library <- append(SL.Library, list(list(algorithm = 'ML.H2O.randomForest',
                          params = list(online = TRUE))))

  SL.Library <- append(SL.Library, list(list(algorithm = 'condensier::speedglmR6',
                          params = list(online = TRUE))))

  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  expect_true(subject$is_online)
})

test_that("it should return false if any estimators is not online", {
  SL.Library <- list()
  SL.Library <- append(SL.Library, list(list(algorithm = 'ML.H2O.randomForest',
                          params = list(online = FALSE))))

  SL.Library <- append(SL.Library, list(list(algorithm = 'condensier::speedglmR6',
                          params = list(online = TRUE))))

  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  expect_false(subject$is_online)
})

context(' is_fitted') 
#==========================================================
test_that("it should return the fitted status of the osl", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  expect_false(subject$is_fitted)
})

context(' fits_osl') 
#==========================================================
test_that("it should return true if the object will fit the osl (also the default)", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  expect_true(subject$fits_osl)
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_osl = TRUE)
  expect_true(subject$fits_osl)
})

test_that("it should return false if the object will fit the osl", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_osl = FALSE)
  expect_false(subject$fits_osl)
})

context(' fits_dosl') 
#==========================================================
test_that("it should return true if the object will fit the dosl (also the default)", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  expect_true(subject$fits_dosl)
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = TRUE)
  expect_true(subject$fits_dosl)
})

test_that("it should return false if the object will fit the dosl", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = FALSE)
  expect_false(subject$fits_dosl)
})

context(' get_estimators') 
#==========================================================
test_that("it should return a list of density estimation objects, fabricated", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = FALSE)
  result <- subject$get_estimators
  expect_equal(length(result), length(SL.Library))
  expect_true(all(sapply(result, function(x) is.a(x, 'DensityEstimation'))))
})

context(' get_cv_risk') 
#==========================================================
test_that("it should return an empty list after initialization", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  result <- subject$get_cv_risk()
  expect_false(is.null(result))
  expect_equal(result, list())
})
 
context(' get_validity') 
#==========================================================
# Hard to test explicitly, it is called by the initialize function
 
context(" fit_dosl")
#==========================================================
test_that("it should return false if the current osl does not fit a dosl", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost', 'ML.SVM', 'ML.NeuralNet', 'ML.randomForest')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = FALSE)
  expect_false(subject$fit_dosl())
})

test_that("it should return the correct estimators", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost', 'ML.SVM', 'ML.NeuralNet', 'ML.randomForest')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)

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
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)

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
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)

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
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)

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





















