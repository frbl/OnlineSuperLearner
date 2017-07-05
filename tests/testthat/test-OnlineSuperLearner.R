context("OnlineSuperLearner")
described.class <- OnlineSuperLearner

context(" initialize")
mylist <- c(SMG.Mock$new())
SMG <- SummaryMeasureGenerator$new(SMG.list = mylist)
subject <- described.class$new(summaryMeasureGenerator = SMG)
test_that("it should initialize", {
  expect_error(described.class$new(summaryMeasureGenerator = SMG), NA)
})

test_that("it should initialize the CV_risk", {
 expect_equal(subject$get_cv_risk, list()) 
})

context(' fit')

context(" predict")

test_that("it should call the predict function of the discrete online super learner if discrete is true", {
})

test_that("it should call the predict function of the  online super learner if discrete is false", {
})

context(' sample_iteratively')
test_that("it should throw if the provided start from is not in the randomvariables", {
  skip('not yet tested')  
})

test_that("it should work with any start from (either NULL or a value)", {
  skip('not yet tested')  
})

context(' is_online')
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
test_that("it should return the fitted status of the osl", {
  SL.Library <- 'ML.Local.lm'
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  expect_false(subject$is_fitted)
})

context(' fits_osl') 
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
test_that("it should return a list of density estimation objects, fabricated", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG, should_fit_dosl = FALSE)
  result <- subject$get_estimators
  expect_equal(length(result), length(SL.Library))
  expect_true(all(sapply(result, function(x) is.a(x, 'DensityEstimation'))))
})

context(' get_cv_risk') 
test_that("it should return an empty list after initialization", {
  SL.Library <- c('ML.Local.lm', 'ML.XGBoost')
  subject <- described.class$new(SL.Library, summaryMeasureGenerator = SMG)
  result <- subject$get_cv_risk
  expect_false(is.null(result))
  expect_equal(result, list())
})
 
context(' get_validity') 
# Hard to test explicitly, it is called by the initialize function
 
 
 
