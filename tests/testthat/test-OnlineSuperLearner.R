context("OnlineSuperLearner")

context(" Initialize")

context(" Run")

context(" predict")
data <- data.frame(a=c(1,2,3),b= c(3,2,1))
test_that("it should return NA if no estimators were fit", {
  SL.Library <- 'ML.Local.lm'
  smg = list()
  class(smg) <- 'SummaryMeasureGenerator'
  subject <- OnlineSuperLearner$new(SL.Library, summaryMeasureGenerator = smg)
  expect_equal(subject$predict(data, c('a'), c('b'), discrete = FALSE), NA)
  expect_equal(subject$predict(data, c('a'), c('b'), discrete = TRUE), NA)
})

test_that("it should call the predict function of the discrete online super learner if discrete is true", {
})

test_that("it should call the predict function of the  online super learner if discrete is false", {
})

