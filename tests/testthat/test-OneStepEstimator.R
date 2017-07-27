library('mockery')
context("OneStepEstimator")
described.class <- OneStepEstimator

W <- RandomVariable$new(formula = W ~ Y_lag_1, family = 'gaussian')
A <- RandomVariable$new(formula = A ~ W, family = 'binomial')
Y <- RandomVariable$new(formula = Y ~ A, family = 'gaussian')
randomVariables <- c(W, A, Y)

B <- 10
N <- 10
tau <- 3
intervention <- list(when=c(1),
                     what=c(1))

data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
osl <- list(
  sample_iteratively = function(tau, ...) {
    lapply(1:tau, function(x) data) %>% rbindlist
  }
)
class(osl) <- 'OnlineSuperLearner'
subject <- described.class$new(
  osl = osl,
  randomVariables = randomVariables,
  N = N,
  B = B
)

context(" initialize")
test_that("it should succesfully initialize when the correct arguments are provided", {
  expect_error(described.class$new(osl = osl , randomVariables = randomVariables, N= N, B=B), NA)
})

context(" perform")

context(" get_h_ratio_estimators")
test_that("it should result in the correct output", {
  data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
  result <- subject$get_h_ratio_estimators(tau = tau,
                                           intervention = intervention,
                                           data = data)

  expect_true(is(result,'list'))
  
  # We should have tau lists of estimators
  expect_equal(length(result), tau)

  # Each of the tau lists, should have 3 estimators (one for each covariate)
  lapply(result,function(entry){
    expect_equal(length(result), length(randomVariables))
    lapply(entry,function(estimator){
      expect_true(is(estimator, 'glm'))
    })
  })
  
})


test_that("it should produce sensible estimators in the correct output", {
  data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
  osl <- list(
    sample_iteratively = function(tau, intervention = NULL, ...) {
      if(is.null(intervention)) {
        dat <- rnorm(N*4, 1, 0.1)
      } else {
        dat <- rnorm(N*4, 1000, 0.1)
      }
      data.table(Y_lag_1 = dat[1:N], 
                 W = dat[(N + 1):(2*N)],
                 A = dat[(2*N + 1):(3*N)], 
                 Y = dat[(3*N + 1):(4*N)])
    }
  )
  class(osl) <- 'OnlineSuperLearner'

  subject <- described.class$new(
    osl = osl,
    B = B, 
    N = N, 
    randomVariables = randomVariables 
  )
  result <- subject$get_h_ratio_estimators(tau = tau,
                                           intervention = intervention,
                                           data = data)

  expect_true(is(result,'list'))
  
  # We should have tau lists of estimators
  expect_equal(length(result), tau)

  # Each of the tau lists, should have 3 estimators (one for each covariate)
  lapply(result,function(entry){
    expect_equal(length(result), length(randomVariables))
    lapply(entry,function(estimator){
      expect_true(is(estimator, 'glm'))
      # TODO: test whether the estimators make sense
    })
  })
})


context(" evaluation_of_conditional_expectations")
test_that("it should perform", {

  subject <- described.class$new(
    osl = osl,
    B = B, 
    N = N, 
    randomVariables = randomVariables 
  )

  stub(subject$evaluation_of_conditional_expectations, 'private$calculate_h_ratio', function(...) 0.5)
  hide_warning_test(
    result <- subject$evaluation_of_conditional_expectations(
      h_ratio_predictors = NULL, 
      variable_of_interest = Y, 
      data = data, 
      tau = tau, 
      intervention = intervention)
  )

  print(result)
  #expect_true(is(result,'list'))
  
  # We should have tau lists of estimators
  #expect_equal(length(result), tau)

  # Each of the tau lists, should have 3 estimators (one for each covariate)
  lapply(result,function(entry){
    #expect_equal(length(result), length(randomVariables))
    lapply(entry,function(estimator){
      #expect_true(is(estimator, 'glm'))
      # TODO: test whether the estimators make sense
    })
  })
  
})


context(" perform_initial_estimation")

