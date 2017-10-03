library('mockery')
context("OneStepEstimator")
described.class <- OneStepEstimator

W <- RandomVariable$new(formula = W ~ Y_lag_1, family = 'gaussian')
A <- RandomVariable$new(formula = A ~ W, family = 'binomial')
Y <- RandomVariable$new(formula = Y ~ A, family = 'gaussian')
randomVariables <- c(W, A, Y)

# Mock the pre_processor
pre_processor <- list(
  denormalize = function(dat) dat
)

B <- 10
N <- 10
tau <- 3
intervention <- list(variable = 'A',
                     when=c(1),
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
  B = B,
  pre_processor = pre_processor,
  tau = tau,
  intervention = intervention,
  variable_of_interest = Y,
  parallel = FALSE
)

context(" initialize")
test_that("it should succesfully initialize when the correct arguments are provided", {
  expect_error(described.class$new(
    osl = osl,
    randomVariables = randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  ), NA)
})

context(" perform")

context(" get_h_ratio_estimators")
test_that("it should result in the correct output", {
  data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
  result <- subject$get_h_ratio_estimators(data = data)

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
    randomVariables = randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y,
    parallel = FALSE
  )
  result <- subject$get_h_ratio_estimators(data = data)

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

test_that("it should create the correct estimators -> the ratio should be approx 20 when there is people with certain covariates received treatment more often", {
  set.seed(12345)
  osl <- list(
    sample_iteratively = function(tau, intervention, ...) {
      data_names <- names(data)
      if(is.null(intervention)) {
        mu <- rep(0, tau)
      } else {
        mu <- rep(10, tau)
        #mu[length(mu)] <- 10
      }
      data_to_return <- lapply(seq_along(data_names), function(x) return(rnorm(tau, mu, 1))) %>%
        as.data.table
      names(data_to_return) <- data_names
      data_to_return
    }
 )
  class(osl) <- 'OnlineSuperLearner'
  subject <- described.class$new(
    osl = osl,
    randomVariables = randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y,
    parallel = FALSE
  )

  h_ratio_predictors <- subject$get_h_ratio_estimators(data = data)

  result <- lapply(seq(tau), function(s) {
    lapply(randomVariables, function(rv) {
      formula <- rv$get_formula_string(Y='Delta')
      ## We essentially force the h_ratio to be high, hence remove the warning
      hide_warning_high_h_ratio(
      subject$calculate_h_ratio(h_ratio_predictors,
                                s=s,
                                formula = formula,
                                dat = data))
    })
  }) %>% 
    unlist 

  for (i in result) {
    expect_lte(abs(19 - i), 0.001)
  }
})


test_that("it should create the correct estimators -> the ratio should be approx 0 when there are people with certain covariates received treatment less often", {
  set.seed(12345)
  osl <- list(
    sample_iteratively = function(tau, intervention, data, ...) {
      data_names <- names(data)
      mu <- rep(0, tau)
      data_to_return <- lapply(seq_along(data_names), function(x) {
        cov1 <- rnorm(1, mu[1], 1)
        cov2 <- rnorm(1, mu[2], 1)
        cov3 <- ifelse(cov1 > mu[1], rbinom(1,1, 0.999), rbinom(1,1, 0.3))
        cov4 <- rnorm(1, mu[tau], 1)

        c(cov1, cov2, cov3, cov4)
      }) %>%
        as.data.table
      names(data_to_return) <- data_names
      data_to_return
    }
 )
  class(osl) <- 'OnlineSuperLearner'
  subject <- described.class$new(
    osl = osl,
    randomVariables = randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y,
    parallel = FALSE
  )

  # Sample just one observation to start with
  data <- osl$sample_iteratively(tau, intervention, data)[1,]
  h_ratio_predictors <- subject$get_h_ratio_estimators(data = data)

  result <- lapply(seq(tau), function(s) {
    lapply(randomVariables, function(rv) {
      formula <- rv$get_formula_string(Y='Delta')
      subject$calculate_h_ratio(h_ratio_predictors,
                                s=s,
                                formula = formula,
                                dat = data)
    })
  }) %>%
   unlist %>%
   abs

  for (i in result) {
    ## If we would increase B and N, this should be closed to 0.05 (delta)
    expect_lte(i, 0.36)
  }
})

test_that("it should create the correct estimators -> the ratio should be approx 1 when there is no difference based on the covariates", {
  set.seed(12345)
  osl <- list(
    sample_iteratively = function(tau, intervention, ...) {
      data_names <- names(data)
      if(is.null(intervention)) {
        ## Don't do anything if there is an intervention, just always do the same
      }
      mu <- rep(0, tau)
      data_to_return <- lapply(seq_along(data_names), function(x) return(rnorm(tau, mu, 1))) %>%
        as.data.table
      names(data_to_return) <- data_names
      data_to_return
    }
 )
  class(osl) <- 'OnlineSuperLearner'
  subject <- described.class$new(
    osl = osl,
    randomVariables = randomVariables,
    N = 9,
    B = 100,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y,
    parallel = FALSE
  )

  h_ratio_predictors <- subject$get_h_ratio_estimators(data = data)


  result <- lapply(seq(tau), function(s) {
    lapply(randomVariables, function(rv) {
      formula <- rv$get_formula_string(Y='Delta')
      subject$calculate_h_ratio(h_ratio_predictors,
                                s=s,
                                formula = formula,
                                dat = data)
    })
  }) %>% 
    unlist %>%
    subtract(., 1) %>%
    abs

  result
  for (i in result) {
    ## If we would increase B and N, this whould be closed to 0
    expect_lt(i,  0.15)
  }
})

context(" evaluation_of_conditional_expectations")
test_that("it should perform the evaluation", {

  my_data <- data
  for (i in seq(N)) {
    my_data <- rbind(my_data, data)
  }
  
  stub(subject$evaluation_of_conditional_expectations, 'private$calculate_h_ratio', function(...) 0.5)
  hide_warning_test(
    result <- subject$evaluation_of_conditional_expectations(
      h_ratio_predictors = NULL, 
      data = my_data)
  )

  expect_equal(result, 0)
  
   ##We should have tau lists of estimators
  #expect_equal(length(result), tau)

   ##Each of the tau lists, should have 3 estimators (one for each covariate)
  #lapply(result,function(entry){
    #expect_equal(length(result), length(randomVariables))
    #lapply(entry,function(estimator){
      #expect_true(is(estimator, 'glm'))
       #TODO: test whether the estimators make sense
    #})
  #})
  
})


context(" get_next_and_current_rv")
test_that("it should get the next and the current random variable without overflowing the S", {
  rv_id <- 1
  result <- subject$get_next_and_current_rv(rv_id) 
  expect_equal(result$rv, randomVariables[[rv_id]])
  expect_equal(result$next_rv, randomVariables[[rv_id+1]])
  expect_equal(result$s_offset, 0)

  rv_id <- 2
  result <- subject$get_next_and_current_rv(rv_id) 
  expect_equal(result$rv, randomVariables[[rv_id]])
  expect_equal(result$next_rv, randomVariables[[rv_id+1]])
  expect_equal(result$s_offset, 0)
})

test_that("it should get the next and current random variable with overvlowing the S", {
  rv_id <- 3
  result <- subject$get_next_and_current_rv(rv_id) 
  expect_equal(result$rv, randomVariables[[rv_id]])
  expect_equal(result$next_rv, randomVariables[[1]])
  expect_equal(result$s_offset, 1)
})


