library('mockery')
context("OneStepEstimator")
described.class <- OneStepEstimator

W <- RandomVariable$new(formula = W ~ Y_lag_1, family = 'gaussian')
A <- RandomVariable$new(formula = A ~ W, family = 'binomial')
Y <- RandomVariable$new(formula = Y ~ A + W, family = 'gaussian')
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
create_subject <- function(other_B = NULL, other_osl = NULL) {
  if(is.null(other_B)) other_B <- B
  if(is.null(other_osl)) other_osl <- osl 
  described.class$new(
    osl = other_osl,
    randomVariables = randomVariables,
    N = N,
    B = other_B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y,
    parallel = FALSE,
    minimal_measurements_needed = 1 # Minimal 1, because lag 1
  )
}

subject <- create_subject()

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
#test_that("it should result in the correct output", {
  #data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
  #result <- subject$get_h_ratio_estimators(data = data)

  #expect_true(is(result,'list'))
  
  ## We should have tau lists of estimators
  #expect_equal(length(result), tau)

  ## Each of the tau lists, should have 3 estimators (one for each covariate)
  #lapply(result,function(entry){
    #expect_equal(length(result), length(randomVariables))
    #lapply(entry,function(estimator){
      #expect_true(is(estimator, 'speedglm'))
    #})
  #})
#})


#test_that("it should produce sensible estimators in the correct output", {
  #data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
  #osl <- list(
    #sample_iteratively = function(tau, intervention = NULL, ...) {
      #if(is.null(intervention)) {
        #dat <- rnorm(N*4, 1, 0.1)
      #} else {
        #dat <- rnorm(N*4, 1000, 0.1)
      #}
      #data.table(Y_lag_1 = dat[1:N], 
                 #W = dat[(N + 1):(2*N)],
                 #A = dat[(2*N + 1):(3*N)], 
                 #Y = dat[(3*N + 1):(4*N)])
    #}
  #)
  #class(osl) <- 'OnlineSuperLearner'

  #subject <- described.class$new(
    #osl = osl,
    #randomVariables = randomVariables,
    #N = N,
    #B = B,
    #pre_processor = pre_processor,
    #tau = tau,
    #intervention = intervention,
    #variable_of_interest = Y,
    #parallel = FALSE
  #)
  #result <- subject$get_h_ratio_estimators(data = data)

  #expect_true(is(result,'list'))
  
  ## We should have tau lists of estimators
  #expect_equal(length(result), tau)

  ## Each of the tau lists, should have 3 estimators (one for each covariate)
  #lapply(result,function(entry){
    #expect_equal(length(result), length(randomVariables))
    #lapply(entry,function(estimator){
      #expect_true(is(estimator, 'speedglm'))
      ## TODO: test whether the estimators make sense
    #})
  #})
#})

test_that("it should create the correct estimators -> the ratio should be approx 20 when there is people with certain covariates received treatment more often", {
  set.seed(12345)
  osl <- list(
    sample_iteratively = function(tau, intervention, ...) {
      data_names <- names(data)
      if(is.null(intervention)) {
        mu <- rep(0, tau)
      } else {
        mu <- rep(1, tau)
      }
      sigma = 1
      data_to_return <- lapply(seq_along(data_names), function(x) return(rnorm(tau, mu, sigma))) %>%
        as.data.table
      names(data_to_return) <- data_names
      data_to_return
    }
  )

  class(osl) <- 'OnlineSuperLearner'
  subject <- create_subject(other_B = 1000, other_osl = osl)

  data <- data.table(Y_lag_1 = 0, W=0, A=0, Y=0)
  h_ratio_predictors <- subject$get_h_ratio_estimators(data = data)

  result <- lapply(seq(tau), function(s) {
    lapply(randomVariables, function(rv) {
      formula <- rv$get_formula_string(Y='Delta')
      ## We essentially force the h_ratio to be high, hence remove the warning
      hide_warning_high_h_ratio(
        subject$calculate_h_ratio(h_ratio_predictors,
                                  s=s,
                                  formula = formula,
                                  dat = data)
      )
    })
  }) %>% 
    unlist 

  for (i in result) {
    print(i)
    expect_lte(abs(19 - i), 0.001)
  }
})


test_that("it should create the correct estimators -> the ratio should be approx 0 when there are people with certain covariates received treatment less often", {
  set.seed(12345)
  osl <- list(
    sample_iteratively = function(tau, intervention, data, ...) {
      data_names <- names(data)
      if(is.null(intervention)) {
        mu <- 0
      } else {
        mu <- 10
      }
      mu <- rep(mu, ncol(data))
      data_to_return <- lapply(seq(tau), function(x) {
        cov1 <- rnorm(1, mu[1], 1)
        cov2 <- rnorm(1, mu[2], 1)
        if (cov1 > mu[3]) {
          cov3 <- rbinom(1,1, 0.99)
        } else {
          cov3 <- rbinom(1,1, 0.01)
        }
        cov4 <- rnorm(1, mu[3], 1)
        data.table(cov1, cov2, cov3, cov4)
      })
      data_to_return <- rbindlist(data_to_return)
      names(data_to_return) <- data_names
      data_to_return
    }
  )
  class(osl) <- 'OnlineSuperLearner'
  subject <- create_subject(other_B = 1000, other_osl = osl)

  # Sample just one observation to start with
  data <- osl$sample_iteratively(1, intervention, data)

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
    expect_lte(i, 0.06)
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
  subject <- create_subject(1000)

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

context(" calculate_h_ratio_predictors")
test_that("it should call the Constrained glm with the correct arguments", {
  subject <- create_subject()
  nobs <- 100
  column_data <- rep(1,nobs)
  column_data_zero <- rep(0,nobs)
  Osample_p <- data.table(W = column_data, 
                          A = column_data,
                          Y = column_data,
                          Delta = column_data
                          )

  Osample_p_star <- data.table(W = c(column_data, column_data, column_data), 
                          A = c(column_data, column_data, column_data),
                          Y = c(column_data, column_data, column_data),
                          Delta = c(column_data_zero, column_data_zero, column_data_zero),
                          time_s_column = c(column_data, column_data * 2, column_data * 3)
                          )

  with_mock(ConstrainedGlm.fit = function(formula, data, delta, previous_glm, s) {
      expect_true(is(formula, 'formula'))
      expect_equal(ncol(data), 4)
      expect_equal(nrow(data), nobs*2)
      if (!subject$is_online) {
        expect_null(previous_glm)
      }
      42
    }, 
    subject$calculate_h_ratio_predictors(Osample_p = Osample_p, Osample_p_star = Osample_p_star))

})

test_that("it should return the results from the constrained glm and properly name the attributes", {
  subject <- create_subject()
  nobs <- 100
  column_data <- rep(1,nobs)
  column_data_zero <- rep(0,nobs)
  Osample_p <- data.table(W = column_data, 
                          A = column_data,
                          Y = column_data,
                          Delta = column_data
                          )

  Osample_p_star <- data.table(W = c(column_data, column_data, column_data), 
                          A = c(column_data, column_data, column_data),
                          Y = c(column_data, column_data, column_data),
                          Delta = c(column_data_zero, column_data_zero, column_data_zero),
                          time_s_column = c(column_data, column_data * 2, column_data * 3)
                          )

  result = with_mock(ConstrainedGlm.fit = function(formula, data, delta, previous_glm, s) 42, 
    subject$calculate_h_ratio_predictors(Osample_p = Osample_p, Osample_p_star = Osample_p_star))

  result <- lapply(seq(tau), function(i) {
    valid = TRUE
    valid = valid && !is.null(names(result[[i]]))
    valid = valid && equals(names(result[[i]]), subject$get_formulae)
    valid = valid && equals(unlist(lapply(result[[i]], function(x) x)), rep(42, length(result[[i]])))
    valid
  }) %>% unlist
  expect_true(all(result))

})

test_that("it should update the cache", {
  subject <- create_subject()
  nobs <- 100
  column_data <- rep(1,nobs)
  column_data_zero <- rep(0,nobs)
  Osample_p <- data.table(W = column_data, 
                          A = column_data,
                          Y = column_data,
                          Delta = column_data
                          )

  Osample_p_star <- data.table(W = c(column_data, column_data, column_data), 
                          A = c(column_data, column_data, column_data),
                          Y = c(column_data, column_data, column_data),
                          Delta = c(column_data_zero, column_data_zero, column_data_zero),
                          time_s_column = c(column_data, column_data * 2, column_data * 3)
                          )

  with_mock(ConstrainedGlm.fit = function(formula, data, delta, previous_glm, s) {
      expect_equal(ncol(data), 4)
      expect_equal(nrow(data), nobs*2)
      42
    }, 
    subject$calculate_h_ratio_predictors(Osample_p = Osample_p, Osample_p_star = Osample_p_star))

  with_mock(ConstrainedGlm.fit = function(formula, data, delta, previous_glm, s) {
      expect_equal(ncol(data), 4)
      expect_equal(nrow(data), nobs*4) ## Note the 4 instead ot 2
    }, 
    subject$calculate_h_ratio_predictors(Osample_p = Osample_p, Osample_p_star = Osample_p_star))

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
      #expect_true(is(estimator, 'speedglm'))
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


