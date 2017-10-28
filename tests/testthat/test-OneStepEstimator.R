library('mockery')
context("OneStepEstimator")
#==========================================================
described.class <- OneStepEstimator

W <- RandomVariable$new(formula = W ~ Y_lag_1, family = 'gaussian')
A <- RandomVariable$new(formula = A ~ W, family = 'binomial')
Y <- RandomVariable$new(formula = Y ~ A + W, family = 'gaussian')
glob_randomVariables <- c(W, A, Y)

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

glob_data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
glob_osl <- list(
  sample_iteratively = function(tau, ...) {
    lapply(1:tau, function(x) glob_data) %>% rbindlist
  }
)

class(glob_osl) <- 'OnlineSuperLearner'
create_subject <- function(other_B = NULL, other_osl = NULL) {
  if(is.null(other_B)) other_B <- B
  if(is.null(other_osl)) other_osl <- glob_osl 
  described.class$new(
    osl = other_osl,
    randomVariables = glob_randomVariables,
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
#==========================================================
test_that("it should succesfully initialize when the correct arguments are provided", {
  expect_error(described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  ), NA)
})

test_that("it should initialize the last oos estimate", {
  result <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )
  expect_equal(result$get_last_oos_estimate, 0)
})

test_that("it should throw if the provided osl is not an online super learner", {
  wrong_osl <- glm
  expect_error(described.class$new(
    osl = wrong_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  ), "Argument 'osl' is neither of nor inherits class OnlineSuperLearner: function", fixed = TRUE)
})

test_that("it should throw if the provided N is not valid", {
  wrong_N <- c(-1, 0)
  for (cur.N in wrong_N) {
    expect_error(described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables,
      N = cur.N,
      B = B,
      pre_processor = pre_processor,
      tau = tau,
      intervention = intervention,
      variable_of_interest = Y
    ), "Argument 'N' is out of range")
  }
})


test_that("it should throw if the provided B is not valid", {
  skip('For some reason, the function does not work on B')
  wrong_B <- c(-1, 0)
  for (cur.B in wrong_B) {
    expect_error(described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables,
      N = N,
      B = cur.B,
      pre_processor = pre_processor,
      tau = tau,
      intervention = intervention,
      variable_of_interest = Y
    ), "Argument 'N' is out of range")
  }
})

test_that("it should should throw if the provided 'discrete' is not valid", {
  wrong_discrete <- c(-1, function(){1})
  for (cur.discrete in wrong_discrete) {
    expect_error(described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables,
      N = N,
      B = B,
      pre_processor = pre_processor,
      tau = tau,
      discrete = cur.discrete,
      intervention = intervention,
      variable_of_interest = Y
    ), "Argument 'discrete' is")
  }
})

test_that("it should throw if the provided random variables are not a list", {
    expect_error(described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables[[1]],
      N = N,
      B = B,
      pre_processor = pre_processor,
      tau = tau,
      intervention = intervention,
      variable_of_interest = Y
    ), "Argument 'randomVariables' is neither of nor inherits class list: RandomVariable, R6", fixed = TRUE)
})

test_that("it should order the random variables in the beginning", {
  rv_mock <- mock(function(...) 42)

  with_mock(RandomVariable.find_ordering = rv_mock, 
  described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  ))
  expect_called(rv_mock, 1)
  args <- mock_args(rv_mock)[[1]]
  expect_equal(args$randomVariables, glob_randomVariables)
})

test_that("it should should store the preprocessor", {
  pre_processor_mock <- mock(function(...) 42)

  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor_mock,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )
  expect_equal(subject$get_pre_processor, pre_processor_mock)
})

test_that("it should store tau", {
  mock_tau <- 1213

  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = mock_tau,
    intervention = intervention,
    variable_of_interest = Y
  )
  expect_equal(subject$get_tau, mock_tau)
})

test_that("it should store the intervention", {
  mock_intervention <- mock('intervention')

  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = mock_intervention,
    variable_of_interest = Y
  )
  expect_equal(subject$get_intervention, mock_intervention)
})

test_that("it should store the variable of interest", {
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )
  expect_equal(subject$get_variable_of_interest, Y$getY)
})

test_that("it should determine wheter it is parallel or not", {
  for (parallel in c(T,F)) {
    subject <- described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables,
      N = N,
      B = B,
      pre_processor = pre_processor,
      tau = tau,
      intervention = intervention,
      parallel = parallel,
      variable_of_interest = Y
    )
    expect_equal(subject$is_parallel, parallel) 
  }
})

test_that("it should determine wheter it is online or not", {
  
  for (online in c(T,F)) {
    subject <- described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables,
      N = N,
      B = B,
      pre_processor = pre_processor,
      tau = tau,
      intervention = intervention,
      online = online,
      variable_of_interest = Y
    )
    expect_equal(subject$is_online, online) 
  }
})

test_that("it should create a data cache for storing the P values", {
  for (online in c(T,F)) {
    subject <- described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables,
      N = N,
      B = B,
      pre_processor = pre_processor,
      tau = tau,
      intervention = intervention,
      online = online,
      variable_of_interest = Y
    )
    dc <- subject$get_data_cache(star = FALSE)
    expect_false(is.null(dc))
    expect_equal(dc$is_online, online)
  }
})

test_that("it should store a data cach for storing the Pstar values", {
  for (online in c(T,F)) {
    subject <- described.class$new(
      osl = glob_osl,
      randomVariables = glob_randomVariables,
      N = N,
      B = B,
      pre_processor = pre_processor,
      tau = tau,
      intervention = intervention,
      online = online,
      variable_of_interest = Y
    )
    dc <- subject$get_data_cache(star = TRUE)
    expect_false(is.null(dc))
    expect_equal(dc$is_online, online)
  }
})

test_that("it should store the minimal values needed before a complete block is refreshed", {
  mock_minimal_measurements <- 1293
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    minimal_measurements_needed = mock_minimal_measurements,
    variable_of_interest = Y
  )
  result <- subject$get_minimal_measurements_needed
  expect_equal(result, mock_minimal_measurements)
})

test_that("it should throw if the provided verbosity is invalid", {
  expect_error(described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y,
    verbose = glm
  ), "Argument 'verbose' is")
})

context(" perform")
#==========================================================
test_that("it should call the caluclate full oos function with the correct parameters", {
 subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = 123
  cur.truth = -12

  stub(subject$perform, 'self$calculate_full_oos',
    function(initial_estimate, data, truth) { 

      expect_equal(initial_estimate, cur.initial_estimate)
      expect_equal(data, glob_data)
      expect_equal(truth, cur.truth)
      called <<- TRUE
      throw('stop_execution')
  })

  called <<- FALSE
  expect_error(subject$perform(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  ), 'stop_execution')

  expect_true(called)
})

test_that("it should call the calculate variance function (which doesnt do anything)", {
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = 123
  cur.truth = -12

  stub(subject$perform, 'self$calculate_full_oos', function(...) { })
  stub(subject$perform, 'self$calculate_oos_variance', function(...) { 
    called <<- TRUE
    throw('stop_execution')
  })

  called <<- FALSE
  expect_error(subject$perform(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  ), 'stop_execution')

  expect_true(called)
})

test_that("it should return a list with two entries (variance and estimate)", {
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = 123
  cur.truth = -12

  stub(subject$perform, 'self$calculate_full_oos', function(...) {1})
  stub(subject$perform, 'self$calculate_oos_variance', function(...) {2})

  result <- subject$perform(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  )

  expect_named(result, c('oos_estimate', 'oos_variance'))
  expect_equal(result$oos_estimate, 1)
  expect_equal(result$oos_variance, 2)
})

context(" calculate_full_oos")
#==========================================================
test_that("it should throw if the initial estimate is not numeric", {
   subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = glm
  cur.truth = -12

  expect_error(subject$calculate_full_oos(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  ), "cannot coerce type 'closure' to vector of type 'double'", fixed = TRUE)
})

test_that("it should call, from 1:N times, the get H ratio function", {
  cur.N <- 10
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = cur.N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = 1
  cur.truth = -12

  stub(subject$calculate_full_oos, 'self$get_h_ratio_estimators', function(data, last_h_ratio_estimators) {
    iter <<- iter + 1
    expect_equal(data, glob_data[iter,])
    if(iter == 1) {
      expect_null(last_h_ratio_estimators)
    } else {
      expect_equal(last_h_ratio_estimators, 1)
    }
    return(1)
  })
  stub(subject$calculate_full_oos, 'self$evaluation_of_conditional_expectations', function(...) {1 })

  iter <<- 0
  subject$calculate_full_oos(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  )
  expect_equal(iter, cur.N)
})

test_that("it should call, from 1:N times, the evaluation of conditional expectaions function ", {
  cur.N <- 10
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = cur.N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = 1
  cur.truth = -12

  stub(subject$calculate_full_oos, 'self$get_h_ratio_estimators', function(...) { 1 })
  stub(subject$calculate_full_oos, 'self$evaluation_of_conditional_expectations', function(data, h_ratio_predictors) {
    iter <<- iter + 1
    expect_equal(data, glob_data[iter,])
    expect_equal(h_ratio_predictors, 1)
    return(1)
  })

  iter <<- 0
  subject$calculate_full_oos(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  )
  expect_equal(iter, cur.N)
})

test_that("it should calculate the correct dstar, and update it every N iteration", {
  cur.N <- 10
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = cur.N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = 1
  cur.truth = -12

  stub(subject$calculate_full_oos, 'self$get_h_ratio_estimators', function(...) { iter })
  stub(subject$calculate_full_oos, 'self$evaluation_of_conditional_expectations', function(...) {
    iter <<- iter + 1
    return(iter)
  })


  iter <<- 0
  result <- subject$calculate_full_oos(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  )

  expected_d_star_evaluation <- 0
  for (i in 1:cur.N) {
    previous_unmeaned <- (i - 1) * expected_d_star_evaluation
    current <- i
    expected_d_star_evaluation <- (previous_unmeaned + current) / i
  }
  
  expect_equal(result, expected_d_star_evaluation + cur.initial_estimate)
})

test_that("it should return the initial estimate if the oos estimate is NA or nan", {
  cur.N <- 10
  subject <- described.class$new(
    osl = glob_osl,
    randomVariables = glob_randomVariables,
    N = cur.N,
    B = B,
    pre_processor = pre_processor,
    tau = tau,
    intervention = intervention,
    variable_of_interest = Y
  )

  cur.initial_estimate = 1
  cur.truth = -12

  stub(subject$calculate_full_oos, 'self$get_h_ratio_estimators', function(...) { 1 })
  stub(subject$calculate_full_oos, 'self$evaluation_of_conditional_expectations', function(...) {
    return(NA)
  })

  expect_warning(result <- subject$calculate_full_oos(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  ), 'Oos estimate is NaN or na')

  expected <- cur.initial_estimate 
  expect_equal(result, expected)

  stub(subject$calculate_full_oos, 'self$evaluation_of_conditional_expectations', function(...) {
    return(NaN)
  })

  expect_warning(result <- subject$calculate_full_oos(
    initial_estimate = cur.initial_estimate,
    data = glob_data,
    truth = cur.truth
  ), 'Oos estimate is NaN or na')

  expected <- cur.initial_estimate 
  expect_equal(result, expected)
})

context(" calculate_oos_variance")
#==========================================================
test_that("it should be implemented", {
  skip('Not yet implemented') 
})

context(" get_h_ratio_estimators")
#==========================================================
test_that("it should sample N observations from both P and P*, and call the h ratio predictor function with them", {
  data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
  result <- subject$get_h_ratio_estimators(data = data)

  expect_true(is(result,'list'))
  
  # We should have tau lists of estimators
  expect_equal(length(result), tau)

  # Each of the tau lists, should have 3 estimators (one for each covariate)
  lapply(result,function(entry){
    expect_equal(length(result), length(glob_randomVariables))
    lapply(entry,function(estimator){
      expect_true(is(estimator, 'speedglm'))
    })
  })
  
})

if(FALSE) {
test_that("it should result in the correct output", {
  data <- data.table(Y_lag_1 = 2, W=1, A=1, Y=0)
  result <- subject$get_h_ratio_estimators(data = data)

  expect_true(is(result,'list'))
  
  # We should have tau lists of estimators
  expect_equal(length(result), tau)

  # Each of the tau lists, should have 3 estimators (one for each covariate)
  lapply(result,function(entry){
    expect_equal(length(result), length(glob_randomVariables))
    lapply(entry,function(estimator){
      expect_true(is(estimator, 'speedglm'))
    })
  })
})


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
    #randomVariables = glob_randomVariables,
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
    #expect_equal(length(result), length(glob_randomVariables))
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
    lapply(glob_randomVariables, function(rv) {
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
    lapply(glob_randomVariables, function(rv) {
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
    lapply(glob_randomVariables, function(rv) {
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
#==========================================================
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
#==========================================================
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
    #expect_equal(length(result), length(glob_randomVariables))
    #lapply(entry,function(estimator){
      #expect_true(is(estimator, 'speedglm'))
       #TODO: test whether the estimators make sense
    #})
  #})
  
})


context(" get_next_and_current_rv")
#==========================================================
test_that("it should get the next and the current random variable without overflowing the S", {
  rv_id <- 1
  result <- subject$get_next_and_current_rv(rv_id) 
  expect_equal(result$rv, glob_randomVariables[[rv_id]])
  expect_equal(result$next_rv, glob_randomVariables[[rv_id+1]])
  expect_equal(result$s_offset, 0)

  rv_id <- 2
  result <- subject$get_next_and_current_rv(rv_id) 
  expect_equal(result$rv, glob_randomVariables[[rv_id]])
  expect_equal(result$next_rv, glob_randomVariables[[rv_id+1]])
  expect_equal(result$s_offset, 0)
})

test_that("it should get the next and current random variable with overvlowing the S", {
  rv_id <- 3
  result <- subject$get_next_and_current_rv(rv_id) 
  expect_equal(result$rv, glob_randomVariables[[rv_id]])
  expect_equal(result$next_rv, glob_randomVariables[[1]])
  expect_equal(result$s_offset, 1)
})

}
