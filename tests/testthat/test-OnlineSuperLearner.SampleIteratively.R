library('mockery')
context("OnlineSuperLearner.SampleIteratively.R")
described.class <- OnlineSuperLearner.SampleIteratively

## Create mock OSL
glob_osl <- list('mock')
class(glob_osl) <- 'OnlineSuperLearner'

## Create mock SummaryMeasureGenerator
glob_SMG <- list('mock')
class(glob_SMG) <- 'SummaryMeasureGenerator'

## Create mock randomVariables
rv.W <- RandomVariable$new(formula = W ~ D, family = 'binomial')
rv.Y <- RandomVariable$new(formula = Y ~ W, family = 'gaussian')
glob_random_variables <- list(W = rv.W, Y = rv.Y)


glob_data <- data.table(W = seq(1,10), Y = seq(10,20))
glob_tau <- mock('tau')
glob_intervention <- mock('intervention')
glob_discrete <- TRUE
glob_return_type <- 'obs'
glob_start_from_variable <- rv.W
glob_start_from_time <- 1
glob_check = TRUE

context(' initialize')
#==========================================================
test_that("it should initalize and store the OSL", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  result <- subject$get_online_super_learner
  expect_false(is.null(result))
  expect_equal(result, glob_osl)
})

test_that("it should initalize and store the smg", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  result <- subject$get_summary_measure_generator
  expect_false(is.null(result))
  expect_equal(result, glob_SMG)
})

test_that("it should initalize, sort, and store the randomVariables", {
  subject <- described.class$new(osl = glob_osl, randomVariables = list(rv.Y, rv.W), summary_measure_generator = glob_SMG) 
  result <- subject$get_random_variables
  expect_false(is.null(result))
  expect_equal(result, list(W = rv.W, Y = rv.Y))
})

test_that("it should initalize and store the remove future rvs", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  result <- subject$is_removing_future_variables
  expect_false(is.null(result))
  expect_false(result)

  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG, remove_future_variables = TRUE) 
  result <- subject$is_removing_future_variables
  expect_false(is.null(result))
  expect_true(result)
})

test_that("it should initialize and store the random variable names", {
  subject <- described.class$new(osl = glob_osl, randomVariables = list(rv.Y, rv.W), summary_measure_generator = glob_SMG) 
  result <- subject$get_random_variable_names
  expect_false(is.null(result))
  expect_false(is(result, 'list'))
  expect_equal(result, c(rv.W$getY, rv.Y$getY))
})

context(" validate_parameters")
#==========================================================

context(' sample_iteratively')
#==========================================================
test_that("it should call the validate function with the correct parameters if check is true", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  stub(subject$sample_iteratively, 'self$validate_parameters', 
    function(start_from_variable, start_from_time, tau, discrete, return_type, intervention) {
      expect_equal(glob_start_from_variable, start_from_variable)
      expect_equal(glob_start_from_time, start_from_time)
      expect_equal(glob_tau, tau)
      expect_equal(glob_discrete, discrete)
      expect_equal(glob_return_type, return_type)
      expect_equal(glob_intervention, intervention)
      called <<- TRUE
      throw('stopping_execution')
    }
  )

  called <<- FALSE
  expect_error(subject$sample_iteratively(
    data = glob_data,
    tau = glob_tau,
    intervention = glob_intervention,
    discrete = glob_discrete,
    return_type = glob_return_type,
    start_from_variable = glob_start_from_variable,
    start_from_time = glob_start_from_time,
    check = glob_check
  ), 'stopping_execution', fixed = TRUE)
  expect_true(called)
})

test_that("it should call set start_from variable equal to the first one provided to it, if not provided", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 

  cur.start_from_variable <- NULL
  stub(subject$sample_iteratively, 'self$set_start_from_variable', 
    function(start_from_variable) {
      expect_equal(start_from_variable, cur.start_from_variable)
      called <<- TRUE
      throw('stopping_execution')
    }
  )
  
  called <<- FALSE
  expect_error(subject$sample_iteratively(
    data = glob_data,
    tau = glob_tau,
    intervention = glob_intervention,
    discrete = glob_discrete,
    return_type = glob_return_type,
    start_from_variable = cur.start_from_variable,
    start_from_time = glob_start_from_time,
    check = glob_check
  ), 'stopping_execution', fixed = TRUE)
  expect_true(called)

  cur.start_from_variable <- 'A'
  called <<- FALSE
  expect_error(subject$sample_iteratively(
    data = glob_data,
    tau = glob_tau,
    intervention = glob_intervention,
    discrete = glob_discrete,
    return_type = glob_return_type,
    start_from_variable = cur.start_from_variable,
    start_from_time = glob_start_from_time,
    check = glob_check
  ), 'stopping_execution', fixed = TRUE)
  expect_true(called)
})

test_that("it should call the sample_single_block function for each t from start til end", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  cur.tau <- 10
  stub(subject$sample_iteratively, 'self$sample_single_block', 
    function(current_time, data, start_from_variable, intervention, discrete) {
      cur.current_time <<- cur.current_time + 1

      expect_equal(current_time, cur.current_time)
      expect_equal(data, glob_data)
      expect_equal(intervention, glob_intervention)
      expect_equal(discrete, glob_discrete)

      list(normalized = data.table(1), denormalized = data.table(1))
    }
  )

  stub(subject$sample_iteratively, 'private$get_latest_covariates', function(...) { glob_data })

  cur.current_time <<- 0
  subject$sample_iteratively(
    data = glob_data,
    tau = cur.tau,
    intervention = glob_intervention,
    discrete = glob_discrete,
    return_type = glob_return_type,
    start_from_variable = glob_start_from_variable,
    start_from_time = glob_start_from_time,
    check = FALSE
  )

  # It always samples from 1 till tau + 1, I think because we have cases in which tau is essentially 1?
  expect_equal(cur.current_time, cur.tau)
})

test_that("it should return using the create_correct_result function", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  cur.tau <- 10
  mock_correct_result <- mock('correct_result')
  expected_result <- data.table(W = seq(0, (cur.tau - 1)), Y = seq(2, 2 + (cur.tau - 1)))
  expected_result_denormalized <- data.table(W = seq(88, 88 - (cur.tau - 1)), Y = rep(10, cur.tau) )


  stub(subject$sample_iteratively, 'self$sample_single_block', 
    function(current_time, start_from_variable, data, intervention, discrete) {
      cur <<- cur + 1
      list(normalized = data.table(W = cur - 2, Y = cur), denormalized = data.table(W = 90 - cur, Y = 10))
    }
  )

  stub(subject$sample_iteratively, 'self$create_correct_result', 
    function(result, result_denormalized_observations, return_type) {
      expect_is(result, 'data.table')
      expect_equal(result, expected_result)

      expect_is(result_denormalized_observations, 'data.table')
      expect_equal(result_denormalized_observations, expected_result_denormalized)

      expect_equal(return_type, glob_return_type)
      mock_correct_result
    }
  )

  stub(subject$sample_iteratively, 'private$get_latest_covariates', function(...) { glob_data })

  cur <<- 1
  result <- subject$sample_iteratively(
    data = glob_data,
    tau = cur.tau,
    intervention = glob_intervention,
    discrete = glob_discrete,
    return_type = glob_return_type,
    start_from_variable = glob_start_from_variable,
    start_from_time = glob_start_from_time,
    check = FALSE
  )

  # It always samples from 1 till tau + 1, I think because we have cases in which tau is essentially 1?
  expect_equal(result, mock_correct_result)
})

test_that("it should keep asking for new covariates, until T = tau", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  cur.tau <- 10
  mock_correct_result <- mock('correct_result')
  mock_block <- list(normalized = data.table(W = 1, Y = 2), denormalized = data.table(W = 1, Y = 8))

  expected_result <- data.table(W = seq(0, (cur.tau - 1)), Y = seq(2, 2 + (cur.tau - 1)))
  expected_result_denormalized <- data.table(W = seq(88, 88 - (cur.tau - 1)), Y = rep(10, cur.tau) )

  stub(subject$sample_iteratively, 'self$sample_single_block', function(...) { mock_block })
  stub(subject$sample_iteratively, 'self$create_correct_result', function(...) { mock_correct_result })


  stub(subject$sample_iteratively, 'private$get_latest_covariates', function(data) { 
    called_times <<- called_times + 1 
    called <<- TRUE
    glob_data 
  })

  called <<- FALSE
  called_times <<- 0
  result <- subject$sample_iteratively(
    data = glob_data,
    tau = cur.tau,
    intervention = glob_intervention,
    discrete = glob_discrete,
    return_type = glob_return_type,
    start_from_variable = glob_start_from_variable,
    start_from_time = glob_start_from_time,
    check = FALSE
  )
  expect_equal(called_times, cur.tau - 1)
  expect_true(called)

  ## Note the tau == glob_start_from_time
  called <<- FALSE
  result <- subject$sample_iteratively(
    data = glob_data,
    tau = glob_start_from_time,
    intervention = glob_intervention,
    discrete = glob_discrete,
    return_type = glob_return_type,
    start_from_variable = glob_start_from_variable,
    start_from_time = glob_start_from_time,
    check = FALSE
  )
  expect_false(called)
})


context(" sample_single_block")
#==========================================================
test_that("it should sample a single block for each of the random variables and the result should be a list of data tables", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  current_time = 1

  mock_rv_outcome <- list(normalized = 1, denormalized = 1)

  stub(subject$sample_single_block, 'self$sample_or_intervene_current_rv', function(...) { mock_rv_outcome })

  result <- subject$sample_single_block(
    data = glob_data[1,],
    current_time = current_time,
    intervention = glob_intervention,
    start_from_variable = rv.W,
    discrete = glob_discrete
  )
  expect_false(is.null(result))
  expect_is(result, 'list')
  expect_named(result, c('denormalized', 'normalized'))
  for (item in result) {
    expect_is(item, 'data.table')
  }
})

test_that("it should remove the future measurements (which are obviously not used for sampling) if this is set in the constructor", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG, 
                                 remove_future_variables = TRUE) 
  current_time = 1

  stub(subject$sample_single_block, 'self$sample_or_intervene_current_rv', function(data, ...) { 
    expect_true(is.na(data$Y))
    expect_true(is.na(data$W))
    throw('stop_execution')
  })

  expect_error(subject$sample_single_block(
    data = glob_data[1,],
    start_from_variable = rv.W,
    current_time = current_time,
    intervention = glob_intervention,
    discrete = glob_discrete
  ), 'stop_execution')

})

test_that("it should not remove past measurements", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG, 
                                 remove_future_variables = TRUE) 
  current_time = 1
  stub(subject$sample_single_block, 'self$sample_or_intervene_current_rv', function(data, ...) { 
    expect_true(is.na(data$Y))

    ## It should not set the data$W to NA
    expect_false(is.na(data$W))
    throw('stop_execution')
  })

  expect_error(subject$sample_single_block(
    data = glob_data[1,],
    start_from_variable = rv.Y,
    current_time = current_time,
    intervention = glob_intervention,
    discrete = glob_discrete
  ), 'stop_execution')
})


test_that("it should not remove any measurements (which are obviously not used for sampling) if this is set in the constructor", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG, 
                                 remove_future_variables = FALSE) 
  current_time = 1
  stub(subject$sample_single_block, 'self$sample_or_intervene_current_rv', function(data, ...) { 
    expect_false(is.na(data$Y))
    expect_false(is.na(data$W))
    throw('stop_execution')
  })

  expect_error(subject$sample_single_block(
    data = glob_data[1,],
    start_from_variable = rv.Y,
    current_time = current_time,
    intervention = glob_intervention,
    discrete = glob_discrete
  ), 'stop_execution')
})

test_that("it should call the should intervene function with the correct parameters", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  cur.current_time = 1

  mock_rv_outcome <- list(normalized = 1, denormalized = 1)

  stub(subject$sample_single_block, 'self$sample_or_intervene_current_rv', function(data, intervention, current_time, current_rv, discrete) {
    iter <<- iter + 1
    expect_equal(data, glob_data[1,])
    expect_equal(intervention, glob_intervention)
    expect_equal(current_time, cur.current_time)
    expect_equal(current_rv, glob_random_variables[[iter]])
    expect_equal(discrete, glob_discrete)
    mock_rv_outcome 
  })

  iter <<- 0
  subject$sample_single_block(
    data = glob_data[1,],
    current_time = cur.current_time,
    intervention = glob_intervention,
    start_from_variable = rv.W,
    discrete = glob_discrete
  )
  expect_equal(iter, length(glob_random_variables))
})

context(" sample_or_intervene_current_rv")
#==========================================================
test_that("it should parse the intervention", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  cur.current_time = 1
  stub(subject$sample_or_intervene_current_rv, 'InterventionParser.parse_intervention', 
    function( intervention, current_time, current_outcome ) { 
      expect_equal(intervention, glob_intervention)
      expect_equal(current_time, cur.current_time)
      expect_equal(current_outcome, rv.W$getY)
      throw('stop_execution')
  })

  expect_error(subject$sample_or_intervene_current_rv(
    data = glob_data[1,],
    intervention = glob_intervention,
    current_time = cur.current_time,
    current_rv = rv.W,
    discrete = glob_discrete
  ), 'stop_execution')
})

test_that("it should intervene if the parsed intervention says to intervene", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 

  mock_intervened_outcome <- list(normalized = 1, denormalized = 1)
  mock_intervention_parsed <- list(should_intervene = TRUE)

  cur.current_time = 1
  stub(subject$sample_or_intervene_current_rv, 'InterventionParser.parse_intervention', 
    function( intervention, current_time, current_outcome ) { 
      return(mock_intervention_parsed)
  })

  stub(subject$sample_or_intervene_current_rv, 'self$perform_intervention', 
    function(parsed_intervention) { 
      expect_equal(parsed_intervention, mock_intervention_parsed)
      called <<- TRUE
      return(mock_intervened_outcome) 
    }
  )

  stub(subject$sample_or_intervene_current_rv, 'self$perform_sample', 
    function(...) { 
      should_not_be_called <<- TRUE
      return(mock_intervened_outcome) 
    }
  )

  called <<- FALSE
  should_not_be_called <<- FALSE
  result <- subject$sample_or_intervene_current_rv(
    data = glob_data[1,],
    intervention = glob_intervention,
    current_time = cur.current_time,
    current_rv = rv.W,
    discrete = glob_discrete
  )
  expect_true(called)
  expect_false(should_not_be_called)
})

test_that("it should not intervene if the parsed intervention doesnt say to intervene", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 

  mock_intervened_outcome <- list(normalized = 1, denormalized = 1)
  mock_intervention_parsed <- list(should_intervene = FALSE)

  cur.current_time = 1
  stub(subject$sample_or_intervene_current_rv, 'InterventionParser.parse_intervention', 
    function( intervention, current_time, current_outcome ) { 
      return(mock_intervention_parsed)
  })

  stub(subject$sample_or_intervene_current_rv, 'self$perform_intervention', 
    function(...) { 
      should_not_be_called <<- TRUE
      return(mock_intervened_outcome) 
    }
  )

  stub(subject$sample_or_intervene_current_rv, 'self$perform_sample', 
    function(data, current_rv, discrete) { 
      expect_equal(data, glob_data[1,])
      expect_equal(current_rv, rv.W)
      expect_equal(discrete, glob_discrete)
      called <<- TRUE
      return(mock_intervened_outcome) 
    }
  )

  called <<- FALSE
  should_not_be_called <<- FALSE

  result <- subject$sample_or_intervene_current_rv(
    data = glob_data[1,],
    intervention = glob_intervention,
    current_time = cur.current_time,
    current_rv = rv.W,
    discrete = glob_discrete
  )
  expect_true(called)
  expect_false(should_not_be_called)
})

context(" perform_intervention")
#==========================================================
test_that("it should return a list with a normalized and denormalized value according to the intervention provided", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  what <- 123
  mock_intervention <- list(what = what)
  result <- subject$perform_intervention(mock_intervention)
  expect_named(result, c('normalized', 'denormalized'))
  expect_equal(result$normalized, what)
  expect_equal(result$denormalized, what)
})

context(" perform_sample")
#==========================================================
test_that("it should call the osl predict function", {
  mock_normal_outcome <- list(normalized = 1, denormalized = 1)
  cur.osl <- list(predict = function(data, randomVariables, discrete, continuous, all_estimators, sample)  {
    expect_equal(data, glob_data[1,])                  
    expect_equal(randomVariables, c(rv.W))                  
    expect_equal(discrete, glob_discrete)                  
    expect_equal(continuous, !glob_discrete)                  
    expect_equal(all_estimators, FALSE)                  
    expect_equal(sample, TRUE)                  
    called <<- TRUE
    mock_normal_outcome
  })
  class(cur.osl) <- 'OnlineSuperLearner'

  subject <- described.class$new(osl = cur.osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 

  called <<- FALSE
  result <- subject$perform_sample(
    data = glob_data[1,],
    current_rv = rv.W,
    discrete = glob_discrete
  )
  expect_true(called)
})

context(" create_correct_result")
#==========================================================
test_that("it should return the denormalized observations if the return type = observations", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 

  result <- mock('result')
  result_denormalized_observations <- data.table(A = 1, W = 2, Y = 3)
  cur.return_type = 'observations'

  result <- subject$create_correct_result(result, result_denormalized_observations, cur.return_type)
  expect_is(result, 'data.table')
  expect_named(result, lapply(glob_random_variables, function(rv) rv$getY) %>% unlist %>% unname)
  expect_equal(result$W, 2)
  expect_equal(result$Y, 3)
})

test_that("it should return the summary measures (normalized) if the return type = summary_measures", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 

  result_denormalized_observations <- mock('result')
  result <- data.table(A = 1, W = 2, Y = 3)
  cur.return_type = 'summary_measures'

  result <- subject$create_correct_result(result, result_denormalized_observations, cur.return_type)
  expect_is(result, 'data.table')
  expect_named(result, c('A'))
  expect_equal(result$A, 1)
})

test_that("it should return the normalized results by default", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 

  result_denormalized_observations <- mock('result')
  result <- data.table(A = 1, W = 2, Y = 3)
  cur.return_type = 'lalalal'

  the_result <- subject$create_correct_result(result, result_denormalized_observations, cur.return_type)
  expect_is(the_result, 'data.table')
  expect_named(the_result, names(result))
  expect_equal(the_result, result)
})

context(" set_start_from_variable")
#==========================================================
test_that("it should should return the provided start_from variable if it is not nil", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  mock_start_from <- mock('start from')
  result <- subject$set_start_from_variable(mock_start_from)
  expect_equal(result, mock_start_from)
})

test_that("it should should return the first of the random variables if it is null", {
  subject <- described.class$new(osl = glob_osl, randomVariables = glob_random_variables, summary_measure_generator = glob_SMG) 
  result <- subject$set_start_from_variable(NULL)
  expect_equal(result, glob_random_variables[[1]])

  result <- subject$set_start_from_variable()
  expect_equal(result, glob_random_variables[[1]])
})

