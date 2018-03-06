context("InterventionEffectCalculator.R")

described.class <- InterventionEffectCalculator
rv.W <- RelevantVariable$new(formula = W ~ D, family = 'binomial')
rv.Y <- RelevantVariable$new(formula = Y ~ W, family = 'gaussian')

glob_bootstrap_iterations <- 5
glob_outcome_variable <- rv.Y$getY
glob_parallel <- TRUE
glob_osl = list('x')
class(glob_osl) <- 'OnlineSuperLearner'

context(" initialize")
#==========================================================
test_that("it should initialize and store the variables", {
  cur.bootstrap_iterations <- 500
  cur.parallel <- TRUE
  cur.outcome_variable <- rv.Y$getY

  expect_error(
    subject <- described.class$new(
      bootstrap_iterations = cur.bootstrap_iterations,
      outcome_variable = cur.outcome_variable,
      parallel = cur.parallel
    ), NA
  )

  expect_equal(subject$get_bootstrap_iterations, cur.bootstrap_iterations)
  expect_equal(subject$get_outcome_variable, cur.outcome_variable)
  expect_equal(subject$is_parallel, cur.parallel)
})

test_that("it should throw whenever bootstrap_iterations are not int", {
  cur.parallel <- TRUE
  cur.outcome_variable <- rv.Y$getY
  for (cur.bootstrap_iterations in list(0, -1, NULL, NA)) {
    expect_error(
      subject <- described.class$new(
        bootstrap_iterations = cur.bootstrap_iterations,
        outcome_variable = cur.outcome_variable,
        parallel = cur.parallel
      ), "Argument 'bootstrap_iterations' "
    )
  }
})

test_that("it should throw whenever outcomevariable is not a string", {
  cur.parallel <- TRUE
  cur.bootstrap_iterations <- 500
  for (cur.outcome_variable  in list(rv.Y)) {
    expect_error(
      subject <- described.class$new(
        bootstrap_iterations = cur.bootstrap_iterations,
        outcome_variable = cur.outcome_variable,
        parallel = cur.parallel
      ), "cannot coerce type 'environment' to vector of type 'character'"
    )
  }
})

test_that("it should not throw whenever parallel is a boolean", {
  cur.bootstrap_iterations <- 500
  cur.outcome_variable <- rv.Y$getY
  for (cur.parallel in list(TRUE,FALSE)) {
    expect_error(
      subject <- described.class$new(
        bootstrap_iterations = cur.bootstrap_iterations,
        outcome_variable = cur.outcome_variable,
        parallel = cur.parallel
      ), NA
    )
  }
})

context(" calculate_intervention_effect")
#==========================================================
test_that("it should test whether all interventions are valid if the check flag is set", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = glob_parallel
  )

  cur.interventions = seq(10)
  names(cur.interventions) = LETTERS[1:length(cur.interventions)]

  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  cur.tau = mock('tau')
  cur.check = TRUE

  stub(subject$calculate_intervention_effect, 'self$evaluate_single_intervention', 
    function(osl, initial_data, intervention, tau, discrete) { }
  )

  mock_interventionparser <- mock(TRUE, cycle=TRUE)

  with_mock(InterventionParser.valid_intervention = mock_interventionparser, 
    subject$calculate_intervention_effect(
      osl = glob_osl, 
      interventions = cur.interventions, 
      discrete = cur.discrete, 
      initial_data = cur.intial_data, 
      tau = cur.tau, 
      check = cur.check
    )
  )

  expect_called(mock_interventionparser, length(cur.interventions))
  args <- mock_args(mock_interventionparser)
  for (idx in 1:length(cur.interventions) ) {
    curarg <- args[[idx]]
    expect_equal(curarg$intervention, cur.interventions[[idx]])
  }
})

test_that("it should throw when the provided osl is not an osl if the check flag is set", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = glob_parallel
  )
  
  cur.interventions = seq(10)
  names(cur.interventions) = LETTERS[1:length(cur.interventions)]

  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  cur.tau = mock('tau')
  cur.check = TRUE

  expect_error(subject$calculate_intervention_effect(
    osl = 'NOTANOSL', 
    interventions = cur.interventions, 
    discrete = cur.discrete, 
    initial_data = cur.intial_data, 
    tau = cur.tau, 
    check = cur.check
  ), "InterventionParser.valid_intervention(intervention = intervention) is not TRUE", fixed = TRUE)
  
})

test_that("it should give a warning if the names of the interventions are nil", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = glob_parallel
  )

  cur.interventions = seq(10)
  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  cur.tau = mock('tau')
  cur.check = FALSE 

  stub(subject$calculate_intervention_effect, 'self$evaluate_single_intervention', 
    function(osl, initial_data, intervention, tau, discrete) { }
  )

  mock_interventionparser <- mock(TRUE, cycle=TRUE)

  expect_warning(subject$calculate_intervention_effect(
    osl = glob_osl, 
    interventions = cur.interventions, 
    discrete = cur.discrete, 
    initial_data = cur.intial_data, 
    tau = cur.tau, 
    check = cur.check
  ), 'Provided interventions do not have a name. Please name them for data management. Continuing without any names.')
})

test_that("it should call the evaluate single intervention function with the correct parameters", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = glob_parallel
  )

  cur.interventions = seq(10)
  names(cur.interventions) = LETTERS[1:length(cur.interventions)]

  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  cur.tau = mock('tau')
  cur.check = FALSE 

  stub(subject$calculate_intervention_effect, 'self$evaluate_single_intervention', 
    function(osl, initial_data, intervention, tau, discrete) { 
      expect_equal(osl, glob_osl)
      expect_equal(initial_data, cur.initial_data)
      expect_equal(intervention, cur.interventions[[iter]])
      expect_equal(tau, cur.tau)
      expect_equal(discrete, cur.discrete)
      iter <<- iter + 1
    }
  )

  iter <<- 1
  subject$calculate_intervention_effect(
    osl = glob_osl, 
    interventions = cur.interventions, 
    discrete = cur.discrete, 
    initial_data = cur.initial_data, 
    tau = cur.tau, 
    check = cur.check
  )
})

test_that("it should return the result of each of the interventions", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = glob_parallel
  )

  cur.interventions = seq(10)
  names(cur.interventions) = LETTERS[1:length(cur.interventions)]

  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  cur.tau = mock('tau')
  cur.check = FALSE 

  mock_result <- c(1,2,3,4)

  stub(subject$calculate_intervention_effect, 'self$evaluate_single_intervention', 
    function(osl, initial_data, intervention, tau, discrete) {
      return(mock_result)
    }
  )

  result <- subject$calculate_intervention_effect(
    osl = glob_osl, 
    interventions = cur.interventions, 
    discrete = cur.discrete, 
    initial_data = cur.initial_data, 
    tau = cur.tau, 
    check = cur.check
  )

  expect_is(result, 'list')
  expect_length(result, length(cur.interventions)) 
  for (res in result) {
    expect_equal(res, mock_result)
  }
})

context(" evaluate_single_intervention")
#==========================================================
test_that("it should call the sample_iteratively function bootstrap_iterations number of times with the correct arguments", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = FALSE
  )

  cur.intervention = mock('intervention')

  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  cur.tau = 1

  mock_result <- data.table(W = c(1,2,3,4), Y = c(4,3,2,1))

  stub(subject$evaluate_single_intervention, 'osl$sample_iteratively', 
    function(data, intervention, discrete, tau) {
      expect_equal(data, cur.initial_data)
      expect_equal(tau, cur.tau)
      expect_equal(intervention, cur.intervention)
      expect_equal(discrete, cur.discrete)
      iter <<- iter + 1
      return(mock_result)
    }
  )

  iter <<- 0
  subject$evaluate_single_intervention(
    osl = glob_osl, 
    initial_data = cur.initial_data, 
    intervention = cur.intervention, 
    tau = cur.tau, 
    discrete = cur.discrete 
  )
  expect_equal(iter, glob_bootstrap_iterations)
})

test_that("it should return the tau'th block and the outcome_variable variable for each of the bootstrap iterations", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = FALSE
  )

  cur.intervention = mock('intervention')

  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  cur.tau = 3

  mock_result <- data.table(W = c(1,2,3,4), Y = c(4,3,2,1))

  stub(subject$evaluate_single_intervention, 'osl$sample_iteratively', 
    function(data, intervention, discrete, tau) {
      return(mock_result)
    }
  )

  result <- subject$evaluate_single_intervention(
    osl = glob_osl, 
    initial_data = cur.initial_data, 
    intervention = cur.intervention, 
    tau = cur.tau, 
    discrete = cur.discrete 
  )

  expected_single_iteration <- unlist(mock_result[cur.tau, glob_outcome_variable, with=FALSE])
  expected <- rep(expected_single_iteration, glob_bootstrap_iterations) %>% unname
  expect_equal(result, expected)
})

context(" perform_initial_estimation")
#==========================================================
test_that("it should throw an error when the provided data is not a data table", {

  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = FALSE
  )

  cur.initial_data = mock('initial_data')
  expect_error(subject$perform_initial_estimation(
    osl = mock(), 
    interventions = mock(), 
    discrete = mock() ,
    initial_data = cur.initial_data,
    tau = mock()
  ), "Argument 'initial_data' is neither of nor inherits class data.table: mock" )

})

test_that("it should call the intervention effect calculator with the correct parameters", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = FALSE
  )

  cur.interventions = seq(10)
  names(cur.interventions) = LETTERS[1:length(cur.interventions)]

  cur.discrete = mock('discrete')
  cur.initial_data = data.table(W=10, Y=1)
  cur.tau = 3

  mock_result <- data.table(W = c(1,2,3,4), Y = c(4,3,2,1))

  stub(subject$perform_initial_estimation, 'self$calculate_intervention_effect', 
    function(osl, interventions, discrete, initial_data, tau, check) {
      expect_equal(osl, glob_osl)
      expect_equal(interventions, cur.interventions)
      expect_equal(discrete, cur.discrete)
      expect_equal(initial_data, cur.initial_data)
      expect_equal(tau, cur.tau)
      called <<- TRUE
      return(mock_result)
    }
  )

  result <- subject$perform_initial_estimation(
    osl = glob_osl, 
    interventions = cur.interventions, 
    discrete = cur.discrete ,
    initial_data = cur.initial_data,
    tau = cur.tau
  )
  expect_true(called)
})

test_that("it should return the mean of all outcomes with their correct names", {
  subject <- described.class$new(
    bootstrap_iterations = glob_bootstrap_iterations,
    outcome_variable = glob_outcome_variable,
    parallel = FALSE
  )

  cur.interventions = seq(10)
  names(cur.interventions) = LETTERS[1:length(cur.interventions)]

  cur.discrete = mock('discrete')
  cur.initial_data = mock('initial_data')
  class(cur.initial_data) <- 'data.table'
  cur.tau = 3

  mock_result <- c(1,2,3,4,5)

  stub(subject$perform_initial_estimation, 'self$calculate_intervention_effect', 
    function(interventions, ...) {
      cur.result <- lapply(seq_along(interventions), function(x) mock_result)
      names(cur.result) <- names(interventions)
      iter <<- iter + 1
      return(cur.result)
    }
  )


  iter <<- 1
  result <- subject$perform_initial_estimation(
    osl = glob_osl, 
    interventions = cur.interventions, 
    discrete = cur.discrete ,
    initial_data = cur.initial_data,
    tau = cur.tau
  )

  expect_is(result, 'list')
  expect_named(result, names(cur.interventions))
  for (entry in result) {
    expect_is(entry, 'numeric')
    expect_equal(entry, mean(mock_result))
  }
})
