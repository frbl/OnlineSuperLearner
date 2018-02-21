library(mockery)

context("CrossValidationRiskCalculator.R")
described.class <- CrossValidationRiskCalculator
subject <- described.class$new()

## Initialize some variables
glob_randomVariables <- list(list(getY='A', getFamily='binomial'),
                        list(getY='W', getFamily='gaussian'),
                        list(getY='Y', getFamily='gaussian')
                        )
randomVariable_names <- lapply(glob_randomVariables, function(rv) rv$getY) %>% unlist

## Create predicted outcomes
glob_predicted.outcome <- list()
glob_predicted.outcome <- append(glob_predicted.outcome, list(data.table(c(9,8,7,6), c(5,4,3,2), c(4,3,2,1))))
names(glob_predicted.outcome[[length(glob_predicted.outcome)]]) <- sapply(glob_randomVariables, function(x) x$getY)
glob_predicted.outcome <- append(glob_predicted.outcome, list(data.table(c(5,5,5,5), c(4,4,4,4), c(3,3,3,3))))
names(glob_predicted.outcome[[length(glob_predicted.outcome)]]) <- sapply(glob_randomVariables, function(x) x$getY)
names(glob_predicted.outcome) <- c('a','b')

## Create predicted outcomes with both normalized and denormalized data
glob_new_prediction <- list()
glob_new_prediction$denormalized <- glob_predicted.outcome
maxval <- glob_predicted.outcome %>% unlist %>% max
glob_new_prediction$normalized <- lapply(glob_predicted.outcome, function(x) as.data.table(lapply(x, function(y) y / maxval)))

## Create observed outcomes
glob_observed.outcome <- data.table(c(1,2,3,4), c(-1,-2,-3,-4), c(0.1,0.2,0.3,0.4))
colnames(glob_observed.outcome) <-  randomVariable_names

context(" calculate_evaluation")
##=====================================================================
test_that("it should call the evaluate single outcome function with the correct parameters", {
  subject <- described.class$new()
  iter <<- 1

  ## We stub the loss function to return the data it gets, so we can check that it received the correct data
  stub(subject$calculate_evaluation, 'self$evaluate_single_outcome',
    function(observed.outcome, predicted.outcome, randomVariables, add_evaluation_measure_name) {
      expect_is(observed.outcome, 'data.table')
      expect_equal(observed.outcome, glob_observed.outcome)

      expect_is(predicted.outcome, 'data.table')
      expect_equal(predicted.outcome, glob_new_prediction$normalized[[iter]])

      expect_is(randomVariables, 'list')
      expect_equal(randomVariables, glob_randomVariables)

      expect_false(add_evaluation_measure_name)
      iter <<- iter + 1
    }
  )

  result <- subject$calculate_evaluation(predicted.outcome = glob_new_prediction,
                              observed.outcome = glob_observed.outcome,
                              randomVariables = glob_randomVariables,
                              add_evaluation_measure_name=FALSE)
})

test_that("it should return the output in a list of data.tables", {
  subject <- described.class$new()
  result <- subject$calculate_evaluation(predicted.outcome = glob_new_prediction,
                              observed.outcome = glob_observed.outcome,
                              randomVariables = glob_randomVariables,
                              add_evaluation_measure_name=FALSE)
  expect_is(result, 'list')
  expect_equal(length(result), length(names(glob_predicted.outcome)))

  lapply(result, function(entry) expect_is(entry, 'data.table'))
  lapply(result, function(entry) length(entry) == length(randomVariable_names)) %>%
    unlist %>%
    all %>%
    expect_true
})

test_that("it should use the normalized data if it is available", {
  subject <- described.class$new()

  ## We stub the loss function to return the data it gets, so we can check that it received the correct data
  input_prediction <- glob_new_prediction$normalized
  stub(subject$calculate_evaluation, 'self$evaluate_single_outcome',
    function(observed.outcome, predicted.outcome, randomVariables, add_evaluation_measure_name) {
      expect_is(predicted.outcome, 'data.table')
      expect_equal(predicted.outcome, input_prediction[[iter]])
      iter <<- iter + 1
    }
  )

  ## Without the normalized input it should use everything
  iter <<- 1
  result <- subject$calculate_evaluation(predicted.outcome = input_prediction,
                              observed.outcome = glob_observed.outcome,
                              randomVariables = glob_randomVariables,
                              add_evaluation_measure_name=FALSE)

  ## With the normalized input it should use the normalized part
  iter <<- 1
  result <- subject$calculate_evaluation(predicted.outcome = glob_new_prediction,
                              observed.outcome = glob_observed.outcome,
                              randomVariables = glob_randomVariables,
                              add_evaluation_measure_name=FALSE)
})

context(" evaluate_single_outcome")
#=====================================================================
test_that("it should call the Evaluation.get_evaluation_function with the correct arguments", {
  subject <- described.class$new()
  prediction <- glob_new_prediction$normalized[[1]]

  mock_lossFn <- mock(c(test_loss = 123), cycle = TRUE)
  mock_get_evaluation <- mock(mock_lossFn, cycle = TRUE)
  with_mock(Evaluation.get_evaluation_function = mock_get_evaluation,
    result <- subject$evaluate_single_outcome(predicted.outcome = prediction,
                                observed.outcome = glob_observed.outcome,
                                randomVariables = glob_randomVariables,
                                add_evaluation_measure_name=FALSE)
  )

  ## First: the get_evaluation_function
  args <- mock_args(mock_get_evaluation)
  expect_length(args, 3)
  for (idx in 1:length(args)) {
    arg <- args[[idx]]
    expect_equal(arg$family, glob_randomVariables[[idx]]$getFamily)
    expect_false(arg$useAsLoss)
  }
})

test_that("it should call the resulting Loss function with the correct arguments", {
  subject <- described.class$new()
  prediction <- glob_new_prediction$normalized[[1]]

  mock_lossFn <- mock(c(test_loss = 123), cycle = TRUE)
  mock_get_evaluation <- mock(mock_lossFn, cycle = TRUE)
  with_mock(Evaluation.get_evaluation_function = mock_get_evaluation,
    result <- subject$evaluate_single_outcome(predicted.outcome = prediction,
                                observed.outcome = glob_observed.outcome,
                                randomVariables = glob_randomVariables,
                                add_evaluation_measure_name=FALSE)
  )

  ## Second: the lossFN
  args <- mock_args(mock_lossFn)
  expect_length(args, 3)
  for (idx in 1:length(args)) {
    arg <- args[[idx]]
    rv <- glob_randomVariables[[idx]]$getY
    expect_equal(arg$data.observed, glob_observed.outcome[[rv]])
    expect_equal(arg$data.predicted, prediction[[rv]])
  }
})

test_that("it should set the correct names for the output with the evaluation metric if true", {
  subject <- described.class$new()
  prediction <- glob_new_prediction$normalized[[1]]

  mock_lossFn <- mock(c(test_loss = 123), cycle = TRUE)
  mock_get_evaluation <- mock(mock_lossFn, cycle = TRUE)
  with_mock(Evaluation.get_evaluation_function = mock_get_evaluation,
    result <- subject$evaluate_single_outcome(predicted.outcome = prediction,
                                observed.outcome = glob_observed.outcome,
                                randomVariables = glob_randomVariables,
                                add_evaluation_measure_name=TRUE)
  )
  expect_named(result, c('test_loss.A','test_loss.W','test_loss.Y'))
})

test_that("it should set the correct names for the output without the evaluation metric if false", {
  subject <- described.class$new()
  prediction <- glob_new_prediction$normalized[[1]]

  mock_lossFn <- mock(c(test_loss = 123), cycle = TRUE)
  mock_get_evaluation <- mock(mock_lossFn, cycle = TRUE)
  with_mock(Evaluation.get_evaluation_function = mock_get_evaluation,
    result <- subject$evaluate_single_outcome(predicted.outcome = prediction,
                                observed.outcome = glob_observed.outcome,
                                randomVariables = glob_randomVariables,
                                add_evaluation_measure_name=FALSE)
  )
  expect_named(result, c('A','W','Y'))
})

context(" calculate_risk")
#=====================================================================
test_that("it should use the normalized data if it is available", {
  subject <- described.class$new()

  ## We stub the loss function to return the data it gets, so we can check that it received the correct data
  stub(subject$calculate_risk, 'self$calculate_risk_of_single_estimator',
    function(observed.outcome, predicted.outcome, randomVariables) {
      expect_is(predicted.outcome, 'data.table')
      expect_equal(predicted.outcome, glob_predicted.outcome[[iter]])
      iter <<- iter + 1
    }
  )

  ## Without the normalized input it should use everything
  iter <<- 1
  result <- subject$calculate_risk(predicted.outcome = glob_predicted.outcome,
                              observed.outcome = glob_observed.outcome,
                              randomVariables = glob_randomVariables)

  ## With the normalized input it should use the normalized part
  iter <<- 1
  result <- subject$calculate_risk(predicted.outcome = list(normalized = glob_predicted.outcome),
                              observed.outcome = glob_observed.outcome,
                              randomVariables = glob_randomVariables)
})

test_that("it should throw if the predicted outcome is not a list and check is true", {
  predicted.outcome <- 'not a list'
  msg <- "Argument 'predicted.outcome' is neither of nor inherits class list: character"
  expect_error(subject$calculate_risk(predicted.outcome = predicted.outcome,
                         observed.outcome = glob_observed.outcome,
                         randomVariables = glob_randomVariables, check = TRUE), msg)
})

test_that("it should throw if the observed.outcome is not a data.table and check is true", {
  observed.outcome <- 'not a data.table'
  msg <- "Argument 'observed.outcome' is neither of nor inherits class data.table: character"
  expect_error(subject$calculate_risk(predicted.outcome = glob_predicted.outcome,
                         observed.outcome = observed.outcome,
                         randomVariables = glob_randomVariables, check = TRUE), msg)
})

test_that("it should set the correct names to the correct risks", {
  evaluation_mock <- mock(function(...) 42, cycle=TRUE)
  with_mock(Evaluation.get_evaluation_function = evaluation_mock,
  result <-subject$calculate_risk(predicted.outcome = glob_predicted.outcome,
                          observed.outcome = glob_observed.outcome,
                          randomVariables = glob_randomVariables))
  expect_equal(names(result), names(glob_predicted.outcome))

  lapply(result, function(x) names(x) == randomVariable_names) %>%
    unlist %>%
    all %>%
    expect_true

})

context(" calculate_risk_of_single_estimator")
#=====================================================================
test_that("it should call the Evaluation.get_evaluation_function with the correct arguments", {
  subject <- described.class$new()
  prediction <- glob_new_prediction$a

  mock_lossFn <- mock(c(test_loss = 123), cycle = TRUE)
  mock_get_evaluation <- mock(mock_lossFn, cycle = TRUE)
  with_mock(Evaluation.get_evaluation_function = mock_get_evaluation,
    result <- subject$calculate_risk_of_single_estimator(predicted.outcome = prediction,
                                observed.outcome = glob_observed.outcome,
                                randomVariables = glob_randomVariables)
  )

  ## First: the get_evaluation_function
  args <- mock_args(mock_get_evaluation)
  expect_length(args, 3)
  expect_called(mock_get_evaluation,length(glob_randomVariables))
  for (idx in 1:length(args)) {
    arg <- args[[idx]]
    expect_equal(arg$family, glob_randomVariables[[idx]]$getFamily)
    expect_true(arg$useAsLoss)
  }
})

test_that("it should call the resulting Loss function with the correct arguments", {
  subject <- described.class$new()
  prediction <- glob_new_prediction$normalized[[1]]

  mock_lossFn <- mock(c(test_loss = 123), cycle = TRUE)
  mock_get_evaluation <- mock(mock_lossFn, cycle = TRUE)
  with_mock(Evaluation.get_evaluation_function = mock_get_evaluation,
    result <- subject$calculate_risk_of_single_estimator(predicted.outcome = prediction,
                                observed.outcome = glob_observed.outcome,
                                randomVariables = glob_randomVariables)
  )

  ## Second: the lossFN
  args <- mock_args(mock_lossFn)
  expect_called(mock_lossFn,length(glob_randomVariables))
  expect_length(args, 3)
  for (idx in 1:length(args)) {
    arg <- args[[idx]]
    rv <- glob_randomVariables[[idx]]$getY
    expect_equal(arg$data.observed, glob_observed.outcome[[rv]])
    expect_equal(arg$data.predicted, prediction[[rv]])
  }
})

test_that("it should return the output in the correct format", {
  subject <- described.class$new()
  prediction <- glob_new_prediction$normalized[[1]]

  mock_lossFn <- mock(c(test_loss = 123), cycle = TRUE)
  mock_get_evaluation <- mock(mock_lossFn, cycle = TRUE)
  with_mock(Evaluation.get_evaluation_function = mock_get_evaluation,
    result <- subject$calculate_risk_of_single_estimator(predicted.outcome = prediction,
                                observed.outcome = glob_observed.outcome,
                                randomVariables = glob_randomVariables)
  )

  expect_is(result, 'data.table')
  expect_equal(nrow(result), 1)
  expect_equal(ncol(result), length(glob_randomVariables))
  expect_equal(colnames(result), randomVariable_names)
})

test_that("it should call the loss function with the correct data", {
  loss_mock <- mock(42, cycle=TRUE)

  with_mock(Evaluation.get_evaluation_function = function(...) loss_mock,
    subject$calculate_risk(predicted.outcome = glob_predicted.outcome,
                           observed.outcome = glob_observed.outcome,
                           randomVariables = glob_randomVariables))

  expect_called(loss_mock,length(glob_randomVariables) * length(glob_predicted.outcome))
  args <- mock_args(loss_mock)

  ## Check if th observed data is passed in correctly
  lapply(1:length(args), function(i) {
    j <- ((i - 1) %% length(glob_randomVariables)) + 1
    glob_observed.outcome[[j]] == args[[i]][['data.observed']]
  }) %>% unlist %>% all %>% expect_true

  k <- 0
  ## Check if th predicted data is passed in correctly
  for(i in 1:length(args)){
    j <- ((i - 1) %% length(glob_randomVariables)) + 1
    if(i %% (length(glob_randomVariables)) == 1 ) k <- k + 1
    glob_predicted.outcome[[k]][[j]] == args[[i]][['data.predicted']]
  } %>% unlist %>% all %>% expect_true
})


context(" update_risk")
#=====================================================================
test_that("it should throw if the predicted outcomes are not a list", {
  erroneous_inputs <- list(NULL, 'hoi')
  expected_msg <- "Argument 'predicted.outcome' is neither of nor inherits class list: "
  for (input in erroneous_inputs) {
    expect_error(subject$update_risk(predicted.outcome = input,
                                      observed.outcome = glob_observed.outcome,
                                      randomVariables = glob_randomVariables,
                                      current_risk = 0, current_count = 0, check = TRUE), expected_msg, fixed = TRUE)
  }
})

test_that("it should throw if the predicted outcomes are not a list", {
  input <- list()
  expected_msg <- 'Predicted outcome is empty!'
  expect_error(subject$update_risk(predicted.outcome = input,
                                    observed.outcome = glob_observed.outcome,
                                    randomVariables = glob_randomVariables,
                                    current_risk = 0, current_count = 0, check = TRUE), expected_msg, fixed = TRUE)
})

test_that("it should throw if the observed outcomes are empty and check is true", {
  erroneous_inputs <- list(NULL, list(), 'hoi')
  expected_msg <- "Argument 'observed.outcome' is neither of nor inherits class data.table:"
  for (input in erroneous_inputs) {
    expect_error(subject$update_risk(predicted.outcome = glob_predicted.outcome,
                                     observed.outcome = input,
                                     randomVariables = glob_randomVariables,
                                     current_risk = 0, current_count = 0, check = TRUE), expected_msg)
  }
})

test_that("it should should call the update_single_risk function with the correct parameters when a risk is not available", {
  subject <- described.class$new()
  cur.current_risk  <- list()
  cur.current_count <- 0

  mocked_risks <- list(a = data.table(A = 1, W = 2, Y=3), b = data.table(A = 4, W = 5, Y = 6))

  stub(subject$update_risk, 'self$calculate_risk',
    function(predicted.outcome, observed.outcome, randomVariables) {
      expect_false(is.null(predicted.outcome))
      expect_equal(predicted.outcome, glob_predicted.outcome)
      expect_false(is.null(observed.outcome))
      expect_equal(observed.outcome, glob_observed.outcome)
      expect_false(is.null(randomVariables))
      expect_equal(randomVariables, glob_randomVariables)
      return(mocked_risks)
    }
  )

  stub(subject$update_risk, 'self$update_single_risk',
    function(old_risk, new_risks, current_count, randomVariables) {
      expect_null(old_risk)
      expect_equal(new_risks, mocked_risks[[iter]])
      expect_equal(current_count, cur.current_count)
      expect_equal(randomVariables, glob_randomVariables)
      iter <<- iter + 1
    }
  )

  iter <<- 1
  updated_risk <- subject$update_risk(
    predicted.outcome = glob_predicted.outcome,
    observed.outcome = glob_observed.outcome,
    randomVariables = glob_randomVariables,
    current_count = cur.current_count,
    current_risk = cur.current_risk
  )

})

test_that("it should should call the update_single_risk function with the correct parameters when a risk is available", {
  subject <- described.class$new()
  cur.current_risk  <- list(a = data.table(A = 1, W = 2, Y=3), b = data.table(A = 4, W = 5, Y = 6))
  cur.current_count <- 1

  mocked_risks <- list(a = data.table(A = 1, W = 2, Y=3), b = data.table(A = 4, W = 5, Y = 6))

  stub(subject$update_risk, 'self$calculate_risk',
    function(predicted.outcome, observed.outcome, randomVariables) {
      expect_false(is.null(predicted.outcome))
      expect_equal(predicted.outcome, glob_predicted.outcome)
      expect_false(is.null(observed.outcome))
      expect_equal(observed.outcome, glob_observed.outcome)
      expect_false(is.null(randomVariables))
      expect_equal(randomVariables, glob_randomVariables)
      return(mocked_risks)
    }
  )

  stub(subject$update_risk, 'self$update_single_risk',
    function(old_risk, new_risks, current_count, randomVariables) {
      expect_equal(old_risk, cur.current_risk[[iter]])
      expect_equal(new_risks, mocked_risks[[iter]])
      expect_equal(current_count, cur.current_count)
      expect_equal(randomVariables, glob_randomVariables)
      iter <<- iter + 1
    }
  )

  iter <<- 1
  updated_risk <- subject$update_risk(
    predicted.outcome = glob_predicted.outcome,
    observed.outcome = glob_observed.outcome,
    randomVariables = glob_randomVariables,
    current_count = cur.current_count,
    current_risk = cur.current_risk
  )
})

test_that("is should set the correct names for the calculated risks", {
  subject <- described.class$new()
  cur.current_risk  <- list(a = data.table(A = 1, W = 2, Y=3), b = data.table(A = 4, W = 5, Y = 6))
  cur.current_count <- 1

  mocked_risks <- list(a = data.table(A = 1, W = 2, Y=3), b = data.table(A = 4, W = 5, Y = 6))

  stub(subject$update_risk, 'self$calculate_risk',
    function(predicted.outcome, observed.outcome, randomVariables) {
      return(mocked_risks)
    }
  )

  stub(subject$update_risk, 'self$update_single_risk',
    function(old_risk, new_risks, current_count, randomVariables) {
      data.table(A = 1, W = 2, Y=3)
    }
  )

  updated_risk <- subject$update_risk(
    predicted.outcome = glob_predicted.outcome,
    observed.outcome = glob_observed.outcome,
    randomVariables = glob_randomVariables,
    current_count = cur.current_count,
    current_risk = cur.current_risk
  )

  expect_is(updated_risk, 'list')
  expect_named(updated_risk, names(mocked_risks))
  lapply(updated_risk, function(risk) {
    expect_named(risk, randomVariable_names)
    expect_is(risk, 'data.table')
  })
})


context(" update_single_risk")
#=====================================================================
test_that("it should update the risk properly when there already was a risk", {
  subject <- described.class$new()

  cur.current_risk <- data.table(A = 1, W = 2, Y = 3)
  cur.new_risks <- data.table(A = 8, W = 9, Y = 10)
  cur.current_count <- 20

  expected_risk <- (cur.current_risk * cur.current_count + cur.new_risks) / (cur.current_count + 1)

  updated_risk <- subject$update_single_risk(
    old_risk = cur.current_risk,
    new_risks = cur.new_risks,
    current_count = cur.current_count,
    randomVariables = glob_randomVariables
  )

  expect_equal(updated_risk, expected_risk)
})

test_that("it should update (set) the risk properly when there is no current risk", {
  subject <- described.class$new()
  cur.current_risk <- NULL
  cur.new_risks <- data.table(A = 8, W = 9, Y = 10)
  cur.current_count <- 0

  expected_risk <- cur.new_risks

  updated_risk <- subject$update_single_risk(
    old_risk = cur.current_risk,
    new_risks = cur.new_risks,
    current_count = cur.current_count,
    randomVariables = glob_randomVariables
  )

  expect_equivalent(updated_risk, expected_risk)
})

test_that("it should update the risk properly and return the correct named datatable", {
  subject <- described.class$new()

  cur.current_risk <- data.table(A = 1, W = 2, Y = 3)
  cur.new_risks <- data.table(A = 8, W = 9, Y = 10)
  cur.current_count <- 20

  updated_risk <- subject$update_single_risk(
    old_risk = cur.current_risk,
    new_risks = cur.new_risks,
    current_count = cur.current_count,
    randomVariables = glob_randomVariables
  )

  expect_true(is.a(updated_risk, 'data.table'))
  expect_equal(colnames(updated_risk), randomVariable_names)
})
