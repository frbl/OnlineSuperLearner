library(mockery)

context("CrossValidationRiskCalculator.R")
described.class <- CrossValidationRiskCalculator
subject <- described.class$new()

# Initialize some variables
randomVariables <- list(list(getY='A', getFamily='binomial'),
                        list(getY='W', getFamily='gaussian'),
                        list(getY='Y', getFamily='gaussian')
                        )
randomVariable_names <- lapply(randomVariables, function(rv) rv$getY) %>% unlist

# Create predicted outcomes
predicted.outcome <- list()
predicted.outcome <- append(predicted.outcome, list(list( c(9,8,7,6), c(5,4,3,2), c(4,3,2,1))))
names(predicted.outcome[[length(predicted.outcome)]]) <- sapply(randomVariables, function(x) x$getY)
predicted.outcome <- append(predicted.outcome, list(list( c(5,5,5,5), c(4,4,4,4), c(3,3,3,3))))
names(predicted.outcome[[length(predicted.outcome)]]) <- sapply(randomVariables, function(x) x$getY)
names(predicted.outcome) <- c('a','b')

# Create observed outcomes
observed.outcome <- data.table(c(1,2,3,4), c(-1,-2,-3,-4), c(0.1,0.2,0.3,0.4))
colnames(observed.outcome) <-  randomVariable_names

context(" calculate_evaluation")


context(" calculate_risk")
test_that("it should throw if the predicted outcome is not a list", {
  predicted.outcome <- 'not a list'
  msg <- "Argument 'predicted.outcome' is neither of nor inherits class list: character"
  expect_error(subject$calculate_risk(predicted.outcome = predicted.outcome, 
                         observed.outcome = observed.outcome, 
                         randomVariables = randomVariables), msg)
})

test_that("it should call the correct lossfunction for each of the random variables", {
  m <- mock(function(...) 42, cycle=TRUE)
  with_mock(Evaluation.get_evaluation_function = m, 
  subject$calculate_risk(predicted.outcome = predicted.outcome, 
                          observed.outcome = observed.outcome, 
                          randomVariables = randomVariables))
  
  expect_called(m,length(randomVariables) * length(predicted.outcome))
  expect_args(m, 1, randomVariables[[1]]$getFamily, useAsLoss = TRUE )

  args <- mock_args(m)
  result <- lapply(1:length(args), function(i) {
    j <- ((i -1) %% length(randomVariables)) + 1
    
    args[[i]][[1]] ==  randomVariables[[j]]$getFamily
    args[[i]][[2]] ==  TRUE
  }) %>% unlist %>% all %>% expect_true
    
})

test_that("it should call the loss function with the correct data", {
  loss_mock <- mock(42, cycle=TRUE)

  with_mock(Evaluation.get_evaluation_function = function(...) loss_mock, 
    subject$calculate_risk(predicted.outcome = predicted.outcome, 
                           observed.outcome = observed.outcome, 
                           randomVariables = randomVariables))
  
  expect_called(loss_mock,length(randomVariables) * length(predicted.outcome))
  args <- mock_args(loss_mock)

  # Check if th observed data is passed in correctly
  lapply(1:length(args), function(i) {
    j <- ((i - 1) %% length(randomVariables)) + 1
    observed.outcome[[j]] == args[[i]][['data.observed']]
  }) %>% unlist %>% all %>% expect_true

  k <- 0
  # Check if th predicted data is passed in correctly
  for(i in 1:length(args)){
    j <- ((i - 1) %% length(randomVariables)) + 1
    if(i %% (length(randomVariables)) == 1 ) k <- k + 1
    predicted.outcome[[k]][[j]] == args[[i]][['data.predicted']]
  } %>% unlist %>% all %>% expect_true
})

test_that("it should set the correct names to the correct risks", {
  evaluation_mock <- mock(function(...) 42, cycle=TRUE)
  with_mock(Evaluation.get_evaluation_function = evaluation_mock, 
  result <-subject$calculate_risk(predicted.outcome = predicted.outcome, 
                          observed.outcome = observed.outcome, 
                          randomVariables = randomVariables))
  expect_equal(names(result), names(predicted.outcome))

  lapply(result, function(x) names(x) == randomVariable_names) %>% 
    unlist %>% 
    all %>% 
    expect_true

})

context(" update_risk")
test_that("it should throw if the predicted outcomes are empty", {
  erroneous_inputs <- list(NULL, list())
  expected_msg <- 'Predicted outcome is empty!'
  for (input in erroneous_inputs) {
    expect_error(subject$update_risk(predicted.outcome = NULL,
                                     observed.outcome = observed.outcome, 
                                     randomVariables = randomVariables,
                                     current_risk = 0, current_count = 0), expected_msg)
  }
})

test_that("it should throw if the observed outcomes are empty", {
  erroneous_inputs <- list(NULL, list())
  expected_msg <- 'Observed outcome is empty!'
  for (input in erroneous_inputs) {
    expect_error(subject$update_risk(predicted.outcome = predicted.outcome,
                                     observed.outcome = input, 
                                     randomVariables = randomVariables,
                                     current_risk = 0, current_count = 0), expected_msg)
  }
})

test_that("it should update (set) the risk properly when there is no risk", {
  subject <- described.class$new()

  # TODO: Very weird why this doesn't work. Look into it.
  # https://github.com/n-s-f/mockery/blob/380d81e1439969908a767e484939450b17b1b9ad/tests/testthat/test_stub.R#L197-L230
  #stub(subject$update_risk, 'self$calculate_risk', list(a = c(1,2,3), b = c(3,2,1)))
  # print(lapply(randomVariables, function(abc) {abc$getY}))

  expected_risk <- list(A = list(A = 1, W = 2, Y=3), B = list(A = 4, W = 5, Y = 6))
  lossFn_mock <- mock(1,2,3,4,5,6)

  evaluation_mock <- mock(lossFn_mock, cycle = TRUE)

  a <- list()
  with_mock(Evaluation.get_evaluation_function = evaluation_mock, 
    updated_risk <- subject$update_risk(predicted.outcome = predicted.outcome, 
                          observed.outcome = observed.outcome, 
                          randomVariables = randomVariables,
                          current_count = 0, current_risk = list())

  )

  expect_true(is.a(updated_risk, 'list'))
  expect_equal(names(updated_risk), names(predicted.outcome))
  expect_equivalent(updated_risk, expected_risk)
})

test_that("it should update the risk properly when there already was a risk", {
  subject <- described.class$new()

  # TODO: Very weird why this doesn't work. Look into it.
  # https://github.com/n-s-f/mockery/blob/380d81e1439969908a767e484939450b17b1b9ad/tests/testthat/test_stub.R#L197-L230
  #stub(subject$update_risk, 'self$calculate_risk', list(a = c(1,2,3), b = c(3,2,1)))
  # print(lapply(randomVariables, function(abc) {abc$getY}))

  current_risk <- list(a = list(A = 1, W = 2, Y=3), b = list(A = 4, W = 5, Y = 6))
  lossFn_mock <- mock(1,1,1,1,1,1)

  expected_risk <- current_risk
  for(algo in names(current_risk)) {
    for (rsk in names(current_risk[[algo]])) {
      expected_risk[[algo]][[rsk]] <- (expected_risk[[algo]][[rsk]] * 20) + 1
      expected_risk[[algo]][[rsk]] <- expected_risk[[algo]][[rsk]] / 21 
    }
  }



  evaluation_mock <- mock(lossFn_mock, cycle = TRUE)


  with_mock(Evaluation.get_evaluation_function = evaluation_mock, 
    updated_risk <- subject$update_risk(predicted.outcome = predicted.outcome, 
                          observed.outcome = observed.outcome, 
                          randomVariables = randomVariables,
                          current_count = 20, current_risk = current_risk)

  )

  expect_true(is.a(updated_risk, 'list'))
  expect_equal(names(updated_risk), names(predicted.outcome))
  expect_equal(updated_risk, expected_risk)
})
