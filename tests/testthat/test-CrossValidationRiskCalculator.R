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
observed.outcome <- list(c(1,2,3,4), c(-1,-2,-3,-4), c(0.1,0.2,0.3,0.4))
names(observed.outcome) <-  sapply(randomVariables, function(x) x$getY)

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

test_that("it should throw if the observed outcomes are empty", {
  m <- mock(function(...) 42, cycle=TRUE)
  with_mock(self$calculate_risk = m, 
  subject$update_risk(predicted.outcome = predicted.outcome, 
                          observed.outcome = observed.outcome, 
                          randomVariables = randomVariables,
                          current_count = 0, current_risk = 0))

  erroneous_inputs <- list(NULL, list())

  expected_msg <- 'Observed outcome is empty!'
  for (input in erroneous_inputs) {
    expect_error(subject$update_risk(predicted.outcome = predicted.outcome,
                                     observed.outcome = input, 
                                     randomVariables = randomVariables,
                                     current_risk = 0, current_count = 0), expected_msg)
  }
})
