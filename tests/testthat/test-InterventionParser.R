context("InterventionParser.R")
context(" InterventionParser.parse_intervention")
test_that("it should parse the intervention and return the correct elements in a list", {
  intervention <- list()
  intervention$variable <- c('A','B','C','A')
  intervention$when <- rep(2, length(intervention$variable))
  intervention$what <- c(1,0,0,1)
  t = 0
  current_outcome = 'A'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_true(is(result, 'list'))
  expect_equal(length(result), 3) 
  expect_named(result, c('when', 'what', 'should_intervene')) 
})

test_that("it should return true if the current time and variable is an intervention time/variable", {
  intervention <- list()
  intervention$variable <- c('A','B','C','A')
  intervention$when <- rep(2, length(intervention$variable))
  intervention$what <- c(1,0,0,1)
  t = 2
  current_outcome = 'A'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_true(result$should_intervene)
})

test_that("it should return false if the current time or variable is not an intervention time/variable", {
  intervention <- list()
  intervention$variable <- c('A','B','C','A')
  intervention$when <- rep(2, length(intervention$variable))
  intervention$what <- c(1,0,0,1)
  t = 2
  current_outcome = 'D'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_false(result$should_intervene)
  expect_equal(result$when, -1)
  expect_equal(result$what, -1)

  t = 1
  current_outcome = 'A'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_false(result$should_intervene)
  expect_equal(result$when, -1)
  expect_equal(result$what, -1)
})

test_that("it should return the correct what and when", {
  intervention <- list()
  intervention$variable <- c('A','B','C','A')
  intervention$when <- c(2,2,2,3)
  intervention$what <- c(1,0,0,0)
  t = 2
  current_outcome = 'A'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_equal(result$when, 2)
  expect_equal(result$what, 1)

  current_outcome = 'B'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_equal(result$when, 2)
  expect_equal(result$what, 0)

  t = 3
  current_outcome = 'A'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_equal(result$when, 3)
  expect_equal(result$what, 0)
})

test_that("it should work with the old style interventions", {
  intervention <- list(variable = 'A',when = c(2), what = c(1))
  t = 1
  current_outcome = 'A'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_false(result$should_intervene)
  expect_equal(result$when, -1)
  expect_equal(result$what, -1)

  t = 2
  current_outcome = 'A'
  result <- InterventionParser.parse_intervention(intervention, 
                                                  current_time= t,
                                                  current_outcome = current_outcome)
  expect_true(result$should_intervene)
  expect_equal(result$when, 2)
  expect_equal(result$what, 1)
})


  
