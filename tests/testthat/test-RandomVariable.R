context("RandomVariable.R")
log <- Arguments$getVerbose(-8, timestamp=TRUE)
log <- FALSE
context(" find_ordering")
test_that("it should return the correct ordered list", {
  W <- RandomVariable$new(formula = (W ~ W_lag_1), family = 'gaussian')
  A <- RandomVariable$new(formula = (A ~ W), family = 'binomial')
  Y <- RandomVariable$new(formula = (Y ~ A + W), family = 'gaussian')
  randomVariables <- c(Y,A,W)
 
  result <- RandomVariable.find_ordering(randomVariables, log)
  expect_false(is.null(result))
  expect_true(length(result) == length(randomVariables))
  expect_equal(result[[1]], W) 
  expect_equal(result[[2]], A) 
  expect_equal(result[[3]], Y) 
})

test_that("it should set the correct names on the array", {
  #log <- FALSE
  W <- RandomVariable$new(formula = (W ~ W_lag_1), family = 'gaussian')
  A <- RandomVariable$new(formula = (A ~ W), family = 'binomial')
  Y <- RandomVariable$new(formula = (Y ~ A + W), family = 'gaussian')
  randomVariables <- c(Y,A,W)
 
  result <- RandomVariable.find_ordering(randomVariables, log)
  expect_false(is.null(result))
  expect_true(length(result) == length(randomVariables))
  expect_equal(names(result), c('W','A','Y'))
})

test_that("it should throw if no ordering is avaiable", {
  W <- RandomVariable$new(formula = (W ~ A), family = 'gaussian')
  A <- RandomVariable$new(formula = (A ~ Y), family = 'binomial')
  Y <- RandomVariable$new(formula = (Y ~ W), family = 'gaussian')
  randomVariables <- c(Y,A,W)
  expect_error(RandomVariable.find_ordering(randomVariables),
               'Intractable problem! There are interdependencies that cannot be solved!')
})

test_that("it should also work if there are no inter dependencies", {
  W <- RandomVariable$new(formula = (W ~ W_lag_1), family = 'gaussian')
  randomVariables <- c(W)
 
  result <- RandomVariable.find_ordering(randomVariables)
  expect_false(is.null(result))
  expect_true(length(result) == length(randomVariables))
  expect_equal(result[[1]], W) 
})
