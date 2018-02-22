context("WCC.CG")
described.class <- WCC.CG
#================================================
library_names <- c('a','b','c','d','e')
number_of_algorithms <- length(library_names)
nobs <- 50

Y <- rnorm(nobs)
Z <- matrix(rnorm(number_of_algorithms * nobs), nobs, number_of_algorithms)

context(' initialize')
#================================================
test_that("it should initialize the weights according to the number of algorithms", {
  subject <- described.class$new(number_of_algorithms = number_of_algorithms)
  expect_equal(subject$get_weights, rep(1/number_of_algorithms, number_of_algorithms))
})

test_that("it should initialize the weights according to the provided weights", {
  initial_weights <- c(rep(0, number_of_algorithms -1), 1)
  subject <- described.class$new(weights.initial = initial_weights)
  expect_equal(subject$get_weights, initial_weights)
})

test_that("it should throw if both the initial weights and number of algorithms are null", {
  expect_error(described.class$new(), 
               'Please provide initial weights (or number_of_algorithms with the number of alphas to estimate)', fixed=TRUE)
})

context(' compute')
#================================================
test_that("it should sum all weights to 1", {
  subject <- described.class$new(number_of_algorithms = number_of_algorithms)
  result <- subject$process(Z, Y, library_names)
  result %<>% sum
  expect_equal(result, 1)
})

test_that("it should only return positive weights", {
  subject <- described.class$new(number_of_algorithms = number_of_algorithms)
  result <- subject$process(Z, Y, library_names)
  expect_true(all(as.vector(result) >= 0))
})

test_that("it should provide sensible results", {
  set.seed(12345)
  n <- 1e3
  Z <- rnorm(n)
  pred <- cbind(Z, Z^2, Z^3, Z^4)
  true_params <- c(.2, .8, 0, 0)

  Y <- pred %*% true_params +rnorm(n, 0, 0.1)
  libraryNames <- c('a', 'b', 'c', 'd')

  subject <- described.class$new(number_of_algorithms = length(libraryNames))

  ## We have to do this multiple times as we'd like eta to go down
  for(i in 1:500) subject$process(Z = pred, Y = Y, libraryNames = libraryNames)

  ## Check that we approximate the true parameters
  difference <- subject$get_weights - true_params
  print(subject$get_weights)

  for (diff in difference) {
    expect_lt(abs(diff), 1e-2)
  }
})

