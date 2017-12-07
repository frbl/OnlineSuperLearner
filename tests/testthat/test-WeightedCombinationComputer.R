context("WeightedCombinationComputer.R")
described.class <- WeightedCombinationComputer
K <- 10
obsWeights <- rep(1/K, K)
libraryNames <- stringi::stri_rand_strings(K, 5)
subject<- described.class$new(obsWeights) 

context(" initialize")
#==========================================================
test_that("it should not throw errors on initialization", {
  expect_error(described.class$new(obsWeights), NA) 
})

test_that("it should throw if the sum of the weights is not 1", {
  obsWeights <- rep(1/K, K+1)
  libraryNames <- stringi::stri_rand_strings(K+1, 5)

  expect_error(described.class$new(obsWeights), 'The sum of the initial weights, 1.1, does not equal 1', fixed = TRUE)
})

context(" compute")
#==========================================================
test_that("it should throw as this function is not implemented in this class", {
  expect_error(subject$compute(Z = 1, Y = 2, libraryNames = '123'),
               'This method is not implemented, please inherit this class and implement it.',
               fixed = TRUE )
})

context(" process")
#==========================================================
test_that("it should throw an error when not extended", {
  error <- 'This method is not implemented, please inherit this class and implement it.'
  expect_error(subject$process(K, K, libraryNames), error, fixed = TRUE)
})

test_that("it should always return 1 if there is only one weight", {
  my.K <- 1
  cur.obsWeights <- my.K
  cur.libraryNames <- stringi::stri_rand_strings(my.K, 5)
  cur.subject <- described.class$new(cur.obsWeights) 
  expect_equal(cur.subject$process(my.K,my.K,cur.libraryNames), 1)
})

test_that("it should throw an error when not the correct set of library names is provided", {
  error <- 'libraryNames should be a vector of names.'
  everything_was_correct_error <- 'This method is not implemented, please inherit this class and implement it.'

  # Librarynames are doubles here
  expect_error(subject$process(K, K, libraryNames = K), error)
  expect_error(subject$process(K, K, libraryNames = NULL), error)
  expect_true(is.a(libraryNames, 'character'))
  expect_error(subject$process(K, K, libraryNames = libraryNames), everything_was_correct_error)
})

context(" get_weights")
#==========================================================
test_that("it should return the set weights", {
  expect_equal(subject$get_weights, obsWeights )
})

