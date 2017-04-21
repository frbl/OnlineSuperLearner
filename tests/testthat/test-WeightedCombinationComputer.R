context("WeightedCombinationComputer.R")
described.class <- WeightedCombinationComputer
K <- 10
obsWeights <- rep(1/K, K)
libraryNames <- stringi::stri_rand_strings(K, 5)
subject<- described.class$new(obsWeights) 

context(" initialize")
test_that("it should initialize", {
  expect_error(described.class$new(obsWeights), NA) 
})

context(" get_weights")
test_that("it should return the set weights", {
  expect_equal(subject$get_weights, obsWeights )
})

context(" process")
test_that("it should throw an error when not extended", {
  error <- 'This method is not implemented, please inherit this class and implement it.'
  expect_error(subject$process(K, K, libraryNames), error)
})


test_that("it should throw an error when not the correct set of library names is provided", {
  error <- 'libraryNames should be a vector of names.'
  everything_was_crrect_error <- 'This method is not implemented, please inherit this class and implement it.'

  # Librarynames are doubles here
  expect_error(subject$process(K, K, libraryNames = K), error)
  expect_error(subject$process(K, K, libraryNames = NULL), error)
  expect_true(is.a(libraryNames, 'character'))
  expect_error(subject$process(K, K, libraryNames = libraryNames), everything_was_crrect_error)
})
