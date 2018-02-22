library('mockery')
context("WeightedCombinationComputer.R")
described.class <- WeightedCombinationComputer
K <- 10
obsWeights <- rep(1/K, K)
glob.libraryNames <- stringi::stri_rand_strings(K, 5)
subject<- described.class$new(obsWeights) 

context(" initialize")
#==========================================================
test_that("it should not throw errors on initialization", {
  expect_error(described.class$new(obsWeights), NA) 
})

test_that("it should throw if the sum of the weights is not 1", {
  obsWeights <- rep(1/K, K+1)
  cur.libraryNames <- stringi::stri_rand_strings(K+1, 5)

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
  expect_error(subject$process(K, K, glob.libraryNames), error, fixed = TRUE)
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
  expect_true(is.a(glob.libraryNames, 'character'))
  expect_error(subject$process(K, K, libraryNames = glob.libraryNames), everything_was_correct_error)
})

test_that("it should call the subclass", {
  stub(subject$process, 'self$compute',
    function(Z, Y, libraryNames, ...) {
      expect_equal(Z, matrix(1))
      expect_equal(Y, matrix(2))
      expect_equal(libraryNames, glob.libraryNames)
      return(obsWeights)
    }
  )
  subject$process(Z = 1, Y = 2, libraryNames = glob.libraryNames)
})

context(" get_weights")
#==========================================================
test_that("it should return the set weights", {
  expect_equal(subject$get_weights, obsWeights )
})

context(" get_historical_weights")
#==========================================================
test_that("it should return a data.table of alphas", {
  subject <- described.class$new(obsWeights) 
  expect_is(subject$get_historical_weights, 'data.frame' )
  stub(subject$process, 'self$compute', function(...) { return(obsWeights) })

  res <- data.frame(t(obsWeights))
  for(step in seq(20)) {
    subject$process(Z = 1, Y = 2, libraryNames = glob.libraryNames)
    res <- rbind(res, obsWeights)
    expect_equal(nrow(subject$get_historical_weights), step + 1) ## one for the initial step
    expect_equal(subject$get_historical_weights, res)
  }
})

test_that("it should add a row for every iteration", {
  subject <- described.class$new(obsWeights) 
  expect_is(subject$get_historical_weights, 'data.frame' )
  expect_equal(nrow(subject$get_historical_weights), 1)
})

context(" get_step_count")
#==========================================================
test_that("it should initially be 0", {
  subject<- described.class$new(obsWeights) 
  expect_equal(subject$get_step_count, 0 )
})

test_that("it should return the stepcount", {
  subject<- described.class$new(obsWeights) 
  stub(subject$process, 'self$compute', function(...) { return(obsWeights) })

  for(step in seq(20)) {
    subject$process(Z = 1, Y = 2, libraryNames = glob.libraryNames)
    expect_equal(subject$get_step_count, step)
  }
})
