context("H2O.Interactor.R")
described.class <- H2O.Interactor

context(" generate_hash")
test_that("it should generate a hash fast", {
  subject <- described.class$new()
  n = 1e5 
  tic <- Sys.time()
  data <- data.table(a = rnorm(n), b=rnorm(n), c=rnorm(n))
  subject$generate_hash(data)
  toc <- Sys.time()
  result <- toc - tic
  if(result >= 0.2) {
    fail(paste(result, 'is to high!'))
  }
  expect_true(result < 0.2)
})

test_that("it should generate a hash correctly", {
  set.seed(1234)         
  subject <- described.class$new()
  expected <- '3d0228862865afb8d3ed4c98f83a10b6ff2dac0f'
  n = 10 
  data <- data.table(a = rnorm(n), b=rnorm(n), c=rnorm(n))
  result <-subject$generate_hash(data)
  expect_equal(result, expected)

  # It should not be the same with different data
  data <- data.table(a = rnorm(n), b=rnorm(n), c=rnorm(n))
  result <- subject$generate_hash(data)
  expect_false(result == expected)
})

