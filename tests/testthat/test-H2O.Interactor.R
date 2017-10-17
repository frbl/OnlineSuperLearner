library(mockery)
library(h2o)

context("H2O.Interactor.R")
described.class <- H2O.Interactor

context(" generate_hash")
test_that("it should generate a hash fast", {
  subject <- described.class$new()
  n <- 1e5 
  threshold <- 0.2
  tic <- Sys.time()
  data <- data.table(a = rnorm(n), b=rnorm(n), c=rnorm(n))
  subject$generate_hash(data)
  toc <- Sys.time()
  result <- as.numeric(difftime(tic, toc, units = c("secs")))
  if(result >= threshold) {
    fail(paste(result, 'is to high!'))
  }
  expect_lt(result, threshold)
})

test_that("it should generate a hash correctly", {
  set.seed(1234)         
  subject <- described.class$new()
  expected <- 'a_3d0228862865afb8d3ed4c98f83a10b6ff2dac0f'
  n = 10 
  data <- data.table(a = rnorm(n), b=rnorm(n), c=rnorm(n))
  result <-subject$generate_hash(data)
  expect_equal(result, expected)

  # It should not be the same with different data
  data <- data.table(a = rnorm(n), b=rnorm(n), c=rnorm(n))
  result <- subject$generate_hash(data)
  expect_false(result == expected)
})

context(" get_data_pointer")
test_that("it should should upload to h2o if the hash is not on there", {
  subject <- described.class$new()
  data = data.table(a= c(1,2,3), b=c(4,3,2))

  hash <- 'a_abcd'
  stub(subject$get_data_pointer, 'self$generate_hash', hash)
  m <- function(...) 42
  result = with_mock(`h2o::as.h2o` = m, 
  subject$get_data_pointer(data = data))
  expect_equal(result, 42)
})

test_that("it should return the existing h2o hash if it is already on h2o", {
  subject <- described.class$new()
  data = data.table(a= c(1,2,3), b=c(4,3,2))
  hash <- 'a_abcd'
  subject$add_and_return_data_hash(data = data, hash = hash)

  stub(subject$get_data_pointer, 'self$generate_hash', hash)

  result = with_mock(`h2o::as.h2o` = function(...) 42, subject$get_data_pointer(data = data))
  expect_equal(as.data.table(result), data)
})

context(" add_and_return_data_hash")

