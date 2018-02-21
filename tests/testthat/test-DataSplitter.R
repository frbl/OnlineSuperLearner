context("DataSplitter.R")
described.class <- DataSplitter

context(' initialize')
#==========================================================
test_that("it should initialize", {
  expect_error(described.class$new(), NA)
})

test_that("it should initialize with a test_set_size", {
  test_set_size <- 10
  expect_error(result <- described.class$new(test_set_size = test_set_size), NA)
  expect_equal(result$get_test_set_size, test_set_size)
})

context(' split')
#==========================================================
data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
testset_size <- 1

test_that("it should return a test and training set", {
  subject <- described.class$new()
  result <- subject$split(data)
  expect_false(is.null(result))
  expect_equal(names(result), c('train', 'test'))
})

test_that("it should have the correct entries in the testset", {
  subject <- described.class$new()
  result <- subject$split(data)
  expect_equal(nrow(result$test), testset_size)
  expect_equal(result$test, as.data.table(t(c(x=4, y=1))))
})

test_that("it should have the correct entries in the trainingset", {
  subject <- described.class$new()
  result <- subject$split(data)
  expect_equal(nrow(result$train), nrow(data) - testset_size)
  expected <- data.table(x=c(1,2,3), y=c(4,3,2))
  expect_equal(result$train, expected)
})

test_that("it should append the previous test set to the trainingset", {
  subject <- described.class$new()
  result <- subject$split(data)
  data <- data.table(x=c(700,800), y=c(1000,900))
  result_new <- subject$split(data)

  expect_equal(nrow(result_new$train), nrow(data))
  expect_equal(nrow(result_new$test), testset_size)

  ## The train set should contain the previous test set
  expect_equal(result_new$train[1,], result$test[1,])

  ## The train set should contain the first part of the new data
  expect_equal(result_new$train[2,], data[1,])

  ## The test set should contain the second part of the new data
  expect_equal(result_new$test[1,], data[2,])
})

test_that("it should throw whenever not enough data is provided", {
  data <- data.table(x=c(1), y=c(4))
  subject <- described.class$new()
  expect_error(subject$split(data), 'At least 2 rows of data are needed, 1 train and 1 test')

  ## No error if enough data is in fact available
  data <- data.table(x=c(1,2), y=c(4,3))
  expect_error(subject$split(data), NA)
})

test_that("it should throw whenever not enough data is provided when the default test size is increased", {
  data <- data.table(x=c(1,2), y=c(4,3))
  subject <- described.class$new(test_set_size = 2)
  expect_error(subject$split(data), 'At least 3 rows of data are needed, 1 train and 2 test')

  ## No error if enough data is in fact available
  data <- data.table(x=c(1,2,3), y=c(4,3,2))
  expect_error(subject$split(data), NA)
})

test_that("it should not throw if not enough data is provided, but there is some in the cache", {
  subject <- described.class$new()
  data_cache <- data.table(x=c(1,2), y=c(4,3))
  subject$split(data_cache)

  data <- data.table(x=c(1), y=c(4))
  expect_error(subject$split(data), NA)
})

test_that("it should return the tau last rows if tau is set different than 1", {
  test_set_size <- 10
  subject <- described.class$new(test_set_size = test_set_size)

  data_cache <- data.table(x=seq(20), y=-seq(20))
  result <- subject$split(data_cache)
  expected_train <- data.table(x=seq(10), y=-seq(10))
  expected_test <- data.table(x=seq(11, 20), y=-seq(11, 20))
  expect_equal(result$train, expected_train)
  expect_equal(result$test, expected_test)
})

test_that("it should append the tau last rows for tau set different than 1", {
  test_set_size <- 10
  subject <- described.class$new(test_set_size = test_set_size)

  data_cache <- data.table(x = seq(20), y = -seq(20))
  result <- subject$split(data_cache)
  result <- subject$split(data_cache)

  ## Note: the training set is now the previous testset
  previous_test <- data.table(x=seq(11, 20), y = -seq(11, 20))
  expected_train <- data.table(x = c(previous_test[,x], seq(10)), 
                               y = c(previous_test[,y], -seq(10)))

  expected_test <- data.table(x = seq(11, 20), y = -seq(11, 20))

  expect_equal(result$train, expected_train)
  expect_equal(result$test, expected_test)
})
