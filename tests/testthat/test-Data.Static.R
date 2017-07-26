context("Data.Static.R")
described.class <- Data.Static

context(' initialize')
test_that("it should set the dataset if this is provided", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  ds <- described.class$new(dataset = data)
  result <- ds$get_all 
  expect_false(is.null(result))
  expect_equal(result, data)
})

test_that("it should get the data from an url if provided", {
  url <- system.file("testdata",'test-read_from_url_data.static.csv',package="OnlineSuperLearner")
  subject <- described.class$new(url = url)
  result <- subject$get_all
  expected <- data.table(A = rep(1,6), B = rep(2,6), C = rep(3,6))
  expect_equal(result, expected)
})

test_that("it should throw if no data frame and no url are provided", {
  expect_error(described.class$new(), 'You need to provide at least a datatable or url')
})

context(' getAll')
test_that("it should return the whole dataset", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  ds <- described.class$new(dataset = data)
  result <- ds$get_all 
  expect_equal(result, data)
})

test_that("it should also return the whole dataset, even if the pointer was placed elsewhere", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  ds <- described.class$new(dataset = data)
  ds$getNext()
  result <- ds$get_all 
  expect_equal(result, data)
})

context(' reset')
test_that("it should eeset the pointer to the first observation", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)

  # 1
  result <- ds$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=1, y=4)))
  expect_equal(result, expected)

  ds$reset

  #1 again
  result <- ds$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=1, y=4)))
  expect_equal(result, expected)
  
})

context(" get_length")
test_that("it should return the total length of the datatable", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  result <- subject$get_length
  expect_equal(result, nrow(data))
})

context(" get_remaining_length")
test_that("it should return the remaining length of the datatable", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  expected <- subject$get_length
  result <- subject$get_remaining_length
  expect_equal(result, expected)
  for (i in 1:expected) {
    subject$getNext()
    result <- subject$get_remaining_length
    expect_equal(result, expected - i)
  }
})

test_that("it should reset to the original when resetting", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  for (i in 1:100) {
    subject$getNext()
  }
  subject$reset
  result <- subject$get_remaining_length
  expect_equal(result,nrow(data))
})

test_that("it should not go below zero", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  for (i in 1:100) {
    subject$getNext()
  }
  result <- subject$get_remaining_length
  expect_equal(result,0)
})

context(" get_currentrow")
test_that("it should return the pointer to the current row", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  result <- subject$get_currentrow
  expect_equal(result,1)
  subject$getNext()
  result <- subject$get_currentrow
  expect_equal(result,2)
})

context(' getNext')
test_that("it should get the next entry from the dataset", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)

  # 1
  result <- ds$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=1, y=4)))
  expect_equal(result, expected)

  # 2
  result <- ds$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=2, y=3)))
  expect_equal(result, expected)
})

test_that("it should return NA if all data was collected", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)
  lapply(seq(nrow(data)), function(i) ds$getNext())
  
  result <- ds$getNext()
  expected <- as.data.table(t(c(x=1, y=1)))
  expected[c(x,y),] <- NA
  expect_equal(result, expected)
})

context(' getNextN')
test_that("it should get the next n  entries from the dataset", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)
  n <- 2
  # 1
  result <- ds$getNextN(n=n)
  expected <- data.table(x=c(1,2), y=c(4,3))
  expect_true(is.a(result, 'data.table'))
  expect_equal(nrow(result), n)
  expect_equal(result, expected)

  # 2
  result <- ds$getNextN(n=n)
  expected <- data.table(x=c(3,4), y=c(2,1))
  expect_true(is.a(result, 'data.table'))
  expect_equal(nrow(result), n)
  expect_equal(result, expected)
})

test_that("it should return only the available data if more data was requested then available", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)
  result <- ds$getNextN(n=3)
  expect_false(is.null(result))
  expect_error(result <- ds$getNextN(n=100), NA)
  expect_equal(dim(result), c(1,2))
  expect_equal(result, data.table(x=4,y=1))
})

test_that("it should return null if no data is available", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)
  result <- ds$getNextN(n=4)
  expect_false(is.null(result))
  expect_equal(result, data)
  expect_error(result <- ds$getNextN(n=100), NA)
  expect_true(is.null(result))
})

