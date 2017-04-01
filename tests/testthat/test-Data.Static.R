context("Data.Static.R")
described.class <- Data.Static

context(' initialize')
test_that("it should set the dataset if this is provided", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  ds <- described.class$new(dataset = data)
  result <- ds$getAll() 
  expect_false(is.null(result))
  expect_equal(result, data)
})

test_that("it should get the data from an url if provided", {
  
})

context(' getAll')
test_that("it should return the whole dataset", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  ds <- described.class$new(dataset = data)
  result <- ds$getAll() 
  expect_equal(result, data)
})

test_that("it should also return the whole dataset, even if the pointer was placed elsewhere", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  ds <- described.class$new(dataset = data)
  ds$getNext()
  result <- ds$getAll() 
  expect_equal(result, data)
})

context(' reset')
test_that("it should reset the pointer to the first observation", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)

  # 1
  result <- ds$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=1, y=4)))
  expect_equal(result, expected)

  ds$reset()

  #1 again
  result <- ds$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=1, y=4)))
  expect_equal(result, expected)
  
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

test_that("it should throw if more data is requested and all data was collected", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  ds <- described.class$new(dataset = data)
  lapply(seq(nrow(data)), function(i) ds$getNext())
  expect_error(ds$getNextN(n=100))
})
