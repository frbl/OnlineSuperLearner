context("Data.Static.R")
described.class <- Data.Static

context(' initialize')
#==========================================================
test_that("it should set the dataset if this is provided", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)

  result <- subject$get_all 
  expect_false(is.null(result))
  expect_equal(result, data)

})

test_that("it should convert the data to a datatable if it was a data frame", {
  data <- data.frame(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(data)
  expect_is(subject$get_all, 'data.table')
})

test_that("it should get the data from an url if provided", {
  url <- system.file("testdata",'test-read_from_url_data.static.csv',package="OnlineSuperLearner")
  subject <- described.class$new(url = url)
  result <- subject$get_all
  expected <- data.table(A = rep(1,6), B = rep(2,6), C = rep(3,6))
  expect_equal(result, expected)
})

test_that("it should initialize the currentrow", {
  data <- data.frame(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(data)
  expect_equal(subject$get_currentrow, 1)
})

test_that("it should throw if no data frame and no url are provided", {
  expect_error(described.class$new(), 'You need to provide at least a datatable or url')
})

test_that("it should throw if the provided verbose is not correct", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  expect_error(described.class$new(data, verbose='incorrect'), "Argument 'verbose' is non-logical: character")
})


context(' getNext')
#=========================================================
test_that("it should get the next entry from the dataset", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  subject <- described.class$new(dataset = data)

  for (idx in 1:nrow(data)) {
    result <- subject$getNext()
    expected <- data[idx,]
    expect_is(result, 'data.table')
    expect_equal(result, expected)
  }
})

test_that("it should return NA if all data was collected before", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  subject <- described.class$new(dataset = data)
  lapply(seq(nrow(data)), function(i) subject$getNext())
  
  result <- subject$getNext()
  expected <- data.table(x = as.numeric(NA), y = as.numeric(NA))

  expect_equal(result, expected)
})

test_that("it should increase its current row", {
  
})

context(' getNextN')
#==========================================================
test_that("it should get the next n  entries from the dataset", {
  data <- data.table(x=c(1,2,3,4,5,6), y=c(6,5,4,3,2,1))
  subject <- described.class$new(dataset = data)
  n <- 2

  ## 1
  result <- subject$getNextN(n=n)
  expected <- data[1:n,] 
  expect_true(is.a(result, 'data.table'))
  expect_equal(nrow(result), n)
  expect_equal(result, expected)

  ## 2
  result <- subject$getNextN(n=n)
  expected <- data[(n+1):(2*n),] 
  expect_true(is.a(result, 'data.table'))
  expect_equal(nrow(result), n)
  expect_equal(result, expected)

  ## 3
  result <- subject$getNextN(n=n)
  expected <- data[(2*n+1):(3*n),] 
  expect_true(is.a(result, 'data.table'))
  expect_equal(nrow(result), n)
  expect_equal(result, expected)
})

test_that("it should return only the available data if more data was requested then available", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  subject <- described.class$new(dataset = data)
  n <- 3
  result <- subject$getNextN(n=n)
  expected <- data[1:n,] 

  expect_true(is.a(result, 'data.table'))
  expect_equal(nrow(result), n)
  expect_equal(ncol(result), ncol(data))
  expect_equal(result, expected)

  expect_error(result <- subject$getNextN(n=100), NA)
  expect_true(is.a(result, 'data.table'))
  expect_equal(dim(result), c(1,ncol(data)))
  expect_equal(result, data.table(x=4,y=1))
})

test_that("it should return null if no data is available", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  subject <- described.class$new(dataset = data)
  result <- subject$getNextN(n=4)
  expect_false(is.null(result))
  expect_equal(result, data)
  expect_error(result <- subject$getNextN(n=100), NA)
  expect_true(is.null(result))
})

context(' reset')
#==========================================================
test_that("it should eeset the pointer to the first observation", {
  data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
  subject <- described.class$new(dataset = data)

  # 1
  result <- subject$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=1, y=4)))
  expect_equal(result, expected)

  expect_equal(subject$get_currentrow, 2)
  subject$reset
  expect_equal(subject$get_currentrow, 1)

  #1 again
  result <- subject$getNext()
  expect_true(is.a(result, 'data.table'))
  expected <- as.data.table(t(c(x=1, y=4)))
  expect_equal(result, expected)
  
})

context(' getAll')
#==========================================================
test_that("it should return the whole dataset", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  result <- subject$get_all 
  expect_equal(result, data)
})

test_that("it should also return the whole dataset, even if the pointer was placed elsewhere", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  subject$getNext()
  result <- subject$get_all 
  expect_equal(result, data)
})

context(" get_length")
#==========================================================
test_that("it should return the total length of the datatable", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  result <- subject$get_length
  expect_equal(result, nrow(data))
})

test_that("it should return the total length of the datatable even if the pointer was placed elsewhere", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  subject$getNextN(3)
  result <- subject$get_length
  expect_equal(result, nrow(data))
})

context(" get_remaining_length")
#==========================================================
test_that("it should return the remaining length of the datatable", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)

  expected <- nrow(data)
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
  for (i in 1:nrow(data)) {
    subject$getNext()
  }

  for (i in 1:nrow(data)) {
    subject$getNext()
    result <- subject$get_remaining_length
    expect_equal(result,0)
  }
})

context(" get_currentrow")
#==========================================================
test_that("it should return the pointer to the current row", {
  data <- data.table(x=c(1,2,3,4), y=c(1,2,3,4))
  subject <- described.class$new(dataset = data)
  result <- subject$get_currentrow
  expect_equal(result, 1)

  subject$getNext()
  result <- subject$get_currentrow
  expect_equal(result, 2)
})

