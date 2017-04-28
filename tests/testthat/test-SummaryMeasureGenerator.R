context("SummaryMeasureGenerator.R")
described.class <- SummaryMeasureGenerator

context(" initialize")
test_that("it should only require SMG.list as a required parameter", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_true(is.a(subject, 'SummaryMeasureGenerator'))
  expect_true(is.a(subject$getCache, "data.table"))
})

test_that("it should set the minimal measurements needed correctly", {
  mylist <- c()
  max <- 10
  for (i in 1:max) {
    mylist <- append(mylist, SMG.Mock$new(minimalObservations = i))
  }
  subject <- described.class$new(SMG.list = mylist)
  expect_equal(subject$minimal.measurements.needed, (max - 1))
})

dataset <- data.table(Y= (seq(1,10)%%2) , A=rep(1,10), W=seq(10,1))

context(" reset")
test_that("it should reset the cache", {
  # Not testable without mocking, or using the fillCache as a proxy for checking
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))

  subject <- described.class$new(SMG.list = mylist, data = data)
  expect_true(subject$fillCache())
  expect_false(subject$fillCache())

  # Resetting should clear the cache
  subject$reset()
  expect_true(subject$fillCache())
})

context(" setData")
test_that("it should reset the summarizer", {
  # Not testable without mocking, or using the fillCache as a proxy for checking
})

test_that("it should should set the correct data", {
  data <- Data.Static$new(dataset = dataset)

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_error(subject$getNext(),'Please set the data of the summary measure generator first')
  subject$setData(data)
  expect_error(subject$getNext(),NA)
})

context(" fillCache")
test_that("it should throw if there is no data set", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_error(subject$fillCache(), 'Please set the data of the summary measure generator first')
})

test_that("it should fill the cache with the correct number of measurments", {
  data <- Data.Static$new(dataset = dataset)

  needed <- 3
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data)
  expect_true(nrow(subject$getCache) == 0)  

  subject$fillCache()
  expect_true(nrow(subject$getCache) == needed -1)  
})

context(" getNext")
test_that("it should return the measurements requested without params", {
  data <- Data.Static$new(dataset = dataset)

  needed <- 1
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data)
  for (i in 1:6) {
    result <- subject$getNext()
    cache  <- subject$getCache
    expected  <- dataset[i,]

    expect_true(is.a(result, 'data.frame'))

    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), ncol(dataset))
    expect_equal(result, expected)
    expect_equal(cache, expected)
  }
})

test_that("it should return the measurements requested when specifying N", {
  data <- Data.Static$new(dataset = dataset)
  needed <- 1
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data)
  for (n in 1:3) {
    data$reset()
    subject$reset()
    for (i in seq(1,4,n)) {
      result <- subject$getNext(n=n)
      cache  <- subject$getCache
      expected  <- dataset[i:(i+n-1),]

      expect_true(is.a(result, 'data.frame'))
      expect_equal(nrow(result), n)
      expect_equal(ncol(result), ncol(dataset) * length(mylist))
      expect_equal(result, expected)

      # The cache is equal, because we dont have a cache, we need just 1 observations
      expect_equal(cache, expected)
    }
  }
})

test_that("it should combine the results of multiple summarizers" , {
  data <- Data.Static$new(dataset = dataset)

  memoriesFirst <- 1
  memoriesSecond <- 2
  n <- 2
  mylist <- c(SMG.Mock$new(memoriesFirst), SMG.Mock$new(memoriesSecond))
  subject <- described.class$new(SMG.list = mylist, data = data)
  result <- subject$getNext(n=n)

  # Note that we throw away the first measurement, this is neccessary for us to comply to the history of
  # each function
  expected  <- dataset[memoriesSecond:(memoriesSecond+n-1),]
  expected <- cbind(expected, expected)

  expect_true(is.a(result, 'data.frame'))
  expect_equal(nrow(result), n)
  expect_equal(ncol(result), ncol(dataset) * length(mylist))
  expect_equal(result, expected)

  result <- subject$getNext(n=n)

  # Note that we don't throw away any data in this turn, as we can use the previously acquired history
  expected  <- dataset[(n+memoriesSecond):(n + (memoriesSecond+n-1)),]
  expected <- cbind(expected, expected)

  expect_true(is.a(result, 'data.frame'))
  expect_equal(nrow(result), n)
  expect_equal(ncol(result), ncol(dataset) * length(mylist))
  expect_equal(result, expected)
})

context(" checkEnoughDataAvailable")
test_that("it should check if enough data is available for all formulae, and return true if all is available", {
  f1 <- RandomVariable$new(formula = y ~ w + w2 + w3 + a, family='gaussian')
  f2 <- RandomVariable$new(formula = w ~ a + a1 + a2, family='gaussian')
  f3 <- RandomVariable$new(formula = a ~ x + y + z, family='gaussian')

  variables = c('y','a','w', 'x', 'z')
  exposed_variables1 <- c('1','2','3','4')
  exposed_variables2 <- c('')
  mySmgs <- c(SMG.Mock$new(variables=variables, exposedVariables=exposed_variables1),
              SMG.Mock$new(variables=variables, exposedVariables=exposed_variables2))
  subject <- described.class$new(SMG.list = mySmgs)
  expect_true(subject$checkEnoughDataAvailable(c(f1,f2,f3)))
})

test_that("it should check if enough data is available for all formulae", {
  f1 <- RandomVariable$new(formula = y ~ w + w2 + w3 + a, family='gaussian')
  f2 <- RandomVariable$new(formula = w ~ a + a1 + a2, family='gaussian')
  f3 <- RandomVariable$new(formula = a ~ x + y + z, family='gaussian')

  variables = c('y', 'a', 'w')
  exposed_variables1 <- c('1','2','3','4')
  exposed_variables2 <- c('')
  mySmgs <- c(SMG.Mock$new(variables=variables, exposedVariables=exposed_variables1),
              SMG.Mock$new(variables=variables, exposedVariables=exposed_variables2))
  subject <- described.class$new(SMG.list = mySmgs)
  expected_msg <-'Not all provided variables (x, z) are included in the SMGs, include the correct SMGs'
  expect_error(subject$checkEnoughDataAvailable(c(f1,f2,f3)), expected_msg, fixed = TRUE)
  expect_true(subject$checkEnoughDataAvailable(c(f1,f2)))
})


context(" getNextN")
test_that("it should be removed, this function is deprecated", {
 skip("Deprecated function, remove") 
})
