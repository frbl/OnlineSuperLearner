context("SummaryMeasureGenerator.R")
described.class <- SummaryMeasureGenerator

context(" initialize")
#==========================================================
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
  expect_equal(subject$get_minimal_measurements_needed, (max - 1))
})

test_that("it should throw when pre_processor is set but are not a pre_processor", {
  mylist <- c(SMG.Mock$new())
  expected_message =  "Argument 'pre_processor' is neither of nor inherits class PreProcessor"
  expect_error(described.class$new(SMG.list = mylist, pre_processor='notalist'), expected_message)
})

test_that("it should set normalized to false if no pre_processor is provided", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_false(subject$is_normalized)
})

test_that("it should set normalized to true and the pre_processor whenever a pre_processor is provided", {
  pre_processor_mock <- list(denormalize = function(data) {return(rep(expected, length(data)))})
  class(pre_processor_mock) <- c(class(pre_processor_mock), 'PreProcessor')

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist, pre_processor = pre_processor_mock)
  expect_true(subject$is_normalized)
  expect_equal(subject$get_pre_processor, pre_processor_mock)
})

dataset <- data.table(Y= (seq(1,10)%%2) , A=rep(1,10), W=seq(10,1))

context(" reset")
#==========================================================
test_that("it should reset the cache", {
  # Not testable without mocking, or using the fillCache as a proxy for checking
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))

  subject <- described.class$new(SMG.list = mylist, data = data)
  expect_true(subject$fillCache())
  expect_false(equals(nrow(subject$getCache), 0))
  expect_false(subject$fillCache())

  # Resetting should clear the cache
  subject$reset()
  expect_equal(nrow(subject$getCache), 0)
  expect_true(subject$fillCache())
})

context(" setData")
#==========================================================
test_that("it should reset the summarizer", {
  mylist <- c(SMG.Mock$new(2))

  subject <- described.class$new(SMG.list = mylist)
  stub(subject$setData, 'self$reset', function() {
    called <<- TRUE
  })
  called <<- FALSE
  subject$setData(data.table(a = c(1,2,3,4)))
  expect_true(called)
})

test_that("it should should set the correct data", {
  data <- Data.Static$new(dataset = dataset)

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_null(subject$get_data_object)
  subject$setData(data)
  expect_false(is.null(subject$get_data_object))
})

context(" fillCache")
#==========================================================
test_that("it should throw if there is no data set", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_error(subject$fillCache(), 'Please set the data of the summary measure generator first')
})

test_that("it should return false if no data is needed", {
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(data = data, SMG.list = mylist)
  subject$set_minimal_measurements_needed(0)
  expect_false(subject$fillCache())
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

test_that("it should normalize the data provided to it, if a pre_processor is available", {
  my_dataset <- copy(dataset)
  my_dataset <- my_dataset + rnorm(nrow(dataset), 0, 1000)
  expect_true(max(my_dataset) > 1)
  expect_true(min(my_dataset) < 1)
  data <- Data.Static$new(dataset = my_dataset)
  bounds <-list(
    A=list(min=min(my_dataset$A), max=max(my_dataset$A)),
    Y=list(min=min(my_dataset$Y), max=max(my_dataset$Y)),
    W=list(min=min(my_dataset$W), max=max(my_dataset$W))
  )
  pre_processor <- PreProcessor$new(bounds = bounds)

  needed <- 3
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data, pre_processor = pre_processor)
  expect_true(nrow(subject$getCache) == 0)  

  subject$fillCache()
  expect_true(max(subject$getCache) <= 1)  
  expect_true(min(subject$getCache) >= 0)  
})


context(" getNext")
#==========================================================
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
    data$reset
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

test_that("it should scale the data if scaling is provided", {
  data <- Data.Static$new(dataset = dataset)
  bounds <-list(A=list(min=0, max=1000))
  pre_processor <- PreProcessor$new(bounds = bounds)

  needed <- 1
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data, pre_processor = pre_processor)
  for (i in 1:6) {
    result <- subject$getNext()
    cache  <- subject$getCache
    expected  <- dataset[i,]

    expect_true(is.a(result, 'data.frame'))

    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), ncol(dataset))
    expect_equal(result[,'Y'], expected[,'Y'])
    expect_equal(result[,'W'], expected[,'W'])

    # No explicit check for now, but at least it should not be the same (this one is scaled)
    expect_false(result[,'A'] ==  expected[,'A'])
  }
})

context(" checkEnoughDataAvailable")
#==========================================================
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


context(" is_new_timeseries")
#==========================================================
test_that("it should should return true if the current row belongs to a new timeseries", {
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))
  ## consider each TS to have 2 elements
  subject <- described.class$new(SMG.list = mylist, data = data, number_of_observations_per_timeseries = 2)
  result <- lapply(seq(1, nrow(dataset)), function(item) {
    result <- subject$is_new_timeseries
    subject$getNext()
    subject$getNext()
    result
  }) %>% unlist 

  ## All elements should be true
  result <- all(result)
  expect_true(result)
  
})

test_that("it should return true for the first timeseries (if a number_of_observations_per_timeseries is set)", {
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))
  ## +10 so its way more than the dataset
  subject <- described.class$new(SMG.list = mylist, data = data, number_of_observations_per_timeseries = (nrow(dataset) + 10))
  result <- subject$is_new_timeseries
  expect_true(result)
})

test_that("it should should return false if the current row belongs to the previous timeseries", {
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))

  subject <- described.class$new(SMG.list = mylist, data = data, number_of_observations_per_timeseries = (nrow(dataset)))
  ## Note that we first remove the first entry, as this one technically belongs
  ## to a new TS
  subject$getNext()
  ## -2 because we already popped the first entry, and now we don't want to
  ## check the last entry.
  result <- lapply(seq(1, (nrow(dataset) - 2)), function(item) {
    result <- subject$is_new_timeseries
    subject$getNext()
    result
  }) %>% unlist %>% any
  result
  expect_false(result)

  ## The last entry should be part of a new time series
  expect_true(subject$is_new_timeseries)
})

test_that("it should return false if the provided number of observations per timeseries is Inf", {
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))
  subject <- described.class$new(SMG.list = mylist, data = data, number_of_observations_per_timeseries = Inf)
  result <- lapply(seq(1, nrow(dataset)), function(item) {
    result <- subject$is_new_timeseries
    subject$getNext()
    result
  }) %>% unlist %>% any
  expect_false(result)
})
