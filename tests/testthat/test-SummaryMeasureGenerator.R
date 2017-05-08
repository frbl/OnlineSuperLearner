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

test_that("it should throw when bounds are set but are not a list", {
  mylist <- c(SMG.Mock$new())
  expected_message =  "Argument 'bounds' is neither of nor inherits class list: character"
  expect_error(described.class$new(SMG.list = mylist, bounds='notalist'), expected_message)
})

test_that("it should set normalized to false if no bounds are provided", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_false(subject$is_normalized)
})

test_that("it should set normalized to true and store the bounds whenever bounds are provided", {
  bounds <-list(A=list(min=0, max=1000))
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist, bounds = bounds)
  expect_true(subject$is_normalized)
  expect_equal(subject$get_bounds, bounds)
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

test_that("it should normalize the data provided to it, if bounds are available", {
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

  needed <- 3
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data, bounds = bounds)
  expect_true(nrow(subject$getCache) == 0)  

  subject$fillCache()
  expect_true(max(subject$getCache) <= 1)  
  expect_true(min(subject$getCache) >= 0)  
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

test_that("it should scale the data if scaling is provided", {
  data <- Data.Static$new(dataset = dataset)
  bounds <-list(A=list(min=0, max=1000))

  needed <- 1
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data, bounds = bounds)
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

context(" normalize")
test_that("it should work with multi dimensional data", {
  data <- data.table(
                     Y =c(1,2,3,4,1,23,4,2,13,4),
                     A =c(2,2,3,4,1,23,4,2,13,4)*10,
                     W =c(3,2,3,4,1,23,4,2,13,4)*100
                     )

  bounds <- list()
  for(name in colnames(data)) {
    min_bound = min(data[, name, with=FALSE] )
    max_bound = max(data[, name, with=FALSE] )
    bounds <- append(bounds, list(list(max_bound = max_bound, min_bound = min_bound)))
  }
  names(bounds) <- colnames(data)

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)

  res <- subject$normalize(data, bounds)

  for(name in colnames(data)) {
    expect_equal(min(res[, name, with=FALSE]), 0)
    expect_equal(max(res[, name, with=FALSE]), 1)
  }
})

test_that("it should work with multi dimensional data when bounds are provided for a subset of the variables", {
  data <- data.table(
    Y =c(1,2,3,4,1,23,4,2,13,4),
    A =c(2,2,3,4,1,23,4,2,13,4)*10,
    W =c(3,2,3,4,1,23,4,2,13,4)*100
  )

  bounds <- list()
  for(name in colnames(data)) {
    min_bound = min(data[, name, with=FALSE] )
    max_bound = max(data[, name, with=FALSE] )
    bounds <- append(bounds, list(list(max_bound = max_bound, min_bound = min_bound)))
  }
  names(bounds) <- colnames(data)
  bounds <- bounds[-1]
  not_changed <- setdiff(colnames(data), names(bounds))

  expected_not_changed <- data[,not_changed, with=FALSE]

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)

  res <- subject$normalize(data, bounds)

  for(name in names(bounds)) {
    expect_equal(min(res[, name, with=FALSE]), 0)
    expect_equal(max(res[, name, with=FALSE]), 1)
  }
  expect_equal(res[,not_changed, with=FALSE], expected_not_changed)
})

test_that("it should throw if the provided data is not a data.table", {
  wrong_datas <- list(NULL, list(), 'a')

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  error_message <- "Argument 'data' is neither of nor inherits class data.table:"
  lapply(wrong_datas, function(data) { 
    current_error <- paste(error_message, class(data))
    expect_error(subject$normalize(data), current_error) 
  }) 
})

test_that("it should throw if the provided bounds are not a list", {
  wrong_bounds <- list(NULL, data.frame, 'a')
  data <- data.table(
    Y =c(1,2,3,4,1,23,4,2,13,4),
    A =c(2,2,3,4,1,23,4,2,13,4)*10,
    W =c(3,2,3,4,1,23,4,2,13,4)*100
  )

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  error_message <- "Argument 'bounds' is neither of nor inherits class list:"
  lapply(wrong_bounds, function(bounds) { 
    current_error <- paste(error_message, class(bounds))
    expect_error(subject$normalize(data, bounds), current_error) 
  }) 
})


test_that("it should use the initialized bounds when no bounds are provided", {
  data <- data.table(
    Y =c(1,2,3,4,1,23,4,2,13,4),
    A =c(2,2,3,4,1,23,4,2,13,4)*10,
    W =c(3,2,3,4,1,23,4,2,13,4)*100
  )

  bounds <- list()
  for(name in colnames(data)) {
    min_bound = min(data[, name, with=FALSE] )
    max_bound = max(data[, name, with=FALSE] )
    bounds <- append(bounds, list(list(max_bound = max_bound, min_bound = min_bound)))
  }
  names(bounds) <- colnames(data)

  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist, bounds = bounds)

  res <- subject$normalize(data)

  for(name in colnames(data)) {
    expect_equal(min(res[, name, with=FALSE]), 0)
    expect_equal(max(res[, name, with=FALSE]), 1)
  }
 
})

