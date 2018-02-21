library('mockery')
context("SummaryMeasureGenerator.R")
described.class <- SummaryMeasureGenerator

context(" initialize")
#==========================================================
test_that("it should only require SMG.list as a required parameter", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_true(is(subject, 'SummaryMeasureGenerator'))
})

test_that("it should initialize the correct variables (even if not provided)", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)
  expect_false(is.null(subject$getCache))
  expect_true(is(subject$getCache, 'list'))

  expect_false(is.null(subject$get_minimal_measurements_needed))
  expect_equal(subject$get_minimal_measurements_needed, 0)

  expect_false(subject$is_normalized)
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
  ## Not testable without mocking, or using the fillCache as a proxy for checking
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))

  subject <- described.class$new(SMG.list = mylist, data = data)
  expect_true(subject$fillCache())
  expect_false(equals(nrow(subject$getCache), 0))
  expect_false(subject$fillCache())

  # Resetting should clear the cache
  subject$reset()

  ## The list of caches should be the same size as the number of trajectories provided
  ## which is 1. Each entry however, should be 0
  expect_equal(length(subject$getCache), 1)
  expect_equal(nrow(subject$getCache[[1]]), 0)

  expect_true(subject$fillCache())
})

context(" set_trajectories")
#==========================================================
test_that("it should reset the summarizer", {
  mylist <- c(SMG.Mock$new(2))

  subject <- described.class$new(SMG.list = mylist)
  stub(subject$set_trajectories, 'self$reset', function() {
    called <<- TRUE
  })
  called <<- FALSE
  subject$set_trajectories(data.table(a = c(1,2,3,4)))
  expect_true(called)
})

test_that("it should should set the correct data", {
  mylist <- c(SMG.Mock$new())
  subject <- described.class$new(SMG.list = mylist)

  # TODO: Find out if we actually want to have a list with a null element
  expect_false(is.null(subject$get_trajectories))
  expect_is(subject$get_trajectories, 'list')
  expect_length(subject$get_trajectories, 1)
  expect_null(subject$get_trajectories[[1]])

  ## Now set some data and check the result again
  data <- Data.Static$new(dataset = dataset)
  subject$set_trajectories(data)

  expect_false(is.null(subject$get_trajectories))
  expect_is(subject$get_trajectories, 'list')
  expect_length(subject$get_trajectories, 1)
  expect_equal(subject$get_trajectories[[1]], data)
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
  expect_true(nrow(subject$getCache[[1]]) == 0)  

  subject$fillCache()
  expect_true(nrow(subject$getCache[[1]]) == needed -1)  
})

test_that("it should fill the cache with the correct number of measurments when using multiple trajectories", {
  data_count <- 3
  needed <- 3

  datas <- lapply(seq(data_count), function(idx) Data.Static$new(dataset = dataset))
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = datas)
  expect_length(subject$getCache, data_count)

  for (idx in seq(data_count)) {
    expect_true(nrow(subject$getCache[[idx]]) == 0)  
  }

  subject$fillCache()
  for (idx in seq(data_count)) {
    expect_true(nrow(subject$getCache[[idx]]) == needed -1)  
  }
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
  expect_true(nrow(subject$getCache[[1]]) == 0)  

  subject$fillCache()
  expect_true(max(subject$getCache[[1]]) <= 1)  
  expect_true(min(subject$getCache[[1]]) >= 0)  
})

#test_that("it should reset the cache whenever we notice that there is a new timeseries", {
  #data <- Data.Static$new(dataset = dataset)
  #mylist <- c(SMG.Mock$new(2))
  ### +10 so its way more than the dataset
  #subject <- described.class$new(SMG.list = mylist, data = data, number_of_observations_per_timeseries = nrow(dataset))
  #expect_true(subject$is_new_timeseries)

  #stub(subject$fillCache, 'self$reset', function() {
    #called <<- TRUE
  #})
  #called <<- FALSE
  #subject$fillCache()
  #expect_true(called)
#})

context(" get_latest_covariates")
#==========================================================
test_that("it should throw if the nrow of data is not equal to 1", {
  data <- Data.Static$new(dataset = dataset)
  mylist <- c(SMG.Mock$new(2))
  subject <- described.class$new(SMG.list = mylist, data = data)
  data <- data.table(a = c(1,2,3,4))
  expect_error(subject$get_latest_covariates(data), 'Not enough data provided to support all summary measures')
})

test_that("it should call the update of all SMG", {
  data_stat <- Data.Static$new(dataset = dataset)

  CALLED1 <<- FALSE
  CALLED2 <<- FALSE
  CALLED3 <<- FALSE
  
  mylist <- list(
    list(minimalObservations = 1, update = function(data) { CALLED1 <<- TRUE; return(data) }),
    list(minimalObservations = 1, update = function(data) { CALLED2 <<- TRUE; return(data) }),
    list(minimalObservations = 1, update = function(data) { CALLED3 <<- TRUE; return(data) })
  )

  subject <- described.class$new(SMG.list = mylist, data = data_stat)
  data <- data.table(a = c(1))
  subject$get_latest_covariates(data)

  expect_true(CALLED1)
  expect_true(CALLED2)
  expect_true(CALLED3)
})

test_that("it should return the results of all SMGS in a datatable", {
  data_stat <- Data.Static$new(dataset = dataset)
  
  mylist <- list(
    list(minimalObservations = 1, update = function(data) { return(data + 1) }),
    list(minimalObservations = 1, update = function(data) { return(data + 2) }),
    list(minimalObservations = 1, update = function(data) { return(data + 3) })
  )

  subject <- described.class$new(SMG.list = mylist, data = data_stat)
  data <- data.table(a = c(1))
  result <- subject$get_latest_covariates(data)
  expect_is(result, 'data.table')
  expect_equal(result, data.table(a=2,a=3,a=4))
})

context(" summarize_data")
#==========================================================
test_that("it should return the provided data if not enough data is available for all summary measures", {
  data <- Data.Static$new(dataset = data.table(a=2,a=3,a=4))
  needed <- 3
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data)
  result <- subject$summarize_data(dataset[1,])
  expect_equal(unname(result), list(dataset[1,]))
})

test_that("it should call the process function for each of the summary measure generators", {
  data_stat <- Data.Static$new(dataset = dataset)

  CALLED1 <<- FALSE
  CALLED2 <<- FALSE
  CALLED3 <<- FALSE
  
  mylist <- list(
    list(minimalObservations = 1, process = function(data) { CALLED1 <<- TRUE; return(data) }),
    list(minimalObservations = 1, process = function(data) { CALLED2 <<- TRUE; return(data) }),
    list(minimalObservations = 1, process = function(data) { CALLED3 <<- TRUE; return(data) })
  )

  subject <- described.class$new(SMG.list = mylist, data = data_stat)
  data <- data.table(a = c(1))
  subject$summarize_data(data)

  expect_true(CALLED1)
  expect_true(CALLED2)
  expect_true(CALLED3)
})

test_that("it should call the process function for each of the summary measure generators for each of the trajectories", {
  data_count <- 3
  needed <- 3

  data_stat <- lapply(seq(data_count), function(idx) Data.Static$new(dataset = dataset))

  CALLED1 <<- 0
  CALLED2 <<- 0
  CALLED3 <<- 0
  
  mylist <- list(
    list(minimalObservations = 1, process = function(data) { CALLED1 <<- CALLED1 + 1; return(data) }),
    list(minimalObservations = 1, process = function(data) { CALLED2 <<- CALLED2 + 1; return(data) }),
    list(minimalObservations = 1, process = function(data) { CALLED3 <<- CALLED3 + 1; return(data) })
  )

  subject <- described.class$new(SMG.list = mylist, data = data_stat)
  data <- list(
    data.table(a = c(1)),
    data.table(a = c(2)),
    data.table(a = c(3))
  )

  subject$summarize_data(data)

  expect_equal(CALLED1, data_count)
  expect_equal(CALLED2, data_count)
  expect_equal(CALLED3, data_count)
})

test_that("it should return a list of datatables with all results", {
  data_stat <- Data.Static$new(dataset = dataset)

  mylist <- list(
    list(minimalObservations = 1, process = function(data) { return(data + 1) }),
    list(minimalObservations = 1, process = function(data) { return(data + 2) }),
    list(minimalObservations = 1, process = function(data) { return(data + 3) })
  )

  subject <- described.class$new(SMG.list = mylist, data = data_stat)
  data <- data.table(a = c(1))
  result <- subject$summarize_data(data)
  expect_is(result, 'list')
  expect_length(result, 1)
  expect_equal(result[[1]], data.table(a=2,a=3,a=4))
})

test_that("it should throw when requesting less data than the number of trajectories", {
  data_count <- 3
  needed <- 3
  data_stat <- lapply(seq(data_count), function(idx) Data.Static$new(dataset = dataset))
  mylist <- list(
    list(minimalObservations = 1, process = function(data) { return(data + 1) }),
    list(minimalObservations = 1, process = function(data) { return(data + 2) }),
    list(minimalObservations = 1, process = function(data) { return(data + 3) })
  )
  subject <- described.class$new(SMG.list = mylist, data = data_stat)
  data <- data.table(a = c(1))
  expect_error(subject$summarize_data(data),
               'When summarizing data, always summarize for all the trajectories')
})

test_that("it should return a list of datatables with all results when there are more trajectories", {
  data_count <- 3
  needed <- 3

  data_stat <- lapply(seq(data_count), function(idx) Data.Static$new(dataset = dataset))

  mylist <- list(
    list(minimalObservations = 1, process = function(data) { return(data + 1) }),
    list(minimalObservations = 1, process = function(data) { return(data + 2) }),
    list(minimalObservations = 1, process = function(data) { return(data + 3) })
  )

  subject <- described.class$new(SMG.list = mylist, data = data_stat)

  data <- list(
    data.table(a = c(1)),
    data.table(a = c(2)),
    data.table(a = c(3))
  )

  result <- subject$summarize_data(data)
  expect_is(result, 'list')
  expect_length(result, data_count)
  for (idx in seq_along(result)) {
    res <- result[[idx]]
    base = data[[idx]]$a
    expect_equal(res, data.table(a = base + 1,
                                 a = base + 2,
                                 a = base + 3))
  }
})

test_that("it should return the correct number of entries from the tail of the dataset", {
  data_stat <- Data.Static$new(dataset = dataset)

  mylist <- list(
    list(minimalObservations = 1, process = function(data) { return(data + 1) }),
    list(minimalObservations = 1, process = function(data) { return(data + 2) }),
    list(minimalObservations = 1, process = function(data) { return(data + 3) })
  )

  subject <- described.class$new(SMG.list = mylist, data = data_stat)
  a_data <- c(1, 2, 3, 4, 5, 6, 7)
  n <- 4
  data <- data.table(a = a_data)
  result <- subject$summarize_data(data, n = n)[[1]]
  expect_is(result, 'data.table')
  expect_equal(nrow(result), n)
  expect_equal(result, data.table(a = (a_data[4:7]+1),
                                  a = (a_data[4:7]+2),
                                  a = (a_data[4:7]+3)))
})

context(" getNext")
#==========================================================
test_that("it should return the measurements requested without params", {
  data <- Data.Static$new(dataset = dataset)

  needed <- 1
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data)
  for (i in 1:6) {

    ## Only testing for one trajectory for now
    result <- subject$getNext()[[1]]
    cache  <- subject$getCache[[1]]
    expected  <- dataset[i,]

    expect_true(is.a(result, 'data.frame'))

    expect_equal(nrow(result), 1)
    expect_equal(ncol(result), ncol(dataset))
    expect_equal(result, expected)
    expect_equal(cache, expected)
  }
})

test_that("it should return the measurements requested without params for multiple trajectories", {
  skip('Test not yet implemented, very similar to the previous spec')
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
      result <- subject$getNext(n=n)[[1]]
      cache  <- subject$getCache[[1]]
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

test_that("it should return the measurements requested when specifying n for multiple trajectories", {
  skip('Test not yet implemented, very similar to the previous spec')
})

test_that("it should combine the results of multiple summarizers" , {
  data <- Data.Static$new(dataset = dataset)

  memoriesFirst <- 1
  memoriesSecond <- 2
  n <- 2
  mylist <- c(SMG.Mock$new(memoriesFirst), SMG.Mock$new(memoriesSecond))
  subject <- described.class$new(SMG.list = mylist, data = data)
  result <- subject$getNext(n=n)[[1]]

  # Note that we throw away the first measurement, this is neccessary for us to comply to the history of
  # each function
  expected  <- dataset[memoriesSecond:(memoriesSecond+n-1),]
  expected <- cbind(expected, expected)

  expect_true(is.a(result, 'data.frame'))
  expect_equal(nrow(result), n)
  expect_equal(ncol(result), ncol(dataset) * length(mylist))
  expect_equal(result, expected)

  result <- subject$getNext(n=n)[[1]]

  # Note that we don't throw away any data in this turn, as we can use the previously acquired history
  expected  <- dataset[(n+memoriesSecond):(n + (memoriesSecond+n-1)),]
  expected <- cbind(expected, expected)

  expect_true(is.a(result, 'data.frame'))
  expect_equal(nrow(result), n)
  expect_equal(ncol(result), ncol(dataset) * length(mylist))
  expect_equal(result, expected)
})

test_that("it should combine the results of multiple summarizers with multiple trajectories", {
  skip('Test not yet implemented, very similar to the previous spec')
})

test_that("it should scale the data if scaling is provided", {
  data <- Data.Static$new(dataset = dataset)
  bounds <-list(A=list(min=0, max=1000))
  pre_processor <- PreProcessor$new(bounds = bounds)

  needed <- 1
  mylist <- c(SMG.Mock$new(needed))
  subject <- described.class$new(SMG.list = mylist, data = data, pre_processor = pre_processor)
  for (i in 1:6) {
    result <- subject$getNext()[[1]]
    cache  <- subject$getCache[[1]]
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

test_that("it should scale the data if scaling is provided with multiple trajectories", {
  skip('Test not yet implemented, very similar to the previous spec')
})

context(" check_enough_data_available")
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
  expect_true(subject$check_enough_data_available(c(f1,f2,f3)))
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
  expect_error(subject$check_enough_data_available(c(f1,f2,f3)), expected_msg, fixed = TRUE)
  expect_true(subject$check_enough_data_available(c(f1,f2)))
})
