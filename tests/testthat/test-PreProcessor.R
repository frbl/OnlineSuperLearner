context("PreProcessor.R")
described.class <- PreProcessor

#------------------------------------------------------#
context(" initialize")
test_that("it should throw if the argument provided is not a list", {
  bounds <- 'not a list'
  expect_error(described.class$new(bounds), "Argument 'bounds' is neither of nor inherits class list")
})

test_that("it should initialize", {
  bounds <- list(1)
  subject <- described.class$new(bounds)
  expect_true(is(subject, 'PreProcessor'))
})

context(" normalize")

test_that("it should normalize data according to the provided bounds", {
  data <- data.table(
                    Y =c(1,2,3,4,1,23,4,2,13,4),
                    A =c(2,2,3,4,1,23,4,2,13,4)*10,
                    W =c(3,2,3,4,1,23,4,2,13,4)*100
                    )
  bounds <- PreProcessor.generate_bounds(data)

  subject <- described.class$new(bounds = bounds)
  res <- subject$normalize(data)

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

  bounds <- PreProcessor.generate_bounds(data)
  bounds <- bounds[-1]
  not_changed <- setdiff(colnames(data), names(bounds))

  expected_not_changed <- data[,not_changed, with=FALSE]
  subject <- described.class$new(bounds = bounds)
  res <- subject$normalize(data)

  for(name in names(bounds)) {
    expect_equal(min(res[, name, with=FALSE]), 0)
    expect_equal(max(res[, name, with=FALSE]), 1)
  }
  expect_equal(res[,not_changed, with=FALSE], expected_not_changed)
})

test_that("it should throw if the provided data is not a data.table", {
  wrong_datas <- list(NULL, list(), 'a')
  subject <- described.class$new(bounds = list('123'))
  error_message <- "Argument 'data' is neither of nor inherits class data.table"
  lapply(wrong_datas, function(data) { 
    expect_error(subject$normalize(data), error_message) 
  }) 
})

context(" denormalize")
test_that("it should denormalize data according to the provided bounds", {
data <- data.table(
                    Y =c(1,2,3,4,1,23,4,2,13,4),
                    A =c(2,2,3,4,1,23,4,2,13,4)*10,
                    W =c(3,2,3,4,1,23,4,2,13,4)*100
                    )
  bounds <- PreProcessor.generate_bounds(data)
  subject <- described.class$new(bounds = bounds)
  res <- subject$normalize(data) %>% subject$denormalize(.)
  expect_equal(res, data)
})


context(" get_bounds")
test_that("it should return the bounds provided to it on initialization", {
  bounds <- list(1)
  subject <- described.class$new(bounds)
  expect_equal(subject$get_bounds, bounds)
})

context(" PreProcessor.generate_bounds")
data <- data.table(
                    Y =c(1,2,3,4,1,23,4,2,13,4),
                    A =c(2,2,3,4,1,23,4,2,13,4)*10,
                    W =c(3,2,3,4,1,23,4,2,13,4)*100
                    )
test_that("it should generate bounds with the correct entries", {
  bounds <- PreProcessor.generate_bounds(data)
  expect_equal(names(bounds), names(data))
  lapply(bounds, function(bound) {
    expect_equal(names(bound), c('max_bound','min_bound'))
  })
})

test_that("it should throw if the provided data is not a data.table", {
  wrong_datas <- list(NULL, list(), 'a')
  error_message <- "Argument 'data' is neither of nor inherits class data.table"
  expect_error(PreProcessor.generate_bounds(wrong_datas), error_message)
})

test_that("it should generate the correct bounds according to the min and max of the data", {
  bounds <- PreProcessor.generate_bounds(data)
  minsmaxs <- lapply(names(data), function(name) {
    entry <- data[,name, with=FALSE]
    c(min = min(entry), max = max(entry))
  })
  names(minsmaxs) <- names(bounds)

  lapply(names(bounds), function(name_bound) {
    minsmaxs_bounds <- minsmaxs[name_bound]
    bound <- bounds[name_bound] 
    expect_equal(bound['max_bound'], minsmaxs_bounds['max'])
    expect_equal(bound['min_bound'], minsmaxs_bounds['min'])
  })
})

