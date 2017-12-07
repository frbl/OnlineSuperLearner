context("SMG.Lag.R")
described.class <- SMG.Lag

context(" initialize")
#==========================================================
test_that("it should be able to initialize an objest with the correct attributes ", {
  lags  <- 2
  colnames <- c('x1')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  expect_that(subject,is_a('SMG.Lag'))
})

context(" lagged_colnames")
#==========================================================
test_that("it should return a list with the correct colnames, in the correct order", {
  lags  <- 3
  colnames <- c('x1', 'x2', 'x3', 'x4')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = 'empty')
  result <-  subject$lagged_colnames(colnames = colnames)
  expected  <- c('x1_lag_1', 'x1_lag_2', 'x1_lag_3',
                'x2_lag_1', 'x2_lag_2', 'x2_lag_3',
                'x3_lag_1', 'x3_lag_2', 'x3_lag_3',
                'x4_lag_1', 'x4_lag_2', 'x4_lag_3')
  expect_equal(result, expected)
})

test_that("it should use the default column names when nothing is provided to the function", {
  lags  <- 3
  colnames <- c('x1', 'x2', 'x3', 'x4')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  result <-  subject$lagged_colnames()
  expected  <- c('x1_lag_1', 'x1_lag_2', 'x1_lag_3',
                'x2_lag_1', 'x2_lag_2', 'x2_lag_3',
                'x3_lag_1', 'x3_lag_2', 'x3_lag_3',
                'x4_lag_1', 'x4_lag_2', 'x4_lag_3')
  expect_equal(result, expected)
})

context(" update")
#==========================================================
test_that("it should work with a set of columns that are similar", {
  data <- data.table(x = c(1,2,3,4), x2 = c(5,4,3,2), x_lag_1 = c(6,5,4,3), x2_lag_1 = c(2,3,4,5))
  colnames <- c('x', 'x2')
  subject <- described.class$new(lags = 1, colnames.to.lag = colnames)
  result <- subject$update(data[1,]) 
  expect_equal(length(names(result)), length(colnames))
})

test_that("it should return a correct new row", {
  data <- data.table(x = c(1,2,3,4), x2 = c(5,4,3,2), x_lag_1 = c(6,5,4,3), x2_lag_1 = c(2,3,4,5))
  colnames <- c('x', 'x2')
  lags <- 1
  subject <- described.class$new(lags = lags, colnames.to.lag = colnames)
  result <- subject$update(data[1,]) 
  expect_equal(length(names(result)), length(colnames) * lags)
  expect_named(result, c('x_lag_1', 'x2_lag_1'))
  expect_equal(result, c(x_lag_1 = data[[1,'x']], x2_lag_1 = data[[1,'x2']]))
})

test_that("it should also work with more than 1 lag", {
  data <- data.table(x = c(1,2,3,4), x2 = c(5,4,3,2), x_lag_1 = c(2,3,4,5), x2_lag_1 = c(4,3,2,1), x_lag_2 = c(3,4,5,6), x2_lag_2 = c(3,2,1,0))
  lags <- 2
  colnames <- c('x', 'x2')
  subject <- described.class$new(lags = lags, colnames.to.lag = colnames)
  result <- subject$update(data[1,]) 
  expect_equal(length(names(result)), length(colnames) * lags)
  expect_named(result, c('x_lag_1', 'x_lag_2', 'x2_lag_1', 'x2_lag_2'))

  expect_equal(result, c(x_lag_1 = data[[1,'x']], x_lag_2 = data[[1,'x_lag_1']],
                         x2_lag_1 = data[[1,'x2']], x2_lag_2 = data[[1,'x2_lag_1']]))
})

test_that("it should throw when data with more than one row is provided", {
  data <- data.table(x = c(1,2,3,4), x2 = c(5,4,3,2), x_lag_1 = c(2,3,4,5), x2_lag_1 = c(4,3,2,1), x_lag_2 = c(3,4,5,6), x2_lag_2 = c(3,2,1,0))
  lags <- 2
  colnames <- c('x', 'x2')
  subject <- described.class$new(lags = lags, colnames.to.lag = colnames)
  expect_error(subject$update(data), 'Provided number of rows to lag / update function needs to be one')
})

context(" Process")
#==========================================================
lags  <- 3
colnames <- c('x1', 'x2')
subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
data <- data.table(x1=c(0,1,2,3),x2=c(1000,900,800,700), x3=c(9,7,5,3), y=c(1,0,1,0))
result <-  subject$process(data)

test_that("it should only return a single row of data", {
  expect_equal(nrow(result), 1)
})

test_that("it should return a dataframe", {
  expect_is(result, 'data.table')
})

test_that("it should create a lagged DT with the correct column names ", {
  names.correct <- c('x1_lag_1', 'x1_lag_2', 'x1_lag_3', 'x2_lag_1', 'x2_lag_2', 'x2_lag_3')
  expected_name_length <- length(names(result))
  expect_length(names.correct, expected_name_length)
  for (name in names.correct) {
    expect_true(name %in% names(result))
  }
})

test_that("it should create a lagged DT with the correct data", {
  for (var in colnames) {
    for(lag in 1:lags) {
      correct.index <- nrow(data) - lag
      lagcol <- paste(var, 'lag', lag, sep='_')
      res <- result[,lagcol, with=FALSE]
      expected <- data[correct.index, var, with=FALSE]
      diff <- (res - expected)
      expect_true(diff == 0)
    }
  }
})

test_that("it should throw when the number of observations provided is not enough", {
  lags  <- 4
  colnames <- c('x1')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  data <- data.table(x1=c(0,1,2,3),x2=c(3,2,1,0), y=c(1,0,1,0))
  expect_that(subject$process(data),throws_error('At least 5 observations required') )
})

context(" minimalObservations")
#==========================================================
test_that("it should return the minimal number of observations needed for a summary measure", {
  lags  <- 2

  # We need #lags number of observations to add a summary of them to the current 1 observation
  expected <- lags + 1
  colnames <- c('x1')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  expect_equal(subject$minimalObservations, expected)
})

context(" exposedVariables")
#==========================================================
test_that("it should return the list of variables exposed by the lag operator", {
  lags  <- 2

  # We need #lags number of observations to add a summary of them to the current 1 observation
  colnames <- c('x1', 'x2')
  expected <- c('x1_lag_1','x1_lag_2','x2_lag_1','x2_lag_2')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  expect_equal(subject$exposedVariables, expected)
})
