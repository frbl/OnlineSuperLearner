context("SMG.Lag.R")
context(" Initializer")
test_that("it should be able to initialize an objest with the correct attributes ", {
            lags  <- 2
            colnames <- c('x1')
            subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
            expect_that(subject,is_a('SMG.Lag'))
})

context(" LaggedColNames")
test_that("it should return a list with the correct colnames, in the correct order", {
  lags  <- 3
  colnames <- c('x1', 'x2', 'x3', 'x4')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  result <-  subject$laggedColnames()
  expected  <- c('x1_lag_1', 'x1_lag_2', 'x1_lag_3',
                'x2_lag_1', 'x2_lag_2', 'x2_lag_3',
                'x3_lag_1', 'x3_lag_2', 'x3_lag_3',
                'x4_lag_1', 'x4_lag_2', 'x4_lag_3')
  expect_equal(result, expected)
})

context(" Process")
lags  <- 3
colnames <- c('x1', 'x2')
subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
data <- data.table(x1=c(0,1,2,3),x2=c(1000,900,800,700), x3=c(9,7,5,3), y=c(1,0,1,0))
result <-  subject$process(data)

test_that("it should only return a single row of data", {
  expect_equal(nrow(result), 1)
})

test_that("it should create a lagged DT with the correct column names ", {
  names.correct <- c('x1_lag_1', 'x1_lag_2', 'x1_lag_3', 'x2_lag_1', 'x2_lag_2', 'x2_lag_3')
  expect_equal(length(names.correct), length(names(result)))
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

test_that("it should throw when the number of observations provided is not enoug", {
  lags  <- 4
  colnames <- c('x1')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  data <- data.table(x1=c(0,1,2,3),x2=c(3,2,1,0), y=c(1,0,1,0))
  expect_that(subject$process(data),throws_error('At least 5 observations required') )
})

context(" minimalObservations")
test_that("it should return the minimal number of observations needed for a summary measure", {
  lags  <- 2

  # We need #lags number of observations to add a summary of them to the current 1 observation
  expected <- lags + 1
  colnames <- c('x1')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  expect_equal(subject$minimalObservations, expected)
})
