context("SMG.Lag.R")
context(" Initializer")
test_that("it should be able to initialize an objest with the correct attributes ", {
            lags  <- 2
            colnames <- c('x1')
            subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
            expect_that(subject,is_a('SMG.Lag'))
})

context(" Process")
test_that("it should create a lagged DT based on the number of lags and the data ", {
  lags  <- 3
  colnames <- c('x1')
  subject <- SMG.Lag$new(lags = lags, colnames.to.lag = colnames)
  data <- data.table(x1=c(0,1,2,3),x2=c(3,2,1,0), y=c(1,0,1,0))
  result <-  subject$process(data)

  # It should only return 1 row
  expect_equal(nrow(result), 1)

  # It should only contain the correct column names
  names.correct <- c('x1_lag_1', 'x1_lag_2', 'x1_lag_3')
  expect_equal(length(names.correct), length(names(result)))
  for (name in names.correct) {
    expect_true(name %in% names(result))
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
