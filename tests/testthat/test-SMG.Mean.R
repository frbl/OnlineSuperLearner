context("SMG.Mean.R")
context(" Initializer")
test_that("it should be able to initialize an objest with the correct attributes ", {
  colnames <- c('x1')
  subject <- SMG.Mean$new(colnames.to.mean = colnames)
  expect_that(subject,is_a('SMG.Mean'))
})

context(" Process")
test_that("it should return a dataframe with the correct names", {
  colnames <- c('x1', 'x2')
  subject <- SMG.Mean$new(colnames.to.mean = colnames)
  data <- data.table(x1=c(0,1,2,3),x2=c(5,2,1,0), y=c(1,0,1,0))
  result <-  subject$process(data)

  expect_equal(length(names(result)), length(colnames))
  for (name in c('x1_mean', 'x2_mean')) {
    expect_true(name %in% names(result))
  }
  expect_true(all(unlist(result['x1_mean']) == mean(data$x1)))
  expect_true(all(unlist(result['x2_mean']) == mean(data$x2)))
})

test_that("it should be able to update an existing mean", {
  colnames <- c('x1', 'x2')
  subject <- SMG.Mean$new(colnames.to.mean = colnames)
  data <- data.table(x1=c(0,1,2,3),x2=c(5,2,1,0), y=c(1,0,1,0))
  subject$process(data)

  newdata <- data.table(x1=c(3),x2=c(5), y=c(1))
  result <-  subject$process(newdata)

  expect_true(all(unlist(result['x1_mean']) == mean(c(data$x1,  newdata$x1))))
  expect_true(all(unlist(result['x2_mean']) == mean(c(data$x2,  newdata$x2))))
})

context(" minimalObservations")
test_that("it should return the minimal number of observations needed for a summary measure", {
  colnames <- c('x1')
  expected <- 1
  subject <- SMG.Mean$new(colnames.to.mean = colnames)
  expect_equal(subject$minimalObservations, expected)
})
