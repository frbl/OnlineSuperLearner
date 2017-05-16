context("SMG.Latest.Entry.R")

described.class <- SMG.Latest.Entry
colnames <- c('x1')
context(" initialize")
test_that("it should be able to initialize an objest with the correct attributes ", {
  subject <- described.class$new(colnames.to.use = colnames)
  expect_that(subject,is_a('SMG.Latest.Entry'))
})

context(" minimalObservations")
test_that("it should expose a minimal observations variable", {
  subject <- described.class$new(colnames.to.use = colnames)
  expect_equal(subject$minimalObservations, 1) 
})

context(" update")
test_that("it should expose an update function that returns the correct columns for the provided data ", {
  data <- data.table(x1 = c(1,2,3,4), x2 = c(5,4,3,2), x3 = c(7,2,2,3))
  subject <- described.class$new(colnames.to.use = colnames)
  result <- subject$update(data) 
  expected <- data[, 'x1', with=FALSE]
  expect_true(is.a(result, 'data.table'))
  expect_equal(ncol(result), length(colnames))
  expect_equal(nrow(result), 1)
  expect_true(all(is.na(result)))
})

context(" process")
test_that("it should return a subset using the colnames to use variable", {
  subject <- described.class$new(colnames.to.use = colnames)

  data <- data.table(x1 = c(1,2,3,4), x2 = c(5,4,3,2), x3 = c(7,2,2,3))
  result <- subject$process(data)
  expect_equal(colnames(result), colnames)
  expect_equal(length(result), subject$minimalObservations) 

  data <- data.table(x1 = c(1), x2 = c(5), x3 = c(7))
  result <- subject$process(data)
  expect_equal(colnames(result), colnames)
  expect_equal(length(result), subject$minimalObservations) 
})

test_that("it should throw if not enough measurements are available", {
  minobs <- 100
  data <- data.table(x1 = c(1,2,3,4), x2 = c(5,4,3,2), x3 = c(7,2,2,3))
  subject <- described.class$new(colnames.to.use = colnames)
  subject$minimalObservations <- minobs 
  expect_error(subject$process(data), paste('At least', minobs, 'observations required'))
})

context(" exposedVariables")
test_that("it should return a list of exposed variables", {
  subject <- described.class$new(colnames.to.use = colnames)
  result <- subject$exposedVariables
  expect_equal(result, colnames)
})

