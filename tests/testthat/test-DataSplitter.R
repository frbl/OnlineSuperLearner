context("DataSplitter.R")
described.class <- DataSplitter

context(' initialize')
test_that("it should initialize", {
  expect_error(described.class$new(), NA)
})

context(' split')
data <- data.table(x=c(1,2,3,4), y=c(4,3,2,1))
ds <- described.class$new()
result <- ds$split(data)
testset_size <- 1
test_that("it should return a test and training set", {
  expect_false(is.null(result))
  expect_equal(names(result), c('train', 'test'))
})

test_that("it should have the correct entries in the testset", {
 expect_equal(nrow(result$test), testset_size)
 expect_equal(result$test, as.data.table(t(c(x=4, y=1))))
})

test_that("it should have the correct entries in the trainingset", {
 expect_equal(nrow(result$train), nrow(data) - testset_size)
 expected <- data.table(x=c(1,2,3), y=c(4,3,2))
 expect_equal(result$train, expected)
})


