context("ML.H2O.randomForest.R")
described.class <- ML.H2O.randomForest

context(" initialize")
##=====================================================================
test_that("it should initialize", {
  expect_error(described.class$new() , NA)
})

test_that("it should nfolds to a default value", {
  expected <- 0;
  subject <- described.class$new()
  result <- subject$get_nfolds
  expect_equal(result, expected)
})
test_that("it should set the nfolds", {
  expected <- 1;
  subject <- described.class$new(nfolds = expected)
  result <- subject$get_nfolds
  expect_equal(result, expected)
})

test_that("it should ntrees to a default value", {
  expected <- 50;
  subject <- described.class$new()
  result <- subject$get_ntrees
  expect_equal(result, expected)
})
test_that("it should set the ntrees", {
  expected <- 1;
  subject <- described.class$new(ntrees = expected)
  result <- subject$get_ntrees
  expect_equal(result, expected)
})

test_that("it should min_rows to a default value", {
  expected <- 1;
  subject <- described.class$new()
  result <- subject$get_min_rows
  expect_equal(result, expected)
})
test_that("it should set the min_rows", {
  expected <- 1;
  subject <- described.class$new(min_rows = expected)
  result <- subject$get_min_rows
  expect_equal(result, expected)
})

test_that("it should verbose to a default value", {
  expected <- FALSE;
  subject <- described.class$new()
  result <- subject$get_verbose
  expect_equal(result, expected)
})
test_that("it should set the verbose", {
  expected <- TRUE;
  subject <- described.class$new(verbose = expected)
  result <- subject$get_verbose
  expect_equal(result, expected)
})
##=====================================================================
