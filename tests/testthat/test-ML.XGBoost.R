context("ML.XGBoost.R")
described.class <- ML.XGBoost
subject <- described.class$new()

context(" initialize")
test_that("it should initialize with the defaults", {
  subject <- described.class$new()
  expect_true(is.a(subject, 'ML.XGBoost'))
})

test_that("it should throw if the rounds are 0 or negative", {
  expect_error(described.class$new(rounds = -1))
  expect_error(described.class$new(rounds = 0))
})

test_that("it should throw if the booster is not valid", {
            # Implement checks
})

test_that("it should throw if alpha is < 0 or > 1", {
  expect_error(described.class$new(alpha = -0.0001))
  expect_error(described.class$new(alpha = 1.0001))
})

test_that("it should throw if lambda is < 0 or > 1", {
  expect_error(described.class$new(lambda = -0.0001))
  expect_error(described.class$new(lambda = 1.0001))
})

test_that("it should throw if eta is <= 0", {
  expect_error(described.class$new(eta = 0.0001), NA)
  expect_error(described.class$new(eta = 0))
  expect_error(described.class$new(eta = -1))
})

test_that("it should throw if gamma is < 0", {
  expect_error(described.class$new(gamma = -1))
})

context(" fitfunname")
test_that("it should define a fitfunname", {
  expect_false(is.null(subject$fitfunname)) 
})

context(" lmclass")
test_that("it should define a fitfunname", {
  expect_false(is.null(subject$lmclass)) 
})

context(" get_validity")
test_that("it should return true if the config is valid", {
  allowed_boosters <- c('gbtree', 'gblinear', 'dart')
  for (booster in allowed_boosters) {
    subject <- described.class$new(booster = booster)
    expect_true(subject$get_validity) 
  }
})

test_that("it should return throw if the config is not valid", {
  allowed_boosters <- c('gbtree', 'gblinear', 'dart')
  expected <- paste('Booster <not correct!> is not in list of allowed boosters:', paste(allowed_boosters, collapse=' '))
  expect_error(described.class$new(booster = '<not correct!>'), expected, fixed = TRUE) 
})

