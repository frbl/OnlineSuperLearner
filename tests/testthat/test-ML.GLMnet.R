context("ML.GLMnet.R")
described.class <- ML.GLMnet
subject <- described.class$new()

context(" initialize")
test_that("it should initialize with the defaults", {
  subject <- described.class$new()
  expect_true(is(subject, 'ML.GLMnet'))
})

test_that("it should throw if the provided family is not correct", {
  expect_error(described.class$new(family = 'incorrect'))
  expect_error(described.class$new(family = 'binomial'), NA)
})

test_that("it should throw if alpha is not valid", {
  expect_error(described.class$new(alpha = -0.0001))
  expect_error(described.class$new(alpha = 1.0001))

  # Correct examples
  expect_error(described.class$new(alpha = 0), NA)
  expect_error(described.class$new(alpha = 1), NA)
})

test_that("it should throw if nlambda is < 1 ", {
  expect_error(described.class$new(nlambda = -0.9999))
  expect_error(described.class$new(nlambda = 1), NA)
})

context(" fitfunname")
test_that("it should define a fitfunname", {
  expect_false(is.null(subject$fitfunname)) 
  expect_equal(subject$fitfunname, 'glmnet') 
})

context(" lmclass")
test_that("it should define an lmclass", {
  expect_false(is.null(subject$lmclass)) 
  expect_equal(subject$lmclass, 'glmnet') 
})

context(" get_validity")
test_that("it should return true if the config is valid", {
  allowed_families <- c("gaussian","binomial","poisson","multinomial","cox","mgaussian")
  for (family in allowed_families) {
    subject <- described.class$new(family = family)
    expect_true(subject$get_validity) 
  }
})

test_that("it should throw if the config is not valid", {
  allowed_families <- c("gaussian","binomial","poisson","multinomial","cox","mgaussian")
  expected <- paste('Family <not correct!> is not in list of allowed families:', paste(allowed_families, collapse=' '))
  expect_error(described.class$new(family = '<not correct!>'), expected, fixed = TRUE) 
})

