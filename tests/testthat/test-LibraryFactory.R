context("LibraryFactory")
described.class <- LibraryFactory

context(" initialize")

context(" fabricate")
context("  > without gridsearch")
test_that("it should should throw if the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- c('Wont work!') 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model name')
})

test_that("it should should throw if one of the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm', 'Wont work!') 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model name')
})

test_that("it should work with list of estimators", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm') 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  for (i in 1:length(result)) {
    model <- result[[i]]
    expect_true(is.a(model, SL.library[i]))
    expect_true(is.a(model, 'ML.Base'))
  }
})

test_that("it should work with a single estimator", {
  subject <- described.class$new()
  SL.library <- 'ML.Local.lm' 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  expect_true(is.a(result[[1]], 'ML.Local.lm'))
  expect_true(is.a(result[[1]], 'ML.Base'))
})

context("  > with gridsearch")
test_that("it should should throw if the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- list(list(name = 'Wont work!')) 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model name')
})

test_that("it should throw if one of the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- list(list(name='ML.Local.lm'), list(name = 'Wont work!')) 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model name')
})

test_that("it should work without prividing parameters", {
described.class <- LibraryFactory
  subject <- described.class$new()
  SL.library <- list(list(name='ML.Local.lm')) 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  for (i in 1:length(result)) {
    model <- result[[i]]
    expect_true(is.a(model, SL.library[i]))
    expect_true(is.a(model, 'ML.Base'))
  }
})

test_that("it should expand a grid for gridsearch", {
  subject <- described.class$new()
  SL.library <- list(list(name = 'ML.Local.lm', params = list(family= c('binomial', 'gaussian'), learning.rate = c(1, 2, 3, 4)))) 
  result <- subject$fabricate(SL.library)
  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 4 * 2 )
  for (model in result) {
    expect_true(is.a(model, 'ML.Local.lm'))
    expect_true(is.a(model, 'ML.Base'))
  }
  # TODO: Also test the actual params
})
