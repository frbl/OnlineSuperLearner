context("LibraryFactory")
context(" initialize")
test_that("it should initialize with the correct parameters", {
  models <- c('ML.H2O.glm')
  result <- LibraryFactory(ML.models.allowed = models)
  expect_that(result, is_a('LibraryFactory'))
})

test_that("It should test that the provided model names start with ML", {
  incorrect.models <- c('H2O.glm')
  expected.msg <- 'Not all provided models are ML models as models should start with ML, please only use ML models.'

  expect_that(LibraryFactory(ML.models.allowed = incorrect.models), throws_error(expected.msg))
})

test_that("it should set a basic list of allowed models", {
  result <- LibraryFactory()
  expect_that(result@ML.models.allowed, matches('ML.H2O.glm') )
})

context(" fabricate")
subject <- LibraryFactory()
SL.library <- c('ML.H2O.glm')

test_that("it should fabricate a model when it is a valid one ", {
 result <- fabricate(subject, SL.library) 
 expect_that(result, is_a('list'))
})
