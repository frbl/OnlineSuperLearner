context("LibraryFactory")
context(" Initialize")
test_that("it should initialize with the correct parameters", {
  result <- LibraryFactory(SL.type = 'h2o')
  expect_that(result, is_a('LibraryFactory'))
})

test_that("it should throw an error when an unsupported SL.type is used", {
  expected.msg <- 'The provided SL.type is not valid'
  expect_that(LibraryFactory(SL.type='somethingwrong'), throws_error(expected.msg))
})

test_that("it should set a correct default protocol", {
  result <- LibraryFactory()
  expect_that(result@SL.type, matches('h2o') )
})
context(" Fabricate")
