context("Global.R")
context(" Implements / extends the correct functions")
test_that("it should define the expit function correctly", {
  expect_true(identical(deparse(expit), deparse(plogis)))
})
test_that("it should define the logit function correctly", {
  expect_true(identical(deparse(logit), deparse(qlogis)))
})

context(" is.a")
test_that("it should throw if the provided class is not a string", {
 a <- data.frame(a=c(1,2,3))
 expect_error(is.a(a, data.frame), 'cannot coerce type \'closure\' to vector of type \'character\'')
})

test_that("it should not throw if the provided class is a string", {
 a <- data.frame(a=c(1,2,3))
 expect_error(is.a(a, 'hoi'), NA)
 expect_false(is.a(a, 'hoi'))
})

test_that("it should check if the class is correct", {
 a <- data.frame(a=c(1,2,3))
 expect_true(is.a(a, class(a)))
})
