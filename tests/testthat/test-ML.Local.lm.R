context("ML.Local.lm.R")
context(" initialize")
test_that("it should accept the learning.rate and familyparameter", {
 expect_error(ML.Local.lm$new(learning.rate = 1, family = 'gaussian'), NA)
})
test_that("it should build without any parameters", {
 expect_error(ML.Local.lm$new(), NA)
})
test_that("it should throw if an invalid family is provided", {
 expect_error(ML.Local.lm$new(family = 'bush'))
})
test_that("it should throw if a negative learning.rate is provided", {
 expect_error(ML.Local.lm$new(learning.rate = 0))
 expect_error(ML.Local.lm$new(learning.rate = -1))
})

context(" predict")
test_that("it should predict using a regression prediction if the family is gaussian", {
  subject <- ML.Local.lm$new(family='gaussian')
  data <- data.table(
                     x=c(1,2,3,4,5,6,7,8,9),
                     y=c(9,8,7,6,5,4,3,2,1)
                     )
  estimator <- glm(y ~ x, data, family = gaussian())
  data.new <-  data.table(x= c(1,11,111))
  expected <- predict(estimator, data.new)
  subject$model <- estimator
  result <- subject$predict(data = data.new, A = NULL, W='x')

  expect_equal(result, expected)
})
test_that("it should predict using a regression prediction if the family is binomial", {
  subject <- ML.Local.lm$new(family='binomial')
  data <- data.table(
                     x=c(2,3,4,5,6,7,8,9,10),
                     y=c(0,0,0,0,0,1,0,1,1)
                     )
  data.new <-  data.table(x= c(-1,1,10,11,12))

  estimator <- glm(y ~ x, data, family = binomial())
  subject$model <- estimator
  expected <- predict(estimator, data.new, type = 'response') > 0.5
  result <- subject$predict(data = data.new, A = NULL, W='x') > 0.5

  expect_true(all(expected == c(FALSE, FALSE, TRUE, TRUE, TRUE)))
  expect_true(all(result == expected))
})
context(" fit")
test_that("it should call the initial GLM function with the correct parameters if no model exists", {
  subject <- ML.Local.lm$new(family='gaussian')
  formula <- subject$createFormula('a','b','c')
  with_mock(
   glm = function(form, data, family) ifelse(family=='gaussian', TRUE, FALSE) && ifelse(formula == formula, TRUE,FALSE),
   expect_true({ subject$fit(NULL, 'a','b','c') }),
   .env = "base"
  )
})

test_that("it should do a gradient descent update of the coefficients of the fitted model, if one is abailable", {
  subject <- ML.Local.lm$new(learning.rate = 0.00001, family='gaussian')
  data <- data.table(
    x=c(1,2),
    y=c(9,8)
  )

  subject$fit(data, Y = 'y', A = NULL, W = 'x')
  model.pre <- subject$model$coefficients

  newdata <- data.table(
    x=(seq(1,200) + 2),
    y=(seq(1,200) + 13)
  )

  subject$fit(newdata, Y = 'y', A = NULL, W = 'x')
  model.post <- subject$model$coefficients
  expect_false(all((model.pre - model.post) == 0))
  # TODO: Test whether the gradient descent actually works
})



