context("ML.Local.lm.R")
described.class <- ML.Local.lm

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

context(" do.predict")
test_that("it should predict using a regression prediction if the family is gaussian", {
  subject <- described.class$new(family='gaussian')
  data <- data.table(
    x=c(1,2,3,4,5,6,7,8,9),
    y=c(9,8,7,6,5,4,3,2,1)
  )
  estimator <- glm(y ~ x, data, family = gaussian())
  data.new <-  data.table(x= c(1,11,111))
  expected <- predict(estimator, data.new)
  result <- subject$perform_prediction(X_mat = data.new, m.fit = list(coef = estimator))
  expect_true(all((result -  expected) == 0))
})
test_that("it should predict using a regression prediction if the family is binomial", {
  subject <- ML.Local.lm$new(family='binomial')
  data <- data.table(
                     x=c(1,2,3,4,5,6,7,8,9,10),
                     y=c(0,0,0,0,0,0,1,0,1,1)
                     )
  data.new <-  data.table(x= c(-1,1,10,11,12))

  estimator <- glm(y ~ x, data, family = binomial())
  expected <- predict(estimator, data.new, type = 'response') > 0.5
  result <- subject$perform_prediction(X_mat = data.new, m.fit = list(coef = estimator)) > 0.5

  expect_true(all(expected == c(FALSE, FALSE, TRUE, TRUE, TRUE)))
  expect_true(all(result == expected))
})

context(" fit")
#test_that("it should call the initial GLM function with the correct parameters if no model exists", {
  #subject <- ML.Local.lm$new(family='gaussian')

  #formula <- subject$createFormula('a','b','c')
  #with_mock(
   ## I'm overriding the original GLM function to check if what gets passed to it is correct.
   ## Then I throw an error (if everything is indeed correct) to stop executing the rest of the code.
   #glm = function(form, data, family) {
     #if(ifelse(family=='gaussian', TRUE, FALSE) && ifelse(formula == formula, TRUE,FALSE))
       #stop('stop_execution')
   #},
   #expect_error(subject$fit(NULL, NULL, 'a','b'), 'stop_execution'),
   #.env = "base"
  #)
#})

#test_that("it should do a gradient descent update of the coefficients of the fitted model, if one is abailable", {
  #subject <- ML.Local.lm$new(learning.rate = 0.00001, family='gaussian')
  #data <- data.table(
    #x=c(1,2),
    #y=c(9,8)
  #)

  #subject$fit(data, Y = 'y', A = NULL, W = 'x')
  #model.pre <- subject$model

  #newdata <- data.table(
    #x=(seq(1,200) + 2),
    #y=(seq(1,200) + 13)
  #)

  #subject$fit(newdata, Y = 'y', A = NULL, W = 'x')
  #model.post <- subject$model
  #expect_false(all((model.pre - model.post) == 0))
  ## TODO: Test whether the gradient descent actually works
#})

#test_that("it should work with a large number of observations", {
  #set.seed(1234)
  #subject <- ML.Local.lm$new(learning.rate = 0.0001, family='gaussian', initialization.random = TRUE)
  #obs <- 100
  #initial <- 1
  #x0 <- rep(1,obs) # column of 1's
  #x1 <- rnorm(obs, mean=5) # original x-values

  #intercept = 1.5
  #coeff1 = 1.5

  ## create the y-matrix of dependent variables
  #noise <- rnorm(obs)
  #y <- intercept * x0 + coeff1 * x1

  #data <- data.table(x1= x1, y= y)
  #subject$fit(data[1:initial,], Y = 'y', A = NULL, W = c('x1'))
  #coeff.org <- subject$model
  #for (i in initial:obs) {
      #subject$fit(data[i,], Y = 'y', A = NULL, W = c('x1'))
  #}
  #coeff.new <- subject$model
  ## TODO: Add actual test
  #print(coeff.org)
  #print(coeff.new)
  #expect_true(abs(coeff.org[1] - intercept) >= abs(coeff.new[1] - intercept))
  #expect_true(abs(coeff.org[2] - intercept) >= abs(coeff.new[2] - intercept))
#})

