context("Evaluation.R")

context(" Evaluation.get_evaluation_function")
#==========================================================
test_that("it should always return the log_loss for a loss function", {
  fn <- Evaluation.get_evaluation_function('gaussian', TRUE)
  expect_false(is.null(fn))
  expect_equal(fn, Evaluation.log_likelihood_loss)
})
#test_that("it should give the mse_loss if a gaussion lossfunction is requested", {
  #fn <- Evaluation.get_evaluation_function('gaussian', TRUE)
  #expect_false(is.null(fn))
  #expect_equal(fn, Evaluation.log_likelihood_loss)
#})

#test_that("it should give the log_loss if a binomial lossfunction is requested", {
  #fn <- Evaluation.get_evaluation_function('binomial', TRUE)
  #expect_false(is.null(fn))
  #expect_equal(fn, Evaluation.log_likelihood_loss)
#})

#test_that("it should throw if an unknown lossfunction is requested", {
  #fake_fam = 'binominominonal'
  #expect_error(Evaluation.get_evaluation_function(fake_fam, TRUE),
               #paste('No loss function implemented for family', fake_fam))
#})

#test_that("it should give a rmse function whenever a gaussian performance measure is requested", {
  #fn <- Evaluation.get_evaluation_function('gaussian', FALSE)
  #expect_false(is.null(fn))
  #expect_equal(fn, Evaluation.root_mean_squared_error)
#})

test_that("it should give a log_likelihood_loss function whenever a performance measure is requested", {
  fn <- Evaluation.get_evaluation_function('binomial', FALSE)
  expect_false(is.null(fn))
  expect_equal(fn, Evaluation.log_loss)
})

#test_that("it should throw if an unknown performance measure is requested", {
  #fake_fam = 'binominominonal'
  #expect_error(Evaluation.get_evaluation_function('binominominonal', FALSE),
               #paste('No evaluation measure implemented for family', fake_fam))
  
#})

context(" Evaluation.log_loss")
#==========================================================
test_that("it should return the a correct log loss and not go to infinity", {
  nobs <- 100
  eps = 1e-13
  observed  <- rep(1, nobs)
  predicted <- rep(0, nobs) 
  loss <- Evaluation.log_loss(data.observed = observed, data.predicted = predicted, eps = eps)

  # Initial loss is large (just a sanity check)
  expect_true(loss > 30)
  expect_true(loss < Inf)

  # Loop over the predicted values and set them to the correct prediction, one by one
  for (i in 1:(nobs)) {
    predicted[i] <- 1
    new_loss <- Evaluation.log_loss(data.observed = observed, data.predicted = predicted, eps = eps)
    expect_true(new_loss < loss)
    loss <- new_loss
  }

  expect_lt(loss, 1e-10)
  expect_gt(loss, 0)
})

test_that("it should throw if the provided eps is too large", {
  nobs <- 100
  eps = 1
  observed  <- rep(1, nobs)
  predicted <- rep(0, nobs) 
  expect_error(Evaluation.log_loss(data.observed = observed, data.predicted = predicted, eps = eps),
        "Argument 'eps' is out of range")
})

context(" Evaluation.log_likelihood_loss")
#==========================================================
test_that("it should should return the correct log likelihood loss", {
  nobs <- 10
  observed  <- rep(1, nobs)
  predicted <- rep(0, nobs) 
  loss <- Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted)

  # Initial loss is large (just a sanity check)
  expect_true(loss > 30)
  expect_true(loss < Inf)
  for (i in 1:(nobs)) {
    # We are switching the elements 1 by one, and it should improve the quality
    predicted[i] <- 1
    new_loss <- Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted)
    expect_true(new_loss < loss)
    loss <- new_loss
  }

  expect_true(loss < 1e-10)
})

test_that("it should work with single and multiple observations", {
  for (nobs in c(1, 10)) {
    observed  <- rep(1, nobs)
    predicted <- rep(0, nobs) 
    expect_error(result <- Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted),
                NA)
    expect_true(is.numeric(result))
  }
})

test_that("it should also work with non binary values", {
  set.seed(12345)
  nobs <- 10
  observed  <- rep(1, nobs)
  predicted <- rep(0, nobs) 
  initial_loss <- Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted)

  loss <- initial_loss
  # Initial loss is large (just a sanity check)
  expect_true(loss > 30)
  expect_true(loss < Inf)
  for (i in 1:(nobs)) {
    # We are switching the elements 1 by one, and it should improve the quality
    predicted[i] <- observed[i] - max(0,min(1,rnorm(1, 0.5, 0.1)))
    new_loss <- Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted)
    expect_true(new_loss < loss)
    loss <- new_loss
  }

  # Loss should be small, but is not 0 because we never hit the truth
  expect_true(loss < initial_loss)
  expect_true(loss != 0)
})

test_that("it should also work with non binary values, and should be minimized when all are one", {
  set.seed(12345)
  nobs <- 10
  observed  <- rep(1, nobs)
  predicted <- rnorm(nobs, 0, 1)
  loss <- Evaluation.log_likelihood_loss(data.observed = NULL, data.predicted = predicted)
  # Initial loss is large (just a sanity check)
  expect_true(loss > 30)
  expect_true(loss < Inf)
  for (i in 1:(nobs)) {
    # We are switching the elements 1 by one, and it should improve the quality
    predicted[i] <- observed[i]
    new_loss <- Evaluation.log_likelihood_loss(data.observed = NULL, data.predicted = predicted)
    expect_true(new_loss < loss)
    loss <- new_loss
  }

  # Loss should go to zero
  expect_true(loss < 1e-10)
})

test_that("it should throw if the provided eps is to small or too large", {
  set.seed(12345)
  nobs <- 10
  observed  <- rep(1, nobs)
  predicted <- rnorm(nobs, 0, 1)
  eps = 1
  expect_error(Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted, eps = eps),
        "Argument 'eps' is out of range")
  eps = 1e-16
  expect_error(Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted, eps = eps),
        "Argument 'eps' is out of range")
  eps = 1e-15
  expect_error(Evaluation.log_likelihood_loss(data.observed = observed, data.predicted = predicted, eps = eps),
        NA)
  
})

context(" Evaluation.mse_loss")
#==========================================================
test_that("it should return the a correct mse loss and not go to infinity", {
  set.seed(12345)
  nobs <- 10
  error <- rnorm(nobs, 10, 5)

  observed  <- rnorm(nobs)
  predicted <- observed + error
  
  loss <- Evaluation.mse_loss(data.observed = observed, data.predicted = predicted)

  # Initial loss is large (just a sanity check)
  expect_true(loss > 30)
  expect_true(loss < Inf)
  for (i in 1:(nobs)) {
    predicted[i] <- observed[i]
    new_loss <- Evaluation.mse_loss(data.observed = observed, data.predicted = predicted)
    expect_true(new_loss < loss)
    loss <- new_loss
  }

  expect_true(loss < 1e-15)
})

test_that("it should throw when the provided data is not a vector", {
  nobs <- 10
  observed  <- rnorm(nobs)
  
  expect_error(Evaluation.mse_loss(data.observed = observed, data.predicted = list()),
               "Argument 'data.predicted' is neither of nor inherits class numeric: list", fixed = TRUE)
})

context(" Evaluation.accuracy")
#==========================================================
test_that("it should calculate the accuracy correctly", {
  true.data <-      c(.1,.1,.6,.4,.6,.4,.1,.1,.4,.9)
  predicted.data <- c(.1,.2,.3,.3,.4,.4,.1,.1,.4,.9)

  accuracy <- Evaluation.accuracy(predicted.data, true.data)
  expected  <- 0.8
  expect_equal(unname(accuracy), expected)
})

test_that("it should also predict the accuracy correctly with boolean values", {
  true.data <- c(T,F,T,F,T,F,F,T,F,T)
  predicted.data <- c(T,F,T,T,T,F,F,F,F,T)
  accuracy <- Evaluation.accuracy(predicted.data, true.data)
  expected  <- 0.8
  expect_equal(unname(accuracy), expected)
})

test_that("it should set the name correctly", {
  true.data <- c(T,F,T,F,T,F,F,T,F,T)
  predicted.data <- c(T,F,T,T,T,F,F,F,F,T)
  accuracy <- Evaluation.accuracy(predicted.data, true.data)
  expected  <- 'accuracy'
  expect_equal(names(accuracy), expected)
})

context(" Evaluation.mean_squared_error")
#==========================================================
true.data <-      c(1,1,6,4,6,4,1,1,4,9)
predicted.data <- c(1,2,3,3,4,4,1,1,4,9)
mse <- Evaluation.mean_squared_error(predicted.data, true.data)
test_that("it should calculate the MSE correctly", {
  diff <- (true.data - predicted.data)^2
  expect_equal(diff, c(0, 1, 9, 1, 4, 0, 0, 0, 0, 0))
  expect_equal(mean(diff), sum(c(0, 1, 9, 1, 4, 0, 0, 0, 0, 0) / length(c(0, 1, 9, 1, 4, 0, 0, 0, 0, 0))))

  expected  <- mean(diff)
  expect_equal(unname(mse), expected)
})

test_that("it should set the names of the outcome correctly", {
  expected  <- 'mse'
  expect_equal(names(mse), expected)
})

context(" Evaluation.root_mean_squared_error")
#==========================================================
true.data <-      c(1,1,6,4,6,4,1,1,4,9)
predicted.data <- c(1,2,3,3,4,4,1,1,4,9)

# Assuming the MSE is correct
mse <- Evaluation.mean_squared_error(predicted.data, true.data)
test_that("it should calculate the RMSE correctly", {
  expected_value <- unname(sqrt(mse))
  result <- unname(Evaluation.root_mean_squared_error(predicted.data, true.data))
  expect_equal(result, expected_value)
})

test_that("it should set the name correctly", {
  expected_value <- 'r.mse'
  result <- Evaluation.root_mean_squared_error(predicted.data, true.data)
  expect_equal(names(result), expected_value)
})

