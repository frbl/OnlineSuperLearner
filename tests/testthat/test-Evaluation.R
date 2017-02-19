context("Evaluation.R")

context(" Evaluation.Accuracy")
test_that("it should calculate the accuracy correctly", {
  true.data <-      c(.1,.1,.6,.4,.6,.4,.1,.1,.4,.9)
  predicted.data <- c(.1,.2,.3,.3,.4,.4,.1,.1,.4,.9)

  accuracy <- Evaluation.Accuracy(predicted.data, true.data)
  expected  <- 0.8
  expect_equal(accuracy, expected)
})
test_that("it should also work with boolean values", {
  true.data <- c(T,F,T,F,T,F,F,T,F,T)
  predicted.data <- c(T,F,T,T,T,F,F,F,F,T)
  accuracy <- Evaluation.Accuracy(predicted.data, true.data)
  expected  <- 0.8
  expect_equal(accuracy, expected)
})

context(" Evaluation.MeanSquaredError")
test_that("it should calculate the MSE correctly", {
  true.data <-      c(1,1,6,4,6,4,1,1,4,9)
  predicted.data <- c(1,2,3,3,4,4,1,1,4,9)

  diff <- (true.data - predicted.data)^2
  expect_equal(diff, c(0, 1, 9, 1, 4, 0, 0, 0, 0, 0))
  expect_equal(mean(diff), sum(c(0, 1, 9, 1, 4, 0, 0, 0, 0, 0) / length(c(0, 1, 9, 1, 4, 0, 0, 0, 0, 0))))

  mse <- Evaluation.MeanSquaredError(predicted.data, true.data)
  expected  <- mean(diff)
  expect_equal(mse, expected)
})

context(" Evaluation.RootMeanSquaredError")
test_that("it should calculate the RMSE correctly", {
  true.data <-      c(1,1,6,4,6,4,1,1,4,9)
  predicted.data <- c(1,2,3,3,4,4,1,1,4,9)

  # Assuming the MSE is correct
  mse <- Evaluation.MeanSquaredError(predicted.data, true.data)
  expected <- sqrt(mse)

  result <- Evaluation.RootMeanSquaredError(predicted.data, true.data)
  expect_equal(result, expected)
})
