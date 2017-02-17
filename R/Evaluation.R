#' Accuracy caluclator
Evaluation.Accuracy <- function(model, data, Y, A, W) {
  true.predicted <- 0
  all.predicted <- 0

  prediction <-  model$predict(data = data, A = A,  W = W)

  boolean.prediction <- prediction > 0.5
  boolean.true <- expit(data[, Y, with = FALSE][[Y]]) > 0.5

  all.predicted <- nrow(data)
  true.predicted <- sum(as.numeric(boolean.prediction == boolean.true))

  accuracy <- true.predicted / all.predicted
  accuracy
}

#' MSE caluclator
Evaluation.MeanSquaredError <- function(model, data, Y, A, W) {
  data.predicted <-  model$predict(data = data, A = A,  W = W)
  data.observed <- data[, Y, with = FALSE][[Y]]

  # Calculate the MSE
  mean((data.observed - data.predicted)^2)
}
