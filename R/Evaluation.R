#' Accuracy caluclator
Evaluation.Accuracy <- function(data.observed, data.predicted) {
  boolean.prediction <- as.numeric(data.predicted) >= 0.5
  boolean.predicted <- as.numeric(data.observed) >= 0.5
  colMeans(as.numeric(boolean.prediction == boolean.predicted))
}

#' MSE caluclator
Evaluation.MeanSquaredError <- function(data.observed, data.predicted) {
  # Calculate the MSE
  means <- colMeans((data.observed - data.predicted)^2)
  names(means) <- names(data.predicted)
  means
}

#' RMSE caluclator
Evaluation.RootMeanSquaredError <- function(data.observed, data.predicted) {
  mse <- Evaluation.MeanSquaredError(data.observed = data.observed, data.predicted = data.predicted)

  # Calculate the root of the mse
  sqrt(mse)
}
