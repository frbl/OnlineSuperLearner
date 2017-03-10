#' Accuracy caluclator
Evaluation.Accuracy <- function(data.observed, data.predicted) {
  boolean.predicted <- as.numeric(data.predicted) >= 0.5
  boolean.observed <- as.numeric(data.observed) >= 0.5
  if (is.a(boolean.predicted, 'matrix')) {
    means <- colMeans(as.numeric(boolean.observed == boolean.predicted))
    names(means) <- names(data.predicted)
    return(means)
  }
  mean(as.numeric(boolean.observed == boolean.predicted))
}

#' MSE caluclator
Evaluation.MeanSquaredError <- function(data.observed, data.predicted) {
  # Calculate the MSE
  se <- (data.predicted - data.observed)^2
  if (is.a(data.predicted, 'matrix')) {
    means <- colMeans(se)
    names(means) <- names(data.predicted)
    return(means)
  }
  mean(se)
}

#' RMSE caluclator
Evaluation.RootMeanSquaredError <- function(data.observed, data.predicted) {
  mse <- Evaluation.MeanSquaredError(data.observed = data.observed, data.predicted = data.predicted)

  # Calculate the root of the mse
  sqrt(mse)
}