#' @import Metrics
Evaluation.get_evaluation_function = function(family, useAsLoss=TRUE) {
  if(useAsLoss){
    if (family == 'gaussian') {
      return(Evaluation.mse_loss)
    } else if(family == 'binomial') {
      return(Evaluation.log_loss)
    } else {
      throw('No loss function implemented for family ', family)
    }
  }
  if (family == 'gaussian') {
    return(Evaluation.mean_squared_error)
  } else if(family == 'binomial') {
    return(Evaluation.accuracy)
  } else {
    throw('No evaluation measure implemented for family ', family)
  }

}

#' Accuracy caluclator
Evaluation.accuracy <- function(data.observed, data.predicted) {
  boolean.predicted <- as.numeric(data.predicted) >= 0.5
  boolean.observed <- as.numeric(data.observed) >= 0.5
  if (is.a(boolean.predicted, 'matrix')) {
    means <- colMeans(as.numeric(boolean.observed == boolean.predicted))
    names(means) <- names(data.predicted)
    return(means)
  }
  mean(as.numeric(boolean.observed == boolean.predicted))
}

#' Log loss evaluation metric
Evaluation.log_loss <- function(data.observed, data.predicted, eps = 1e-15) {
  data.predicted = pmin(pmax(data.predicted, eps), 1-eps)
  loss <- - (sum(data.observed * log(data.predicted) + (1 - data.observed) * log(1 - data.predicted))) / length(data.observed)

  # TODO: Not entirely sure why, but the loss is 0 if every element is different
  loss
}

Evaluation.mse_loss <-  function(data.observed, data.predicted) {
  mean((data.predicted - data.observed)^2)
}

#' MSE caluclator
Evaluation.mean_squared_error <- function(data.observed, data.predicted) {
  # Calculate the MSE
  se <- Evaluation.mse_loss(data.predicted, data.observed)
  if (is.a(data.predicted, 'matrix')) {
    means <- colMeans(se)
    names(means) <- names(data.predicted)
    return(means)
  }
  mean(se)
}

#' RMSE caluclator
Evaluation.root_mean_squared_error <- function(data.observed, data.predicted) {
  mse <- Evaluation.mean_squared_error(data.observed = data.observed, data.predicted = data.predicted)

  # Calculate the root of the mse
  sqrt(mse)
}
