#' Returns the correct evaluation function given the family of the data
#' # @import Metrics
#' @param family the family of the data (binomial for binary, gaussian for cts)
#' @param useAsLoss should we use the loss function or the performance function?
#' @export
Evaluation.get_evaluation_function = function(family, useAsLoss=TRUE) {
  if(!useAsLoss){
    #return(Evaluation.log_likelihood_loss)
    ###if (family == 'gaussian') {
      return(Evaluation.log_loss)
    ###} else if(family == 'binomial') {
      ###return(Evaluation.log_loss)
    ###} else {
      ###throw('No loss function implemented for family ', family)
    ###}
  }
  return(Evaluation.log_loss)

  ##if (family == 'gaussian') {
    ##return(Evaluation.root_mean_squared_error)
  ##} else if(family == 'binomial') {
    ##return(Evaluation.accuracy)
  ##} else {
    ##throw('No evaluation measure implemented for family ', family)
  ##}
}

#' Accuracy caluclator
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator 
Evaluation.accuracy <- function(data.observed, data.predicted) {
  boolean.predicted <- as.numeric(data.predicted >= 0.5)
  boolean.observed <- as.numeric(data.observed >= 0.5)
  if (is.a(boolean.predicted, 'matrix')) {
    means <- colMeans(as.numeric(boolean.observed == boolean.predicted))
    names(means) <- names(data.predicted)
    return(means)
  }
  c(accuracy = mean(as.numeric(boolean.observed == boolean.predicted)))
}

#' Log loss evaluation metric
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator 
#' @param eps is a small offset to let the log not go to Inf
Evaluation.log_loss <- function(data.observed, data.predicted, eps = 1e-15) {
  data.predicted = pmin(pmax(data.predicted, eps), 1-eps)
  res <- c(log_loss = -mean(data.observed * log(data.predicted) + (1 - data.observed) * log(1 - data.predicted)))
  #if(is.na(res)) browser()
  res
}

#' Log likelihood loss evaluation metric
#' @param data.observed is unused. It is included in the function to comply to the general 
#' @param data.predicted the Y outcome from the estimator 
#' @param eps is a small offset to let the log not go to Inf
Evaluation.log_likelihood_loss <- function(data.observed = NULL, data.predicted, eps = 1e-15) {
  data.predicted = pmax(data.predicted, eps)
  c(log_likelihood_loss = - sum(log(data.predicted)))
}

#' Mean squared error loss
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator 
Evaluation.mse_loss <-  function(data.observed, data.predicted) {
  c(mse_loss = mean((data.predicted - data.observed)^2))
}

#' MSE caluclator
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator 
Evaluation.mean_squared_error <- function(data.observed, data.predicted) {
  ## Calculate the MSE
  se <- Evaluation.mse_loss(data.predicted, data.observed)
  if (is.a(data.predicted, 'matrix')) {
    means <- colMeans(se)
    names(means) <- names(data.predicted)
    return(means)
  }
  c(mse = mean(se))
}

#' RMSE caluclator
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator 
Evaluation.root_mean_squared_error <- function(data.observed, data.predicted) {
  mse <- Evaluation.mean_squared_error(data.observed = data.observed, data.predicted = data.predicted)

  ## Calculate the root of the mse
  c(r = sqrt(mse))
}
