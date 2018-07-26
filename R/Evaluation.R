#' Evaluation
#'
#' Static class with multiple, different evaluation functions. These functions
#' can be used to either deterimine the loss of an estimator, or to determine
#' its performance.

#' Evaluation.get_evaluation_function
#'
#' Returns the correct evaluation function given the family of the data.
#' # @import Metrics
#' @param family the family of the data (binomial for binary, gaussian for cts).
#' @param useAsLoss should we use the loss function or the performance function?
#' @return function the log likelihoodloss function if \code{useAsLoss = TRUE}, and the log loss function otherwise.
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
  return(Evaluation.log_likelihood_loss)

  ##if (family == 'gaussian') {
  ##return(Evaluation.root_mean_squared_error)
  ##} else if(family == 'binomial') {
  ##return(Evaluation.accuracy)
  ##} else {
  ##throw('No evaluation measure implemented for family ', family)
  ##}
}

#' Evaluation.log_loss
#'
#' Function to calculate the log loss evaluation metric.
#' @param data.observed the true data (Y).
#' @param data.predicted the Y outcome from the estimator.
#' @param eps is a small offset to let the log not go to Inf.
#' @return double the calculated log loss.
#' @export
Evaluation.log_loss <- function(data.observed, data.predicted, eps = 1e-15) {
  eps <- Arguments$getNumerics(eps, c(1e-15, 1e-1))
  if (is(data.observed, 'numeric')) {
    Evaluation.log_loss_single_variable(data.observed = data.observed,
                                        data.predicted = data.predicted,
                                        eps = eps)
  } else {
    Evalutation.log_loss_multiple_learners(data.observed = data.observed,
                                           data.predicted = data.predicted,
                                           eps = eps)
  }
}

Evalutation.log_loss_multiple_learners <- function(data.observed, data.predicted, eps) {
  result <- lapply(names(data.predicted), function(algo_name) {
    current <- data.predicted[[algo_name]]
    outcomenames <- colnames(data.observed)
    tmp <- rep(NaN, length(outcomenames)) %>% t %>% as.data.table
    colnames(tmp) <- outcomenames
    for (col_name in outcomenames) {
      data.observed[, col_name, with=FALSE]
      tmp_res <- Evaluation.log_loss_single_variable(data.observed = data.observed[,col_name, with=FALSE][[1]],
                                          data.predicted = current[,col_name, with=FALSE][[1]],
                                          eps = eps)
      tmp[1,col_name] <- tmp_res %>% unname
      tmp
    }
    tmp
  })
  names(result) <- names(data.predicted)
  result
}

Evaluation.log_loss_single_variable <- function(data.observed, data.predicted, eps) {
  data.predicted = pmin(pmax(data.predicted, eps), 1-eps)
  res <- c(log_loss = sum(- data.observed * log(data.predicted) - (1 - data.observed) * log(1 - data.predicted)))
  #if(is.na(res)) browser()
  res
}

#' Evaluation.Accuracy
#'
#' Function to calculate the accuract of an estimator, given its estimates and
#' the observed outcomes.
#' @param data.observed the true data (Y).
#' @param data.predicted the Y outcome from the estimator .
#' @return double the calculated accuracy.
#' @export
Evaluation.accuracy <- function(data.observed, data.predicted) {
  boolean.observed <- as.numeric(data.observed >= 0.5)
  boolean.predicted <- as.numeric(data.predicted >= 0.5)
  c(accuracy = mean(as.numeric(boolean.observed == boolean.predicted)))
}

#' Evaluation.log_likelihood_loss
#'
#' Log likelihood loss evaluation metric
#' @param data.observed is unused. It is included in the function to comply to
#'  the general.
#' @param data.predicted the Y outcome from the estimator.
#' @param eps is a small offset to let the log not go to Inf.
#' @return double the calculated log likelihood loss.
#' @export
Evaluation.log_likelihood_loss <- function(data.observed = NULL, data.predicted, eps = 1e-15) {
  eps <- Arguments$getNumerics(eps, c(1e-15, 1e-1))

  # Make sure all data is bounded between eps and 1-eps
  data.predicted = pmax(data.predicted, eps)
  data.predicted = pmin(data.predicted, 1-eps)
  c(log_likelihood_loss = - sum(log(data.predicted)))
}



#' Evaluation.mse_loss
#'
#' Function to calculate the mean squared error loss
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator
#' @return double the calculated mean squared error loss
#' @export
Evaluation.mse_loss <-  function(data.observed, data.predicted) {
  data.predicted <- Arguments$getInstanceOf(data.predicted, 'numeric')
  c(mse_loss = mean((data.predicted - data.observed)^2))
}

#' Evaluation.mean_squared_error
#'
#' Function to calculate the mean squared error (uses the mse loss function internally).
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator
#' @return double the calculated MSE.
#' @export
Evaluation.mean_squared_error <- function(data.observed, data.predicted) {
  ## Calculate the MSE
  se <- Evaluation.mse_loss(data.predicted, data.observed)
  c(mse = mean(se))
}

#' Evaluation.root_mean_squared_error
#'
#' Function to calculate the root of the mean squared error.
#' @param data.observed the true data (Y)
#' @param data.predicted the Y outcome from the estimator
#' @return double the root of the mean squared error.
#' @export
Evaluation.root_mean_squared_error <- function(data.observed, data.predicted) {
  mse <- Evaluation.mean_squared_error(data.observed = data.observed, data.predicted = data.predicted)

  ## Calculate the root of the mse
  c(r = sqrt(mse))
}
