#' Evaluation.Accuracy caluclator
#'
#' @docType class
#' @importFrom R6 R6Class
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
