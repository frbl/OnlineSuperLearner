#' ML.H2O.glm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @export
ML.H2O.glm <-
  R6Class (
           "ML.H2O.glm",
           inherit = ML.H2O,
           public =
             list(
                  nfolds = NULL,
                  alpha = NULL,
                  family = NULL,

                  initialize = function(nfolds = 10, alpha = 0.5, family = 'gaussian') {
                    super$initialize()
                    self$nfolds <- nfolds
                    self$alpha <- alpha
                    self$family <- family
                  },

                  fit = function(train, test, Y, A, W){
                    # TODO:! This is teribly inefficient and is merely for testing
                    train.hex <- as.h2o(train, key="train.hex")
                    test.hex <- as.h2o(test, key="test.hex")

                    self$model <- h2o.glm(x = X, y = y,
                                          training_frame = train.hex,
                                          validation_frame = test.hex,
                                          family = self$family,
                                          nfolds = self$nfolds,
                                          alpha = self$alpha)
                    h2o.cross_validation_predictions(self$model)
                  }
                  )
           )
