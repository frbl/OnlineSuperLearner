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

                  initialize = function(nfolds = 1, alpha = 0.5, family = 'gaussian') {
                    super$initialize()
                    self$nfolds <- nfolds
                    self$alpha <- alpha
                    self$family <- family
                  },

                  fit = function(train, Y, A, W){
                    X <- c(A, W)
                    self$model <- h2o.glm(x = X, y = Y,
                                          training_frame = train,
                                          family = self$family,
                                          nfolds = self$nfolds,
                                          alpha = self$alpha,
                                          checkpoint = private$getCheckpoint()
                                          )

                  }
                  )
           )
