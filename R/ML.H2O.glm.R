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

                  initialize = function(data, nfolds = 10, alpha = 0.5, family = 'binomial') {
                    super$initialize(data = data)
                    self$nfolds <- nfolds
                    self$alpha <- alpha
                    self$family <- family
                  },

                  fit = function(X, y){
                    h2o.glm(x = X, y = y,
                            training_frame = self$data,
                            family = self$family,
                            nfolds = self$nfolds,
                            alpha = self$alpha)
                  }
                  )
           )
