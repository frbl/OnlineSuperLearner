#' ML.NeuralNet
#' Class to create neuralnetworks
#'
#' @docType class
#' @importFrom neuralnet neuralnet
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(hidden)}}{
#'     Initializes a new neuralnet estimator with the provided hidden layers. 
#'   }
#' }
#' @export
ML.NeuralNet <- R6Class("ML.NeuralNet",
  inherit = ML.Base,
  public =
    list(
      fitfunname='neural-net',
      lmclass='neural-net',
      initialize = function(hidden=c(1,3)) {
        private$hidden <- hidden
      }
    ),
  active =
    list(
    ),
  private =
    list(
      hidden = NULL,
      do.fit = function(X_mat, Y_vals, coef = NULL) {
        formula <- self$create_formula(X_mat)
        data = cbind(X_mat, Y = Y_vals) 
        neuralnet(formula, data = data, hidden = private$hidden, linear.output=FALSE, startweights = coef)
      },

      do.update = function(X_mat, Y_vals, m.fit, ...) {
        # By default the neuralnet function uses the old model as a parameter.
        # Therefore we can just simply call the fit function
        private$do.fit(X_mat, Y_vals, coef = m.fit$coef$weights)
      },

      do.predict = function(X_mat, m.fit) {
        if (any(is.na(m.fit$coef))) {
          result <- super$do.predict(X_mat, m.fit)
        } else {
          result <- compute(m.fit$coef, X_mat)
        }
        if(any(is.na(result)) || any(is.null(result))) browser()
        return(result)
      }
    )
)

