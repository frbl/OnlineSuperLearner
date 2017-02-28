#' WCC.NLopt
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom nloptr nloptr
#' @include WeightedCombinationComputer.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{
#'    Creates a new computer for determining the best weighted combination of the ML libraries. This
#'    class uses the NLopt backend for optimizing
#'    \code{lossFunction} function (optional) function to compute the loss of the OSL
#'    \code{lossFunctionGradient} function (optional) gradient of the function to compute the loss for the OSL
#'    \code{opts} list (optional) list with options for 
#'   }
#'   \item{\code{getWeigths()}}{
#'    Returns the current list of optimal weights (or the initial weights, if not yet fitted)
#'   }
#'   \item{\code{compute(Z, Y, libraryNames)}}{
#'    Method to compute the best weighted combination of the underlying estimators. The function will use the weights supplied to the constructor. If the model has been fitted before, it will reuse the weights of the previous run as the starting value for the current run.
#'    \code{Z} matrix containing the outcomes of each of the estimators
#'    \code{Y} vector containing the actual outcome
#'    \code{libraryNames} vector containing the names of the estimators
#'   }
#' }
WCC.NLopt <-
  R6Class (
           "WCC.NLopt",
           inherit = WeightedCombinationComputer,
           private =
             list(
                  lossFunction = NULL,
                  lossFunctionGradient = NULL,
                  opts = NULL,

                  compute = function(Z, Y, libraryNames, opts = NULL ) {
                    # Compute the best convex combination
                    result <- nloptr(x0=self$getWeights,
                                  eval_f=private$lossFunction,
                                  eval_grad_f = private$lossFunctionGradient,
                                  opts=private$opts,
                                  X = Z,
                                  Y = Y)

                    # Normalize the weights
                    private$weights <- (result$solution / sum(result$solution))
                  }
                  ),
           active = 
             list(),
           public =
             list(
                  initialize = function(weights.initial, lossFunction = NULL, lossFunctionGradient = NULL, opts = NULL) {
                    super$initialize(weights.initial)

                    # TODO: These functions are probably incorrect / inefficient currently. Update them with the correct functions.
                    if(is.null(lossFunction)){
                      lossFunction <- function(coeff, X, Y) {
                        pred <- as.matrix(X) %*% coeff
                        1/2 * mean((pred - Y)^2)
                      }
                    }

                    if(is.null(lossFunctionGradient)){
                      lossFunctionGradient <- function(coeff, X, Y) {
                        pred <- as.matrix(X) %*% coeff
                        t(X) %*% (1/length(Y) * (pred - Y))
                      }
                    }

                    if(is.null(opts)){
                      opts <- list("algorithm"="NLOPT_LD_LBFGS",
                                   "xtol_rel"=1.0e-8)
                    }

                    private$lossFunction <- lossFunction
                    private$lossFunctionGradient <- lossFunctionGradient
                    private$opts <- opts
                  }
                  )
           )
