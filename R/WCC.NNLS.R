#' WCC.NNLS
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom nnls nnls
#' @include WeightedCombinationComputer.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{
#'    Creates a new computer for determining the best weighted combination of the ML libraries.
#'    currently this is merely the NNLS version from the general superlearner, but in the future
#'    it should be updated to be 1. online, and 2. using better algorithm (like NLopts)
#'    \code{obsWeights} vector containing the initial weights
#'   }
#'   \item{\code{getWeigths()}}{
#'    Returns the current list of optimal weights (or the initial weights, if not yet fitted)
#'   }
#'   \item{\code{compute(Z, Y, libraryNames, obsWeights)}}{
#'    Method to compute the best weighted combination of the underlying estimators
#'    \code{Z} matrix containing the outcomes of each of the estimators
#'    \code{Y} vector containing the actual outcome
#'    \code{libraryNames} vector containing the names of the estimators
#'   }
#' }
WCC.NNLS <-
  R6Class (
           "WCC.NNLS",
           inherit = WeightedCombinationComputer,
           private =
            list(
                ),
           active = 
             list(),
           public =
             list(
                  initialize = function(obsWeights) {
                    super$initialize(obsWeights)
                  },

                  compute = function(Z, Y, libraryNames ) {
                    # Compute the best convex combination
                    weights <- coef(nnls(Z,Y))

                    if(all(weights == 0)) {
                      warning('All algorithms have zero weight')
                      return(weights)
                    } 

                    # Normalize the weights and store them for later use
                    private$obsWeights <- weights/sum(weights)
                    private$obsWeights
                  }
                  )
           )
