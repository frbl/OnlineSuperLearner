#' WeightedCombinationComputer
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{
#'    Creates a new computer for determining the best weighted combination of the ML libraries.
#'    \code{weigths.initial} vector containing the initial weights
#'   }
#'   \item{\code{getWeigths()}}{
#'    Returns the current list of optimal weights (or the initial weights, if not yet fitted)
#'   }
#'   \item{\code{compute(Z, Y, libraryNames)}}{
#'    Method to compute the best weighted combination of the underlying estimators
#'    \code{Z} matrix containing the outcomes of each of the estimators
#'    \code{Y} vector containing the actual outcome
#'    \code{libraryNames} vector containing the names of the estimators
#'   }
#' }
WeightedCombinationComputer <- R6Class("WeightedCombinationComputer",
  private =
    list(
        weights = NULL,

        compute = function(Z, Y, libraryNames ) {
          throw('This method is not implemented, please inherit this class and implement it.')
        }
        ),
  active = 
      list(
        get_weights = function() {
          return(private$weights)
        }
        ),
  public =
    list(
        initialize = function(weights.initial) {
          private$weights <- Arguments$getNumerics(weights.initial, c(0, 1))
          sum_of_weights <- sum(private$weights)
          if (sum_of_weights != 1) {
            throw("The sum of the initial weights, ", sum_of_weights, ", does not equal 1")
          }
        },

        process = function(Z, Y, libraryNames, ...) {
          if(!is.character(libraryNames)) throw('libraryNames should be a vector of names.')
          browser()

          Z <- Arguments$getInstanceOf(as.matrix(Z), 'matrix') 
          Y <- Arguments$getInstanceOf(as.matrix(Y), 'matrix') 

          if (length(private$weigths) == 1) {
            private$weights <- c(1)
            return(private$weights)
          }

          if (length(private$weights) != length(libraryNames)) {
            throw('Not each estimator has exactly one weight: estimators: ', length(libraryNames) ,' weights: ',length(private$weights),'.')
          }
          libraryNames <- Arguments$getCharacters(libraryNames)

          # Call the subclass
          private$compute(Z, Y, libraryNames, ...)
          return(private$weights)
        }

    )
)
