#' WeightedCombinationComputer
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(weights.initial)}}{
#'    Creates a new computer for determining the best weighted combination of
#'    the ML libraries.  \code{weigths.initial} vector containing the initial
#'    weights.
#'    @param weights.initial vector the initial vector of weights to use.
#'   }
#'
#'   \item{\code{compute(Z, Y, libraryNames, ...)}}{
#'    Method to compute the best weighted combination in an underlying
#'    optimizer. Note that by default this method is not implemented, and
#'    should be implemented by one of the subclasses. 
#'
#'    @param Z matrix containing the outcomes of each of the estimators.
#'
#'    @param Y vector containing the actual observed outcomes.
#'
#'    @param libraryNames vector containing the names of the estimators.
#'
#'    @param ... other parameters to pass to the underlying combination
#'     computers.
#'   }
#'
#'   \item{\code{process(Z, Y, libraryNames, ...)}}{
#'    Method to compute the best weighted combination of the underlying
#'    optimizer. This function internally calls the extended \code{compute}
#'    function.
#'
#'    @param Z matrix containing the outcomes of each of the estimators.
#'
#'    @param Y vector containing the actual observed outcome.
#'
#'    @param libraryNames vector containing the names of the estimators.
#'
#'    @param ... other parameters to pass to the underlying combination
#'     computers.
#'
#'    @return vector of the trained / updated weights.
#'   }
#'
#'   \item{\code{get_weights}}{
#'    Active Method. Returns the current vector of optimal weights (or the
#'    initial weights, if not yet fitted)
#'
#'    @return vector the current vector of weights
#'   }
#'
#'   \item{\code{get_historical_weights()}}{
#'    Active Method. Returns the history of the alphas, and shows how they were
#'    updated over time.
#'
#'    @return data.frame a data.frame containing the alphas. One row for each
#'    iteration, one column for each algorithm.
#'   }
#'
#'   \item{\code{get_step_count}}{
#'    Active Method. Returns the current step count of the algorithm. This is
#'    updated on every \code{process} call.
#'
#'    @return integer the current stepcount
#'   }
#' }
WeightedCombinationComputer <- R6Class("WeightedCombinationComputer",
  public =
    list(
      initialize = function(weights.initial) {
        private$weights <- Arguments$getNumerics(weights.initial, c(0, 1))
        private$step_count <- 0

        private$historical_weights <- data.frame(t(weights.initial))
        sum_of_weights <- sum(private$weights)
        if (sum_of_weights != 1) {
          throw("The sum of the initial weights, ", sum_of_weights, ", does not equal 1")
        }
      },

      compute = function(Z, Y, libraryNames, ... ) {
        throw('This method is not implemented, please inherit this class and implement it.')
      },

      process = function(Z, Y, libraryNames, ...) {
        if(!is.character(libraryNames)) throw('libraryNames should be a vector of names.')

        Z %<>% as.matrix
        Y %<>% as.matrix
        
        if (length(self$get_weights) == 1) {
          private$weights <- c(1)
          return(self$get_weights)
        }

        if (length(private$weights) != length(libraryNames)) {
          throw('Not each estimator has exactly one weight: estimators: ', length(libraryNames) ,' weights: ',length(private$weights),'.')
        }

        libraryNames <- Arguments$getCharacters(libraryNames)

        ## Increase the counter
        private$increase_and_return_counter()

        ## Call the subclass
        self$compute(Z = Z, Y = Y, libraryNames = libraryNames, ...)
        private$historical_weights <- rbind(private$historical_weights, self$get_weights)
        return(self$get_weights)
      }
    ),
  active = 
    list(
      get_weights = function() {
        return(private$weights)
      },

      get_historical_weights = function() {
        return(private$historical_weights)
      },

      get_step_count = function() {
        return(private$step_count)
      }
    ),
  private =
    list(
      historical_weights = NULL,
      step_count = NULL,
      weights = NULL,

      increase_and_return_counter = function() {
        private$step_count <- private$step_count + 1
        private$step_count
      }
    )
)
