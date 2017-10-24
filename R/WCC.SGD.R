#' WCC.SGD
#' Stochastic gradient descent optimizer, based on the R sgd package
#'
#' @docType class
#' @include WeightedCombinationComputer.R
#' @importFrom R6 R6Class
#' @import sgd
WCC.SGD <- R6Class("WCC.SGD",
  inherit = WeightedCombinationComputer,
  public =
    list(
        initialize = function(weights.initial) {
          if(is.null(weights.initial)){
            throw('Please provide initial weights (or NA vector with the correct size)')
          }
          weights.initial <- runif(length(weights.initial), 0, 1)
          weights.initial <- weights.initial / sum(weights.initial)
          super$initialize(weights.initial)
        },

        compute = function(Z, Y, libraryNames) {
          dat <- data.frame(y=Y, x=Z)

          start <- private$weights
          if(!is.null(private$sgd_weights)) {
            start <- private$sgd_weights$coefficients
          } 

          colnames(dat) <- c('y', libraryNames)
          # Compute the best convex combination
          private$sgd_weights <- sgd(y ~ 0 + ., dat, model="lm",
                                sgd.control = list(
                                  method="implicit",
                                  start = start
                                  )
                                )

          private$weights <- exp(as.vector(private$sgd_weights$coefficients))

          # Simple scaling between 0 and 1
          # We could (and probably should) do this better. Either in the SGD algorithm place the
          # restrictions on the thetas, or do here some proper min / max scaling. The fact why I
          # did it this way is because I wanted to scale everything between 0-1, and sum to 1, but not
          # set the lowest values (close) to zero.
          private$weights <- private$weights / sum(private$weights)
        }

        ),
  active =
    list(
        ),
  private =
    list(
        sgd_weights = NULL
    )
)
