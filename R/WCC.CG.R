#' WCC.CG
#' Constrained descent optimizer
#'
#' @docType class
#' @include WeightedCombinationComputer.R
#' @importFrom R6 R6Class
#' @import sgd
WCC.CG <- R6Class("WCC.CG",
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

          colnames(dat) <- c('y', libraryNames)
          # Compute the best convex combination

          private$weights <- 0 ## PERFORM THE COMPUTATION

        ),
  active =
    list(
        ),
  private =
    list(
        sgd_weights = NULL
    )
)
