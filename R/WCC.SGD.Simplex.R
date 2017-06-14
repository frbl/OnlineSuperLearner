#' WCC.SGD.Simplex
#' This is the SGD computer used in the Online SL package by David Benkeser; https://github.com/benkeser/onlinesl/
#'
#' @docType class
#' @importFrom R6 R6Class
WCC.SGD.Simplex <- R6Class("WCC.SGD.Simplex",
  inherit = WeightedCombinationComputer,
  public =
    list(
      initialize = function(weights.initial, step_size = 0.01, auto_update_stepsize = FALSE) {
        if(is.null(weights.initial)){
          throw('Please provide initial weights (or NA vector with the correct size)')
        }
        weights.initial <- runif(length(weights.initial), 0, 1)
        weights.initial <- weights.initial / sum(weights.initial)
        super$initialize(weights.initial)

        private$auto_update_stepsize <- Arguments$getLogical(auto_update_stepsize)
        private$step_count <- 0
        if (!private$auto_update_stepsize) {
          private$step_size <- Arguments$getNumerics(step_size, c(0.000001, Inf))
        }
      }
    ),
  active =
    list(
    ),
  private =
    list(
      step_size = NULL,
      step_count = NULL,
      auto_update_stepsize = NULL,
      compute = function(Z, Y, libraryNames) {

        grad <- - t(Z) %*% (Y - Z %*% private$weights)

        new_weights <- private$weights - private$get_step_size * grad
        names(new_weights) <- libraryNames

        # Normalize the weights using the L1 simplex
        new_weights_normalized <- private$project_to_l1_simplex(new_weights)
        private$weights <- new_weights
      },

      get_step_size = function() {
        if (private$auto_update_stepsize) {
          private$step_size <- 1 / private$increase_and_return_counter()
        } 

        private$step_size
      },

      increase_and_return_counter = function() {
        private$step_count <- private$step_count + 1
        private$step_count
      },
      
      project_to_l1_simplex = function(weights) {
        sorted_weights <- sort(weights, decreasing = TRUE)
        cumulative_sum_of_weights <- cumsum(sorted_weights) - 1
        sequence_weights <- seq(length(weights))
        
        # By dividing the cumulative sum of weights (which was sorted), the lower weights will be
        # divided by a larger number 
        # (resulting in a larger numbers at the end if < 1 , lower numbers at the end if > 1)
        reweighted_cumulative_sum_of_weights <- cumulative_sum_of_weights / sequence_weights

        K <- sorted_weights - reweighted_cumulative_sum_of_weights %>%
          which(. > 0) %>%
          rev(.)[1]

        tau <- (sum(sorted_weights[seq(K)]) - 1) / K

        w_pos <- weights - tau
        ifelse(w_pos >= 0, w_pos, 0) %>%
          return
      }
    )
)

