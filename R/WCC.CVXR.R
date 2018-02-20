#' WCC.CVXR
#'
#' @docType class
#' @importFrom CVXR Variable Problem Minimize sum_squares
#' @include WeightedCombinationComputer.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{
#'   }
#'   \item{\code{getWeigths()}}{
#'   }
#'   \item{\code{compute(Z, Y, libraryNames, weights.initial)}}{
#'   }
#' }
WCC.CVXR <- R6Class("WCC.CVXR",
  inherit = WeightedCombinationComputer,
  private =
    list(),
  active =
    list(),
  public =
    list(
        initialize = function(weights.initial) {
          super$initialize(weights.initial)
        },

        compute = function(Z, Y, libraryNames ) {
          k = length(self$get_weights)
          alpha <- Variable(k)
          objective <- Minimize(sum_squares(Y - Z %*% alpha))

          ## Constraing according to our sigma
          ## \Sigma_k = \{ \alpha \in \RR_+^k : \sum_{k=1}^K \alpha = 1 \}
          constraints <- list(alpha > 0, sum(alpha) == 1)

          optimization_problem <- Problem(objective, constraints)
          optimization_solution <- solve(optimization_problem)

          if (optimization_solution$status != 'optimal') {
            warning('The optimization problem was not solved optimally!')
          }

          ## Retrieve the actual alphas from the solved system
          weights <- optimization_solution$getValue(alpha)

          ## Online update of the weights. Note that this is different from
          ## Benkeser2017, but I believe this is better. (We also multiply the
          ## original weights).
          ## We might want to move this to the super class.
          weights <- self$get_current_nobs / (self$get_current_nobs + 1) * self$get_weights + 
                     1 / (self$get_current_nobs + 1) * weights %>% as.vector

          self$set_weights(weights)
          self$get_weights
        }
    )
)
