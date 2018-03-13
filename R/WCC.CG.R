#' WCC.CG
#' Constrained descent optimizer
#'
#' @docType class
#' @include WeightedCombinationComputer.R
#' @importFrom R6 R6Class
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(weights.initial)}}{
#'    Creates a new computer for determining the best weighted combination of
#'    the ML libraries based on stochastic gradient descent and simplex
#'    projection.
#'    @param weights.initial vector (default = NULL) the initial vector of
#'     weights to use. Can be NULL if the number_of_algorithms is specified.
#'
#'    @param number_of_algorithms integer (default = NULL) the number of
#'    algorithms to calculate the weights for. Is used to initialize the
#'    weights. Can be NULL if the weights are provided
#'   }
#'
#'   \item{\code{compute(Z, Y, libraryNames, ...)}}{
#'    Method to compute the best weighted combination for a set of estimators.
#'    In this implementation we use a two-step approach, in which we first
#'    estimate the weights based on a gradient descent procedure, and then
#'    project these weights back to the L1 simplex, scaling them between 0-1.
#'
#'    @param Z matrix containing the outcomes of each of the estimators
#'
#'    @param Y vector containing the actual observed outcome.
#'
#'    @param libraryNames vector containing the names of the optimizer.
#'
#'    @param ... other parameters to pass to the underlying combination
#'     computers.
#'
#'    @return vector of the trained / updated weights.
#'   }
#' }
WCC.CG <- R6Class("WCC.CG",
  inherit = WeightedCombinationComputer,
  public =
    list(
        initialize = function(weights.initial = NULL, number_of_algorithms = NULL ) {
          if(is.null(weights.initial) && is.null(number_of_algorithms)){
            throw('Please provide initial weights (or number_of_algorithms with the number of alphas to estimate)')
          }
          if(is.null(weights.initial)) {
            weights.initial <- rep(1 / number_of_algorithms, number_of_algorithms)
          }

          super$initialize(weights.initial)
        },

        compute = function(Z, Y, libraryNames, ...) {
          ## Using the definition of eta of the OCO Book
          ## See: page 44: http://www.nowpublishers.com/article/Details/OPT-013
          eta <- 1 / (2 * nrow(Z) * sqrt(self$get_step_count))
          current_alpha <- self$get_weights

          ## Using algorithm 6 of the OCO Book,
          ## See: page 43 http://www.nowpublishers.com/article/Details/OPT-013

          ## 1. Play xt and observe ft (prediction).
          prediction <- Z %*% current_alpha

          ## SGD Method
          ## 2. yt+1 = xt - eta t nabla ft(xt)
          gradient <- -2 * t(Y - prediction) %*% Z 
          tentative_alpha <- current_alpha - eta * gradient %>% as.vector

          ## 3. Project to the simplex
          updated_alpha <- private$project_to_l1_simplex(tentative_alpha)
          names(updated_alpha) <- c(libraryNames)
          updated_alpha
          ## output has to be a vector
          private$weights <- updated_alpha
        }
      ),
  active =
    list(
        ),
  private =
    list(
      # Function to project a vector of alpha (alpha) to the  L1-simplex.
      # Essentially, this function makes sure that all values aree scaled
      # between 0 and 1
      # @param alpha the vector of alpha to be projected
      # @return the vector projected onto L1 simplex
      project_to_l1_simplex = function(alpha) {
        ## Partly based on https://gist.github.com/daien/1272551
        ## Check if we are already in the simplex
        if(sum(alpha) == 1 && all(alpha >= 0)) { return(alpha) }

        sorted_alpha <- sort(alpha, decreasing = TRUE)
        sequence_alpha <- alpha %>% 
          length %>% 
          seq

        cumulative_sum_of_alpha <- cumsum(sorted_alpha) - 1
        
        # By dividing the cumulative sum of alpha (which was sorted), the lower alpha will be
        # divided by a larger number 
        # (resulting in a larger numbers at the end if < 1 , lower numbers at the end if > 1)
        reweighted_cumulative_sum_of_alpha <- cumulative_sum_of_alpha / sequence_alpha

        K <- sorted_alpha - reweighted_cumulative_sum_of_alpha
        K <- which(K > 0)
        K <- rev(K)[1]

        tau <- (sum(sorted_alpha[seq(K)]) - 1) / K

        w_pos <- alpha - tau
        ifelse(w_pos >= 0, w_pos, 0) %>%
          return
      }
    )
)
