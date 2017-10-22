#' WCC.SGD.Simplex
#' This is the SGD computer used in the Online SL package by David Benkeser;
#' https://github.com/benkeser/onlinesl/. It performs a gradient descent update
#' (or a number of gradient descent updates) using the estimates of the
#' separate estimators.
#'
#' @docType class
#' @importFrom R6 R6Class
WCC.SGD.Simplex <- R6Class("WCC.SGD.Simplex",
  inherit = WeightedCombinationComputer,
  public =
    list(
      initialize = function(weights.initial, step_size = 0.0001, auto_update_stepsize = TRUE, iterations = 1000) {
        if(is.null(weights.initial)){
          throw('Please provide initial weights (or NA vector with the correct size)')
        } else if (!all(is.na(weights.initial))) {
          warning('The weights provided will be overridden by a random vector! If you dont want this message, please provide a vector (of the correct size) with NAs')
        }
        weights.initial <- runif(length(weights.initial), 0, 1)
        weights.initial <- weights.initial / sum(weights.initial)
        super$initialize(weights.initial)

        private$auto_update_stepsize <- Arguments$getLogical(auto_update_stepsize)
        private$step_count <- 0

        private$step_size <- Arguments$getNumerics(step_size, c(0.0000000001, Inf))
        private$iterations <- Arguments$getInteger(iterations, c(1, Inf))
      }
    ),
  active =
    list(
      get_step_size = function() {
        if (private$auto_update_stepsize) {
          private$step_size <- 1 / private$increase_and_return_counter()
        } 

        private$step_size
      }
    ),
  private =
    list(
      iterations = NULL,
      step_size = NULL,
      step_count = NULL,
      auto_update_stepsize = NULL,

      # This function performs several iterations (\code{private$iterations}) steps of gradient descent
      # on the weight vector in \code{private$weights}.
      # @param Y the true outcomes
      # @param Z the estimated outcomes by each of the estimators
      # @param libraryNames the names of the libraries for each of the weights
      # @return a vector of new weights after \code{private$iterations} iterations
      compute = function(Z, Y, libraryNames) {
        current_weights <- self$get_weights
        names(current_weights) <- libraryNames
        #df <- data.table(t(current_weights))
        for (i in seq(private$iterations)) {
          current_weights <- private$get_updated_weights(Z=Z, Y=Y, libraryNames = libraryNames, current_weights = current_weights)
          #df <- rbind(df, t(current_weights))
        }
        #plot.new()
        #plot(df$a, ylim=range(c(0, 1)))
        #par(new=TRUE)
        #plot(df$b, ylim=range(c(0, 1)), col="red", axes = FALSE, xlab = "", ylab = "")
        #par(new=TRUE)
        #plot(df$c, ylim=range(c(0, 1)), col="orange", axes = FALSE, xlab = "", ylab = "")
        #par(new=TRUE)
        #plot(df$d, ylim=range(c(0, 1)), col="purple", axes = FALSE, xlab = "", ylab = "")
        private$weights <- current_weights
      },

      # This function performs one step of gradient descent on the weight vector provided to it. 
      # After computing these new weights, it projects them onto the L1 simplex, scaling them between
      # 0 and 1 and letting them sum to 1
      # @param Y the true outcomes
      # @param Z the estimated outcomes by each of the estimators
      # @param libraryNames the names of the libraries for each of the weights
      # @param current_weights the weights we currently estimated for our estimators
      # @param Y The outcome at iteration t
      # @return a vector of new weights
      get_updated_weights = function(Z, Y, libraryNames, current_weights) {
        gradient <- private$compute_gradient(Z = Z, Y = Y, current_weights = current_weights)
        new_weights <- current_weights - self$get_step_size * gradient

        names(new_weights) <- libraryNames
        # Normalize the weights using the L1 simplex
        private$project_to_l1_simplex(new_weights)
      },

      increase_and_return_counter = function() {
        private$step_count <- private$step_count + 1
        private$step_count
      },

      # This function computes the gradient of the current weights provided to it.
      # @param Y the true outcomes
      # @param Z the estimated outcomes by each of the estimators
      # @param current_weights the weights we currently estimated for our estimators
      # @return the gradient to be applied to the weights
      compute_gradient = function(Z, Y, current_weights){
        current_prediction <- Z %*% current_weights
        grad <- - t(Z) %*% (Y - current_prediction) %>%
          as.vector
      },
      

      # Function to project a vector of weights (weights) to the  L1-simplex. Essentially, this function
      # makes sure that all values aree scaled between 0 and 1
      # @param weights the vector of weights to be projected
      # @return the vector projected onto L1 simplex
      project_to_l1_simplex = function(weights) {
        sorted_weights <- sort(weights, decreasing = TRUE)
        sequence_weights <- weights %>% 
          length %>% 
          seq

        cumulative_sum_of_weights <- cumsum(sorted_weights) - 1
        
        # By dividing the cumulative sum of weights (which was sorted), the lower weights will be
        # divided by a larger number 
        # (resulting in a larger numbers at the end if < 1 , lower numbers at the end if > 1)
        reweighted_cumulative_sum_of_weights <- cumulative_sum_of_weights / sequence_weights

        reweighted_cumulative_sum_of_weights

        K <- sorted_weights - reweighted_cumulative_sum_of_weights
        K <- which(K > 0)
        K <- rev(K)[1]

        tau <- (sum(sorted_weights[seq(K)]) - 1) / K

        w_pos <- weights - tau
        ifelse(w_pos >= 0, w_pos, 0) %>%
          return
      }
    )
)

