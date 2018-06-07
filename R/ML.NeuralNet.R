#' ML.NeuralNet
#'
#' Class to create neuralnetwork machine learning models. Information from the
#' package:
#' Training of neural networks using backpropagation, resilient backpropagation
#' with (Riedmiller, 1994) or without weight backtracking (Riedmiller and
#' Braun, 1993) or the modified globally convergent version by Anastasiadis et
#' al.  (2005). The package allows flexible settings through custom-choice of
#' error and activation function. Furthermore, the calculation of generalized
#' weights (Intrator O & Intrator N, 1993) is implemented.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom neuralnet neuralnet compute
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(hidden = c(1, 3)) }}{ 
#'     Initializes a new neuralnet estimator with the provided hidden layers. 
#'
#'     @param hidden vector (default = c(1,3)) a vector specifying the hidden
#'      layers in the neural network. See the documentation of the neural
#'      network package for more details.
#'   } 
#' }  
#' @export
ML.NeuralNet <- R6Class("ML.NeuralNet",
  inherit = ML.Base,
  public =
    list(
      fitfunname='neural-net',
      lmclass='neural-net',
      initialize = function(hidden=c(1,3), stepmax=1e6) {
        private$hidden <- hidden
        private$stepmax <- stepmax
      }
    ),
  active =
    list(
      get_file_name = function() {
        return(private$file_name)
      }
    ),
  private =
    list(
      stepmax = NULL,
      hidden = NULL,
      file_name = file.path('output', 'model_NN.rds'),

      do.fit = function(X_mat, Y_vals, save_model = FALSE, coef = NULL) {
        formula <- self$create_formula(colnames(X_mat))
        data = cbind(X_mat, Y = Y_vals) 
        fitted_model <- neuralnet(formula, data = data, hidden = private$hidden,
                                  linear.output=FALSE,
                                  startweights = coef,
                                  stepmax = private$stepmax)

        if (save_model) {
          private$save_model(model = fitted_model)
        }

        return(fitted_model)
      },

      do.update = function(X_mat, Y_vals, save_model = FALSE,  m.fit = NULL, ...) {
        # By default the neuralnet function uses the old model as a parameter.
        # Therefore we can just simply call the fit function,if m.fit is null
        # then look for a saved model
        if (is.null(m.fit)){
          m.fit <- private$read_model()
        }

        fitted_model <- private$do.fit(X_mat, Y_vals, 
                                      save_model = save_model,
                                      coef = m.fit$coef$weights)

        if (save_model) {
          private$save_model(model = fitted_model)
        }

        if(is.null(fitted_model$weights)){
          browser()
        }

        
        return(fitted_model)
      },

      do.predict = function(X_mat, m.fit = NULL) {
        if (is.null(m.fit)){
          m.fit <- private$read_model()
        }
          
        result <- compute(m.fit$coef, X_mat)

        return(result$net.result)
      },

      save_model = function(model) {
        saveRDS(model, self$get_file_name)
      },

      read_model = function() {
        readRDS(self$get_file_name)
      }
    )
)
