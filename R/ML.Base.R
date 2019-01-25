#' ML.Base
#'
#' Base class for any machine learning model. Extend this class if you want to
#' create a new machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize() }}{ 
#'     Method used to create object of this class. 
#'   } 
#' 
#'   \item{\code{process(X_mat, Y_vals, X_mat_test, Y_vals_test) }}{ 
#'     <<description>> 
#'     @param X_mat 
#'     @param Y_vals 
#'     @param X_mat_test 
#'     @param Y_vals_test  
#'   } 
#' 
#'   \item{\code{perform_prediction(...) }}{ 
#'     Method to perform a prediction using the current machine learning model.
#'     Note that this function is for testing purposes only. This function
#'     calls the private \code{do.predict} function. Each ML.Base model should
#'     have a private \code{do.predict} function, which is called by the
#'     condensier package. The params for this private function are described
#'     below.
#'
#'     @param X_mat matrix a matrix of X variables / independent variables to
#'      do the prediction with.
#'
#'     @param m.fit fitted machine learning model. The actual model (NOT A
#'      ML.BASE MODEL) to run the prediction from.
#'
#'     @return vector the predicted probabilities from the provided m.fit model.
#'   } 
#' 
#'   \item{\code{perform_fit(...) }}{ 
#'     Method to fit the current machine learning model. Note that this
#'     function is for testing purposes only. This function calls the private
#'     \code{do.fit} function. Each ML.Base model should have a private
#'     \code{do.fit} function, which is called by the condensier package. The
#'     params for this private function are described below.
#'
#'     @param X_mat matrix a matrix of X values / independent variables
#'
#'     @param Y_vals vector a vector with the dependent variable
#'
#'     @return a fitted instance of the actual machine learning model
#'   } 
#' 
#'   \item{\code{perform_update(...) }}{ 
#'     Method to perform an update of the provided machine learning model.
#'     Note that this function is for testing purposes only. This function
#'     calls the private \code{do.update} function. Each online ML.Base model
#'     should have a private \code{do.update} function, which is called by the
#'     condensier package. The params for this private function are described
#'     below.
#'
#'     @param X_mat matrix a matrix of X variables / independent variables to
#'      do the prediction with.
#'
#'     @param Y_vals vector a vector with the dependent variable
#'
#'     @param m.fit fitted machine learning model. The actual model (NOT A
#'      ML.BASE MODEL) to perform the update on.
#'
#'     @return an updated instance of the actual machine learning model
#'   } 
#' 
#'   \item{\code{create_formula(X, Y='Y') }}{ 
#'     Small helper function to create a formula notation of a dependent
#'     variable ~ independent variables. Useful when the submodel accepts
#'     this format for specifying a model and the outcome Y.
#'
#'     @param X vector a vector of X variables / independent variable names
#'
#'     @param Y string (default = "Y") the name of the dependent variable
#'   } 
#' }  
ML.Base <- R6Class("ML.Base",
  inherit = condensier::logisfitR6,
  public =
    list(
      fitfunname='ml.base',
      lmclass='ML.BaseR6',

      initialize = function() {
        super$initialize()
      },

      process = function(X_mat, Y_vals, X_mat_test, Y_vals_test) {
        throw('Deprecated!')
      },

      # Functions for testing only
      perform_prediction = function(...) {
        private$do.predict(...)
      },

      perform_fit = function(...) {
        private$do.fit(...)
      },

      perform_update = function(...) {
        private$do.update(...)
      },

      create_formula = function(X, Y = 'Y', intercept = FALSE, force_intercept_removal = FALSE) {
        form <- paste(Y, "~")
        if (intercept) {
          form <- paste(form, "Intercept + ")
        } else if(force_intercept_removal) {
          form <- paste(form, "0 + ")
        }

        as.formula(paste(form, paste(X, collapse = " + ")))
      }
    ),
  active =
    list(
      ),
  private =
    list(
      do.fit = function(X_mat, Y_vals, ...) {
        throw('The fit method needs to be inherited')
      }
    )
)

