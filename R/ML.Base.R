#' Base class for any machine learning model. Extend this class if you want to create a new machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @field model the most recent / best model fitted.
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{This method is used to create object of this class. }
#'
#'   \item{\code{fit(data, X, Y)}}{Method to fit the current machine learning model. Should be overridden in the subclass.}
#'   \item{\code{predict(data, X)}}{Method to predict using the current model.}
#'   \item{\code{getModel()}}{Getter for the latest model.}
#'}
ML.Base <-
  R6Class (
           "ML.Base",
           public =
             list(
                  model = NULL,

                  initialize = function() {
                  },

                  fit = function(data, X, Y) {
                    throw('The fit method needs to be inherited')
                  },

                  predict = function(data, X) {
                    if (is.null(self$model)) {
                     throw('Train the model first')
                    }
                    pred <- predict(self$model, as.matrix(data[, X, with = FALSE]))
                  },

                  getModel = function() {
                    return(self$model)
                  }
                  )
           )
