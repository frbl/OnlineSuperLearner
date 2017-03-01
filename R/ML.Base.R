#' Base class for any machine learning model. Extend this class if you want to create a new machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @field model the most recent / best model fitted.
#' @field iteration the iteration we are currently on. This is needed for calculating the average cv risk score
#' @field CV.risk the calculated crossvalidated risk of the estimator
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
           private =
             list(
                  data.previous = NULL
                  ),
           public =
             list(
                  model = NULL,

                  initialize = function() {
                  },

                  process = function(train, test, Y, A, W) {
                    # DO NOT OVERRIDE THIS FUNCTION!
                    private$data.previous <- test

                    # This function delegates the call to its subclass
                    self$fit(train, Y, A, W)

                    # Predict the outcome on the testset
                    self$predict(test, A, W)
                  },

                  fit = function(train, Y, A, W) {
                    throw('The fit method needs to be inherited')
                  },

                  predict = function(data, A, W) {
                    warning('You are using the base predict function, you\'d probably want to inherit and override this')
                    if (is.null(self$model)) {
                      throw('Train the model first')
                    }
                    X <- c(A, W)
                    pred <- predict(self$model, as.matrix(data[, X, with = FALSE]))
                  }
                  )
           )
