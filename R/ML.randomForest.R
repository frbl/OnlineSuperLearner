#' ML.randomForest
#' Class to create randomForest models
#'
#' @docType class
#' @importFrom randomForest randomForest
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{
#'     Initializes a new randomforest estimator. 
#'   }
#' }
#' @export
ML.randomForest <- R6Class("ML.randomForest",
  inherit = ML.Base,
  public =
    list(
      fitfunname='randomForest-local',
      lmclass='ML.randomForest',
      initialize = function(ntrees = 500) {
        private$ntrees <- Arguments$getInteger(ntrees, c(1, Inf))
      }
    ),
  active =
    list(
    ),
  private =
    list(
      ntrees = NULL,
      do.fit = function(X_mat, Y_vals) {
        randomForest(x = X_mat, y = as.factor(Y_vals), ntrees = private$ntrees)
      },

      do.predict = function(X_mat, m.fit) {
        if (any(is.na(m.fit$coef))) {
          result <- super$do.predict(X_mat, m.fit)
        } else {
          # Predicts a matrix with probabilities, get the second column (for label '1')
          result <- predict(m.fit$coef, X_mat, type="prob")[,2]
        }
        if(any(is.na(result)) || any(is.null(result))) browser()
        return(result)
      }
    )
)

