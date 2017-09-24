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
        initialize = function() { }
        ),
  active =
    list(
        ),
  private =
    list(
        do.fit = function(X_mat, Y_vals) {
          randomForest(x = X_mat, y = Y_vals)
        },

        do.update = function(m.fit, X_mat, Y_vals) {
          updateWithMoreData(m.fit, X = X_mat, y = Y_vals)
        },

        do.predict = function(X_mat, m.fit) {
          if (any(is.na(m.fit$coef))) {
            result <- super$do.predict(X_mat, m.fit)
          } else {
            result <- predict(m.fit, X_mat, type="response")
          }
          if(any(is.na(result))) browser()
          return(result)
        }
    )
)

