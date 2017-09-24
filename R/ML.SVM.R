#' ML.SVM
#' Class to create support vector machine models
#'
#' @docType class
#' @importFrom e1071 svm
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{
#'     Initializes a new SVM estimator. 
#'   }
#' }
#' @export
ML.SVM <- R6Class("ML.SVM",
  inherit = ML.Base,
  public =
    list(
        fitfunname='supportVectorMachine',
        lmclass='ML.SVM',
        initialize = function() { }
        ),
  active =
    list(
        ),
  private =
    list(
        do.fit = function(X_mat, Y_vals) {
          svm(x= X_mat, y = Y_vals, probability = TRUE)
        },

        do.predict = function(X_mat, m.fit) {
          if (any(is.na(m.fit$coef))) {
            result <- super$do.predict(X_mat, m.fit)
          } else {
            result <- predict(m.fit, X_mat)
          }
          if(any(is.na(result))) browser()
          return(result)
        }
    )
)

