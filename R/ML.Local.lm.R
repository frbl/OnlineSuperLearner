#' ML.Local.lm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stats lm
#' @include ML.Local.R
#' @export
ML.Local.lm <-
  R6Class (
           "ML.Local.lm",
           inherit = ML.Local,
           private =
             list(
                  model = NULL
                  ),
           active =
             list(
                  score = function() {
                    if(is.null(private$model)) {
                      throw('Fit a model first!')
                    }
                    summary(private$model)$r.squared
                  }
                  ),
           public =
             list(
                  initialize = function() {
                  },

                  fit = function(data, X, Y){
                    warning('This method does not actually work, its just for testing purposes')
                    x <- paste(X, collapse = ' + ')
                    formula <- paste(Y, '~', x)

                    # If there is no model, we need to fit a model based on Nl observations.
                    # If we already have a model, we update the old one, given the new measurement
                    if(is.null(private$model)){
                      private$model <- lm(formula, data)
                    } else {
                      private$model <- update(private$model, formula, data)
                    }
                    private$model
                  }
                  )
           )
