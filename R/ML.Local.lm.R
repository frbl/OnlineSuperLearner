#' ML.Local.lm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.Local.R
#' @export
ML.Local.lm <-
  R6Class (
           "ML.Local.lm",
           inherit = ML.Local,
           private =
             list(
                  observations.minimum = NULL
                  ),
           public =
             list(
                  nfolds = NULL,
                  alpha = NULL,
                  family = NULL,


                  initialize = function(data, observations.minimum=5) {
                    super$initialize(data = data)
                    priate$observations.minimum = observations.minimum
                  },

                  fit = function(X, y, oldmodel = NULL){
                    formula <- paste(y, '~', X)

                    # If there is no model, we need to fit a model based on Nl observations.
                    # If we already have a model, we update the old one, given the new measurement
                    if(is.null(oldmodel)){
                      data  <- self$data$getNextN(n=private$observations.minimum)
                      model <- lm(formula, data)
                    } else {
                      model <- update(oldmodel, formula, self$data$getNext())
                    }
                    model
                  }
                  )
           )
