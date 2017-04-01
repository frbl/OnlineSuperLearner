#' ML.H2O
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @include ML.Base.R
#' @include Data.Base.R
#' @include H2O.initializer.R
#' @export
ML.H2O <- R6Class("ML.H2O",
  inherit = ML.Base,
  private =
    list(
          getCheckpoint = function() {
            checkpoint <- NULL
            if(!is.null(self$model)){
              checkpoint <- self$model@model_id
            }
            checkpoint
          }
          ),
  public =
    list(
          initialize = function(verbose = Arguments$getVerbose(-8, timestamp=TRUE) ) {
            H2O.Initializer(host = "localhost",
                            port = 54321,
                            runlocal = TRUE,
                            verbose = verbose)
          },

          predict = function(data, A, W) {
            # Upload the data to h2o. 
            # TODO: This is terribly inefficient.
            data.hex <- as.h2o(data, key="data.hex")
            as.data.table(h2o.predict(object = self$model, newdata = data.hex))
          }

    )
)
