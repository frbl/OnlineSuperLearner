#' ML.Base
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @include ML.Base.R
#' @include Data.Base.R
#' @export
ML.Base <-
  R6Class (
           "ML.Base",
           public =
             list(
                  data = NULL,

                  initialize = function(data) {
                    self$data = data
                  },

                  fit = function() {
                    throw('The fit method needs to be inherited')
                  },

                  predict = function() {
                    throw('The predict method needs to be inherited')
                  }
                  )
           )
