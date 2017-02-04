#' ML.Base
#'
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @include ML.Base.R
#' @include Data.Base.R
ML.Base <-
  R6Class (
           "ML.Base",
           public =
             list(
                  data = NULL,

                  initialize = function(data) {
                    self$data = data
                  }
                  )
           )
