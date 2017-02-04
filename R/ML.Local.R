#' ML.Local
#'
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @include ML.Base.R
#' @include Data.Base.R
ML.Local <-
  R6Class (
           "ML.Local",
           inherit = ML.Base,
           public =
             list(
                  initialize = function(data) {
                    super$initialize(data = data$readDataFromUrl())
                  }
                  )
           )
