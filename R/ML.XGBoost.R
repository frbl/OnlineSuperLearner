#' ML.XGBoost
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.Base.R
ML.XGBoost <-
  R6Class (
           "ML.XGBoost",
           inherit = ML.Base,
           private =
            list(
                ),
           public =
             list(
                  initialize = function(data) {
                    super$initialize(data = data$readDataFromUrl())
                  }
                  )
           )
