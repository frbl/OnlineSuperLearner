#' Base class for any XGBoost machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.Base.R
ML.XGBoost <-
  R6Class(
          "ML.XGBoost",
          inherit = ML.Base,
          private = list(),
          public = list()
          )
