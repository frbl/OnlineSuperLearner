#' ML.H2O
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @include ML.Base.R
#' @include Data.Base.R
#' @include H2O.initializer.R
#' @export
ML.H2O <-
  R6Class("ML.H2O",
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
                 initialize = function() {
                   H2O.Initializer(host = "localhost",
                                   port = 54321,
                                   runlocal = TRUE)
                 }
                 )
          )
