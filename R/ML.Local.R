#' ML.Local
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.Base.R
#' @include Data.Base.R
#' @keywords data
ML.Local <- R6Clas("ML.Local",
  inherit = ML.Base,
  public =
    list(
          initialize = function() {
            super$initialize()
          },

          createFormula = function(Y, A, W) {
            X <- paste(c(A,W), collapse = ' + ')
            paste(Y, '~', X)
          }
    )
)
