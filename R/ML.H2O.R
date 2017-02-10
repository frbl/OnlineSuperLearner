#' ML.H2O
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @include ML.Base.R
#' @include Data.Base.R
#' @export
ML.H2O <- R6Class("ML.H2O", inherit = ML.Base, public = list(initialize = function(data) {
    super$initialize(data = h2o.importFile(data$getAll()))
}))
