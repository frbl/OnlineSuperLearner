#' RandomVariable
#'
#' @docType class
#' @importFrom R6 R6Class
RandomVariable <-
  R6Class (
           "RandomVariable",
           private =
            list(
                 formula = NULL,
                 formula.X = NULL,
                 formula.Y = NULL
                ),
           public =
             list(
                  initialize = function(formula, family) {
                  }
                  )
           )
