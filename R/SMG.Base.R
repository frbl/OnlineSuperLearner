#' SMG.Base
#'
#' Base class for any summary measure generator instance. These are used by the
#' \code{SummaryMeasureGenerator} to create summary measures.
#'
#' @docType class
#' @importFrom R6 R6Class
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{
#'   }
#'
#'   \item{\code{process()}}{
#'   }
#'
#'   \item{\code{update(data.current)}}{
#'   }
#'}
#' @export
SMG.Base <- R6Class ("SMG.Base",
  public =
    list(
      initialize = function() {
      },

      process = function(data.current){
        throw('This method needs to be inherited in a subclass')
      },

      update = function(data.current){
        throw('This method needs to be inherited in a subclass')
      }
    ),
  active =
    list(
      check_enough_available = function(data.current) {
        nobs <- nrow(data.current)
        if(nobs < self$minimalObservations){
          stop(paste('At least', self$minimalObservations, 'observations required'))
        }
      }
    )
)
