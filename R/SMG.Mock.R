#' SMG.Mock
#' Mock summary generator. For testing purposes only.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
SMG.Mock <- R6Class("SMG.Mock",
  private =
    list(
        theMinimalObservations = NULL,
        theExposedVariables = NULL
        ),
  active =
    list(
        minimalObservations = function() {
          private$theMinimalObservations
        },
        exposedVariables = function() {
          private$theExposedVariables
        }
        ),
  public =
    list(
        initialize = function(minimalObservations = 1, variables = c('y','a','w'), exposedVariables = ''){
          private$theMinimalObservations <- minimalObservations
          private$theExposedVariables <- unlist(lapply(variables, function(x) paste(x,exposedVariables, sep='')))
        },
        
        process = function(data.current) {
          # Using the history (minimalObservations), it should return a DF of the data size
          # - the size of the data needed as history 
          if(self$minimalObservations == 1) return(data.current)
          return(tail(data.current, -(self$minimalObservations -1)))
        }
    )
)

# stel history van 2 nodig voor 1 obs. Dan 3 voor 2 obs, 
