#' SMG.Mock
#' Mock summary generator. For testing purposes only.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
SMG.Mock <-
  R6Class (
           "SMG.Mock",
           private =
             list(
                  theMinimalObservations = NULL
                  ),
           active =
             list(
                  minimalObservations = function() {
                    private$theMinimalObservations
                  }
                  ),
           public =
             list(
                  initialize = function(minimalObservations = 1){
                    private$theMinimalObservations <- minimalObservations
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
