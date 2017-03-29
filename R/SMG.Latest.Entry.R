#' SMG.Latest.Entry
#'
#' @docType class
#' @importFrom R6 R6Class
SMG.Latest.Entry <-
  R6Class (
           "SMG.Latest.Entry",
           private =
            list(
                 colnames.to.use = NULL
                ),
           active =
             list(
                  exposedVariables = function() {
                    private$colnames.to.use
                  }
                  ),
           public =
             list(
                  initialize = function(colnames.to.use) {
                    private$colnames.to.use = colnames.to.use
                  },

                  minimalObservations = 1,

                  process = function(data.current){
                    nobs <- nrow(data.current)
                    if(nobs < self$minimalObservations){
                      stop(paste('At least', self$minimalObservations, 'observations required'))
                    }
                    if(self$minimalObservations == 1) return(data.current)
                    return(tail(data.current, -(self$minimalObservations - 1)))
                  }
                  )
           )
