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
                    tail(data.current, 1)
                  }
                  )
           )
