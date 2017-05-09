#' SMG.Latest.Entry
#'
#' @docType class
#' @importFrom R6 R6Class
SMG.Latest.Entry <- R6Class("SMG.Latest.Entry",
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

        update = function(data.current) {
          data.current[,private$colnames.to.use, with=FALSE]
        },

        process = function(data.current){
          nobs <- nrow(data.current)
          if(nobs < self$minimalObservations){
            throw('At least ', self$minimalObservations, ' observations required')
          }
          
          if(self$minimalObservations == 1) return(self$update(data.current))
          return(tail(self$update(data.current), -(self$minimalObservations - 1)))
        }
    )
)
