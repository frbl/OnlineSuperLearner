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
          # An update means getting the next set of variables, based on the current ones.
          # since this will get the 'new' current ones, this is never possible. Hence
          # we return a list of NA's
          result <- rep(NA, length(private$colnames.to.use))
          names(result) <- private$colnames.to.use
          #data.current[,private$colnames.to.use, with=FALSE]
          as.data.table(t(result))
        },

        process = function(data.current){
          nobs <- nrow(data.current)
          if(nobs < self$minimalObservations){
            throw('At least ', self$minimalObservations, ' observations required')
          }
          
          
          if(self$minimalObservations == 1) return(data.current[,private$colnames.to.use, with=FALSE])
          return(tail( data.current[,private$colnames.to.use, with=FALSE], -(self$minimalObservations - 1)))
        }
    )
)
