#' SMG.Mean
#'
#' The OnlineSuperLearner uses various \code{SummaryMeasureGenerator} instances
#' to generate new variables that can be used to represent the relevant history
#' of a variable. The \code{SMG.Mean} class allows a user to include the
#' running mean of a variable. The colname for such a variable is generated
#' with a _mean appendix.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(colnames.to.mean) }}{ 
#'     Initializes a new \code{SMG.Mean} object.
#'
#'     @param colnames.to.mean vector a vector containg the names of the
#'      variables for which to create a running mean variable.
#'   } 
#' 
#'   \item{\code{process(data.current) }}{ 
#'     Adds the running mean columns for each of the specified colnames.  It
#'     goes through the data and selects the variables specified on
#'     initialization.
#'
#'     @param data.current data.table the currently available data.
#'
#'     @return data.table with the new summary measure columns.
#'   } 
#' 
#'   \item{\code{update(data.current) }}{ 
#'     For online learning we need to be able to create new data blocks on the
#'     fly (as not all data is available beforehand. This function updates a
#'     set of variables to contain the new version of the running mean.
#'
#'     @param data.current data.table the current data / the last data used for
#'      training, from which a new summary measure needs to be generated.
#'
#'     @return data.table a new block for the next training iteration.
#'   } 
#'
#'   \item{\code{exposedVariables}}{ 
#'     Active method. Returns a list of variables returned by this SMG.
#'
#'     @return vector a vector of strings conaining all specified (used)
#'      contemporaneous colnames.
#'   } 
#' 
#'   \item{\code{minimalObservations}}{ 
#'     Active method. The minimal number of measurements needed for this SMG to
#'     be able to generate a new block. This is 1.
#'
#'     @return integer the minimal number of measurements needed.
#'   } 
#' }  
#' @export
SMG.Mean <- R6Class("SMG.Mean",
  public =
    list(
      initialize = function(colnames.to.mean) {
        private$colnames.to.mean <- colnames.to.mean
        private$colnames.mean.affix <- paste(colnames.to.mean, 'mean', sep = "_")

        private$mean.current <- rep(0,length(colnames.to.mean))
        names(private$mean.current) <- colnames.to.mean
      },

      process = function(data.current){
        nobs <- nrow(data.current)
        if(nobs < self$minimalObservations){
          stop(paste('At least', self$minimalObservations, 'observations required'))
        }
        sums <- colSums(data.current[,private$colnames.to.mean, with=FALSE])

        ## TODO: Super naive implementation, fix it
        private$mean.current <- setNames(mapply('+',
                                                sums[private$colnames.to.mean],
                                                private$mean.current[private$colnames.to.mean] * private$nobs
                                                ),
                                          private$colnames.to.mean)

        private$nobs <- private$nobs + nobs
        private$mean.current <- private$mean.current / private$nobs

        temp <- copy(private$mean.current)
        names(temp) <- private$colnames.mean.affix
        temp
      },

      update = function(data.current) {
        ## The update procedure is essentially the same as the process function
        ## for the mean 
        ## TODO: Do we only need to return the last mean? Or should we also
        ## update the existing mean?
        return(self$process(data.current) %>% unlist)
      }
    ),
  active = 
    list(
      exposedVariables = function() {
        private$colnames.to.mean
      },

      minimalObservations = function() {
        return(private$theMinimalObservations)
      }
    ),
  private =
    list(
      theMinimalObservations = 1,
      mean.current = 0,
      nobs = 0,
      colnames.to.mean = NULL,
      colnames.mean.affix = NULL
    )
)
