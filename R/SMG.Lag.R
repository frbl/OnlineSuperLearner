#' SMG.Lag
#'
#' The OnlineSuperLearner uses various \code{SummaryMeasureGenerator} instances
#' to generate new variables that can be used to represent the relevant history
#' of a variable. The \code{SMG.Lag} class allows a user to specify lagged
#' variables. That is, in the prediction of a variable at time $t$, one can
#' include variables from past measurements, if there is an lagged or cross
#' lagged effect.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(lags, colnames.to.lag) }}{ 
#'     Initializes a new \code{SMG.Lag} object.
#'
#'     @param lags integer the number of lags the \code{SMG.Lag} object should
#'      create (if there is a lag_1 variable, this number should be 1).
#'
#'     @param colnames.to.lag vector a vector containg the names of the
#'      variables to lag. 
#'   } 
#' 
#'   \item{\code{lagged_colnames(colnames = NULL) }}{ 
#'     This function returns a list of names that represent the lagged names of
#'     the \code{colnames} provided to it. It uses the \code{lags} variable as
#'     specified in the initialization step. One can choose to provide colnames
#'     to this function to get the result for a specific set of colnames. If no
#'     colnames are provided, the function uses the \code{colnames.to.lag}
#'     names as specified on initialization.
#'
#'     @param colnames vector (default = NULL) the vector of names for which to
#'      genearte a lagged version.
#'
#'     @return vector a vector of colnames with appended '_lag_x' names.
#'   } 
#' 
#'   \item{\code{update(data.current) }}{ 
#'     For online learning we need to be able to create new data blocks on the
#'     fly (as not all data is available beforehand. This function updates a
#'     set of variables to contain the lagged variables for the next block.
#'
#'     @param data.current data.table the current data / the last data used for
#'      training, from which a new summary measure needs to be generated.
#'
#'     @return data.table a new block for the next training iteration.
#'   } 
#' 
#'   \item{\code{process(data.current) }}{ 
#'     Converts lagged columns for all provided data. It goes through the data
#'     and generates the lagged variables accordingly.
#'
#'     @param data.current data.table the currently available data.
#'
#'     @return data.table with the new summary measure columns.
#'   } 
#' 
#'   \item{\code{minimalObservations}}{ 
#'     Active method. The minimal number of measurements needed for this SMG to
#'     be able to generate a new block. This is the number of lags + 1.
#'
#'     @return integer the number of lags + 1
#'   } 
#' 
#'   \item{\code{exposedVariables}}{ 
#'     Active method. Returns a list of variables returned by this SMG.
#'
#'     @return vector a vector of strings conaining all lagged columnames.
#'   } 
#' }  
#' @export
SMG.Lag <- R6Class("SMG.Lag",
  public =
    list(
      initialize = function(lags, colnames.to.lag){
        private$lags <- lags
        private$lags.vector <- seq(1,lags)
        private$colnames <- colnames.to.lag
        private$colnames.lagged  <- self$lagged_colnames()
      },

      lagged_colnames = function(colnames = NULL ) {
        if (is.null(colnames)) colnames <- private$colnames

        ## This will create a list like this:
        ## A_lag_1 A_lag_2  W_lag_1   W_lag_2  Y_lag_1   Y_lag_2
        ## If we want to have a different ordering, note that the update function also
        ## needs to be updated!
        unlist(lapply(colnames, function(col){
          paste(col, 'lag', seq(private$lags), sep = "_")
        })
        )
      },

      ## An update means getting the next set of variables, based on the
      ## current ones. This results in a vector with the new values
      update = function(data.current) {
        if (nrow(data.current) > 1) {
          throw('Provided number of rows to lag / update function needs to be one')
        }
        ## The data we have is 1 row, with all lags available. We only focus on the lags

        ## Retrieve the columns we are interested in
        subset_to_lag <- data.current[, c(private$colnames, private$colnames.lagged), with=FALSE]
        data.new <- lapply(private$colnames, function(cn) {

          ## Only use the current columnname + its lagged ones (this should be enough)
          used_colnames <- c(cn, self$lagged_colnames(cn))
          current <- subset(subset_to_lag, select = used_colnames)

          ## shift the names (essentially changing x -> x_lag_1, and x_lag_1 to x_lag_2)
          current.names <- shift(names(current), type = 'lead')

          ## Rename the other columns to REMOVE to remove them all
          current.names[is.na(current.names)] <- 'REMOVE'
          names(current) <- current.names
          while('REMOVE' %in% names(current)){
            current[,REMOVE:=NULL]
          }
          current
        }) %>% unlist
        return(data.new)
      },

      process = function(data.current) {
        # Create column names
        if(nrow(data.current) < self$minimalObservations){
          stop(paste('At least', self$minimalObservations, 'observations required'))
        } #else if(anyNA(tail(data.current, 1))){
          ## If the last row contains an NA, we are not supposed to create new lags,
          ## So we just return the lagged data we received.
          #return(head(data.current, -1))
        #}

        # TODO: We could probably do this in a smarter way, i.e., creating an initial frame first
        # and than prepend new columns, and remove the last column.
        data.current[, (private$colnames.lagged) := shift(.SD, private$lags.vector, NA),
                      .SDcols = private$colnames]

        # This will return the lagged dataset, and only the complete cases
        # which boils down to a single measurement.
        data.current[complete.cases(data.current), private$colnames.lagged, with=FALSE]
      }
    ),
  active =
    list(
      minimalObservations = function() {
        private$lags + 1
      },
      exposedVariables = function() {
        private$colnames.lagged
      }
    ),
  private =
    list(
      minimal.measurements = NULL,
      lags.vector = NULL,
      lags = NULL,
      colnames.lagged = NULL,
      colnames = NULL
    )
)
