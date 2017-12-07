#' SMG.Lag
#'
#' @docType class
#' @importFrom R6 R6Class
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
