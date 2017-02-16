#' SMG.Lag
#'
#' @docType class
#' @importFrom R6 R6Class
SMG.Lag <-
  R6Class (
           "SMG.Lag",
           private =
             list(
                  minimal.measurements = NULL,
                  lags.vector = NULL,
                  lags = NULL,
                  colnames.lagged = NULL,
                  colnames = NULL
                  ),
           active =
             list(
                  minimalObservations = function() {
                    private$lags + 1
                  }
                  ),
           public =
             list(
                  initialize = function(lags, colnames.to.lag){
                    private$lags <- lags
                    private$lags.vector <- seq(1,lags)
                    private$colnames <- colnames.to.lag
                    private$colnames.lagged  <- unlist(lapply(seq(private$lags), function(x) paste(colnames.to.lag,'lag', x , sep = "_")))
                  },

                  process = function(data.current) {
                    # Create column names
                    if(nrow(data.current) < self$minimalObservations){
                      stop(paste('At least', self$minimalObservations, 'observations required'))
                    }

                    data.current[, (private$colnames.lagged) := shift(.SD, private$lags.vector, NA, "lag"),
                                 .SDcols = private$colnames]

                    # This will return the lagged dataset, and only the complete cases
                    # which boils down to a single measurement.
                    data.current[complete.cases(data.current), private$colnames.lagged, with=FALSE]
                  }

                  )
           )
