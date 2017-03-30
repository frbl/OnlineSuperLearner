#' SMG.Lag
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
SMG.Lag <- R6Class("SMG.Lag",
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
        },
        exposedVariables = function() {
          private$colnames.lagged
        }
        ),
  public =
    list(
        initialize = function(lags, colnames.to.lag){
          private$lags <- lags
          private$lags.vector <- seq(1,lags)
          private$colnames <- colnames.to.lag
          private$colnames.lagged  <- self$laggedColnames()
        },

        laggedColnames = function() {
          # This will create a list like this:
          # A_lag_1 A_lag_2  W_lag_1   W_lag_2  Y_lag_1   Y_lag_2
          # If we want to have a different ordering, note that the update function also
          # needs to be updated!
          unlist(lapply(private$colnames, function(col){
            paste(col, 'lag', seq(private$lags), sep = "_")
          })
          )
        },

        update = function(data.current) {
          # The data we have is 1 row, with all lags available. We only focus on the lags
          subset.to.lag <- data.current[, c(private$colnames, private$colnames.lagged), with=FALSE]
          data.new <- lapply(private$colnames, function(cn) {
            current <- subset(subset.to.lag, select=grep(cn, names(subset.to.lag)))
            current.names <- shift(names(current), type='lead')
            current.names[is.na(current.names)] <- 'REMOVE'
            names(current) <- current.names
            while('REMOVE' %in% names(current)){
              current[,REMOVE:=NULL]
            }
            current
          })
          return(unlist(data.new))
        },

        process = function(data.current) {
          # Create column names
          if(nrow(data.current) < self$minimalObservations){
            stop(paste('At least', self$minimalObservations, 'observations required'))
          } else if(anyNA(tail(data.current, 1))){
            # If the last row contains an NA, we are not supposed to create new lags,
            # So we just return the lagged data we received.
            return(head(data.current, -1))
          }

          # TODO: We could probably do this in a smarter way, i.e., creating an initial frame first
          # and than prepend new columns, and remove the last column.
          data.current[, (private$colnames.lagged) := shift(.SD, private$lags.vector, NA),
                        .SDcols = private$colnames]

          # This will return the lagged dataset, and only the complete cases
          # which boils down to a single measurement.
          data.current[complete.cases(data.current), private$colnames.lagged, with=FALSE]
        }

    )
)
