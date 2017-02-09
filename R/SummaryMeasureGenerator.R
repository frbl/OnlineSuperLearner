#' SummaryMeasureGenerator
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @export
SummaryMeasureGenerator <-
  R6Class (
           "SummaryMeasureGenerator",
           private =
             list(
                  lags = NULL,
                  data = NULL,
                  cache = data.table(),
                  minimal.measurements = NULL,
                  lags.vector = NULL
                  ),
           public =
             list(
                  initialize = function(data, lags, minimal.measurements=NULL) {
                    private$data <- data
                    private$lags <- lags
                    private$lags.vector <- seq(lags)
                    private$minimal.measurements <- ifelse(is.null(minimal.measurements),
                                                           lags + 1,
                                                           minimal.measurements)
                  },

                  # This function will fill the cache with the first Nl measurements,
                  # if needed.
                  fillCache = function() {
                    updated <-  FALSE
                    # TODO: Maybe we can read a number of measurements in one call?
                    while(nrow(private$cache) < private$minimal.measurements) {
                      updated <- TRUE
                      new <-private$data$getNext()
                      print(paste('old', dim(private$cache)))
                      print(paste('new', dim(new)))
                      private$cache <- rbindlist(list(private$cache, new))
                    }
                    updated
                  },

                 getNext = function(){
                    if(!self$fillCache()) {
                      private$cache <- rbindlist(list(private$cache, private$data$getNext()))

                      # Remove the first measurement from the dataframe
                      private$cache <- private$cache[-1,]
                    }

                    # on data.tables
                    current.data <- copy(private$cache)
                    cols <- c("CAPSULE", "AGE","RACE","PSA","DCAPS")

                    # Create column names
                    anscols  <- unlist(lapply(seq(private$lags), function(x) paste(x, 'lag', cols, sep="_")))
                    current.data[, (anscols) := shift(.SD, private$lags.vector, NA, "lag"), .SDcols=cols]

                    #m <- mean(df$a)
                    #v <- var(df$a)
                    # Moving window mean
                    # Moving window variance / MSSD
                    #cbind(df, b, m, v)

                    # This will return the lagged dataset, and only the complete cases
                    # which boils down to a single measurement.
                    current.data[complete.cases(current.data),]
                  },

                  getNextN = function(n=1){
                    # TODO: Make this much more efficient
                    # TODO: This is an exact copy of the Data.Base function
                    dt <- data.table()
                    i <- 0
                    while(i < n) {
                      dt <- rbindlist(list(dt, self$getNext()))
                      i <- i +1
                    }
                    dt
                  }
                  )
           )
