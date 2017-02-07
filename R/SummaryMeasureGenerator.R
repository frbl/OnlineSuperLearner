#' SummaryMeasureGenerator
#' @importFrom R6 R6Class
#' @importFrom zoo lag
SummaryMeasureGenerator <-
  R6Class (
           "SummaryMeasureGenerator",
           private =
            list(
                 lags = NULL,
                 data = NULL,
                 cache = data.frame(),
                 minimal.measurements = NULL,
                 lags.vector = NULL
                ),
           public =
             list(
                  initialize = function(data, lags, minimal.measurements=NULL) {
                    private$data <- data
                    private$lags <- lags
                    private$lags.vector <- seq(lags) * -1
                    private$minimal.measurements <- ifelse(is.null(minimal.measurements),
                                                           lags + 1,
                                                           minimal.measurements)
                  },

                  # This function will fill the cache with the first Nl measurements,
                  # if needed.
                  fillCache = function() {
                    updated <-  FALSE
                    while(nrow(private$cache) <= private$minimal.measurements) {
                      updated <- TRUE
                      private$cache = rbind(private$cache, private$data$getNext())
                    }
                    updated
                  },

                  getNext = function(){
                    if(!self$fillCache())
                      private$cache <- rbind(private$cache, private$data$getNext())

                    # Remove the first measurement from the dataframe
                    private$cache = private$cache[-1,]

                    current.data <- zoo(private$cache)
                    current.data <- lag(current.data,k=private$lags.vector, na.pad= TRUE)
                    #m <- mean(df$a)
                    #v <- var(df$a)
                    # Moving window mean
                    # Moving window variance / MSSD
                    #cbind(df, b, m, v)

                    # This will return the lagged dataset, and only the complete cases
                    # which boils down to a single measurement.
                    current.data[complete.cases(current.data),]
                  }
                  )
           )
