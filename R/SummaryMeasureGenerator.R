#' SummaryMeasureGenerator
#'
#' This is a decorator for a general Data.Base object. It retrieves data from it, processes it, and
#' returns a data frame containing the summary measure provided. Currently it only provides the lag
#' of the data, but this could be extended with for example the mean, variance, MSSD of the data.
#' to the super learner machine learning model.
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{}
#'
#'   \item{\code{fillCache()}}{}
#'   \item{\code{getNext()}}{}
#'   \item{\code{getNextN()}}{}
#' }
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
           active =
             list(),
           public =
             list(
                  X = NULL,
                  Y = NULL,
                  A = NULL,
                  initialize = function(X, Y, data, lags, minimal.measurements = NULL) {
                    self$X <- X
                    self$Y <- Y

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
                    if(nrow(private$cache) < private$minimal.measurements) {
                      updated <- TRUE
                      private$cache <- private$data$getNextN(private$minimal.measurements)
                    }
                    updated
                  },

                  getNext = function(){
                    if(!self$fillCache()) {
                      private$cache <- rbindlist(list(private$cache, private$data$getNext()))

                      # Remove the first measurement from the dataframe
                      private$cache <- private$cache[-1, ]
                    }

                    # on data.tables
                    current.data <- copy(private$cache)

                    # Create column names
                    anscols  <- unlist(lapply(seq(private$lags), function(x) paste(x, 'lag', c(self$X, self$Y), sep = "_")))
                    current.data[, (anscols) := shift(.SD, private$lags.vector, NA, "lag"), .SDcols = c(self$X, self$Y)]

                    # This will return the lagged dataset, and only the complete cases
                    # which boils down to a single measurement.
                    current.data[complete.cases(current.data), ]
                  },

                  getNextN = function(n = 1){
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
