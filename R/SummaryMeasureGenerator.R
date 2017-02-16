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
                  data = NULL,
                  cache = data.table(),
                  SMG.list = NULL,
                  minimal.measurements.needed = NULL
                  ),
           public =
             list(
                  W = NULL,
                  Y = NULL,
                  A = NULL,
                  initialize = function(W, A, Y, data, SMG.list) {
                    self$W <- W
                    self$A <- A
                    self$Y <- Y

                    private$data <- data
                    private$SMG.list <- SMG.list
                    private$minimal.measurements.needed <- max(sapply(SMG.list, function(obj) obj$minimalObservations))
                  },

                  # This function will fill the cache with the first Nl measurements,
                  # if needed.
                  fillCache = function() {
                    updated <-  FALSE
                    if(nrow(private$cache) < private$minimal.measurements.needed) {
                      updated <- TRUE
                      private$cache <- private$data$getNextN(private$minimal.measurements.needed)
                    }
                    updated
                  },

                  getNext = function(){
                    if(!self$fillCache()) {
                      private$cache <- rbindlist(list(private$cache, private$data$getNext()))

                      # Remove the first measurement from the dataframe
                      private$cache <- private$cache[-1, ]
                    }

                    # Generate summary measures using each of the provided objects
                    current.data <- copy(private$cache)
                    datas <- lapply(private$SMG.list, function(smg) smg$process(copy(current.data)))
                    data.table(t(unlist(datas)))
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
