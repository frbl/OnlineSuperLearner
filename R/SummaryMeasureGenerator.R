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
                  verbose = NULL,
                  minimal.measurements.needed = NULL
                  ),
           public =
             list(
                  initialize = function(data = NULL, SMG.list, verbose = FALSE) {
                    private$data <- data
                    private$SMG.list <- SMG.list
                    private$verbose <- verbose

                    # Determine the minimal number of measurements we need in order to be able to
                    # support all our SMGs
                    private$minimal.measurements.needed <- max(sapply(SMG.list, function(obj) obj$minimalObservations))
                  },

                  reset = function() {
                    private$cache = data.table()
                  },

                  setData = function(data) {
                    self$reset()
                    private$data = data
                  },

                  # This function will fill the cache with the first N measurements if the cache is empty
                  fillCache = function() {
                    if(nrow(private$cache) < private$minimal.measurements.needed) {
                      private$cache <- private$data$getNextN(private$minimal.measurements.needed)
                      return(TRUE)
                    }
                    FALSE
                  },

                  getNext = function(n = 1) {
                    if(is.null(private$data)) {
                      throw('Please set the data of the summary measure generator first')
                    }

                    dt <- data.table()
                    # TODO: Make this much more efficient
                    # TODO: This is an exact copy of the Data.Base function
                    for(i in 1:n) {

                      if(!self$fillCache()) {
                        current <- private$data$getNext()
                        if (is.null(current)) return(NULL)

                        private$cache <- rbindlist(list(private$cache,current ))

                        # Remove the first measurement from the dataframe
                        private$cache <- tail(private$cache, -1)
                      }

                      # Generate summary measures using each of the provided objects
                      # TODO: Use reduce here
                      datas <- lapply(private$SMG.list, function(smg) smg$process(copy(private$cache)))
                      current <- data.table(t(unlist(datas)))

                      if(is.null(current)) break

                      dt <- rbindlist(list(dt, current))
                    }
                    dt
                  },

                  getNextN = function(n = 1){
                    tic <- Sys.time()
                    private$verbose && enter(private$verbose, paste('Generating subset of',n,'observations'))
                    data <- self$getNext(n=n) 
                    toc <- Sys.time()
                    private$verbose && exit(private$verbose, paste('This took', (toc - tic), 'seconds!!'))
                    data

                  }
                  )
           )
