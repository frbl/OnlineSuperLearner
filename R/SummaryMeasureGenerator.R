#' SummaryMeasureGenerator
#'
#' This is a decorator for a general Data.Base object. It retrieves data from it, processes it, and
#' returns a data frame containing the summary measure provided. Currently it only provides the lag
#' of the data, but this could be extended with for example the mean, variance, MSSD of the data.
#' to the super learner machine learning model.
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @include SMG.Mock.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{}
#'
#'   \item{\code{fillCache()}}{}
#'   \item{\code{getNext()}}{}
#' }
#' @export
SummaryMeasureGenerator <- R6Class("SummaryMeasureGenerator",
  public =
    list(
        minimal.measurements.needed = NULL,

        initialize = function(data = NULL, SMG.list, verbose = FALSE, pre_processor = NULL) {
          private$data <- data
          private$SMG.list <- SMG.list
          private$verbose <- verbose

          if (is.null(pre_processor)) {
            private$normalized <- FALSE
          } else {
            private$pre_processor <- Arguments$getInstanceOf(pre_processor, 'PreProcessor')
            private$normalized <- TRUE
          }

          # Determine the minimal number of measurements we need in order to be able to
          # support all our SMGs
          # TODO: We do the -1 so we can just get new measurements, without caring about the cache getting filled or not.
          # This could be made more explicit
          self$minimal.measurements.needed <- max(sapply(SMG.list, function(obj) obj$minimalObservations)) - 1
        },

        reset = function() {
          private$cache = data.table()
        },

        checkEnoughDataAvailable = function(randomVariables) {
          # Currently we do not support interactions
          needed <- unique(unlist(lapply(randomVariables, function(rv) rv$getX)))
          available <- unlist(lapply(private$SMG.list, function(smg) smg$exposedVariables))
          diff <- setdiff(needed, available)

          # check if our set is empty, in that case we cover them all
          if (length(diff) != 0) { 
            missing <- paste(diff, collapse = ', ', sep = ', ')
            throw('Not all provided variables (', missing, ') are included in the SMGs, include the correct SMGs')
          }
          TRUE
        },

        setData = function(data) {
          self$reset()
          private$data = data
        },

        # This function will fill the cache with the first N measurements if the cache is empty
        fillCache = function() {
          private$checkDataAvailable()
          # If no history is needed, we don't have to fill the cache
          if(self$minimal.measurements.needed == 0) return(FALSE)

          extraMeasurementsNeeded <- nrow(self$getCache) - self$minimal.measurements.needed
          if(extraMeasurementsNeeded < 0) {
            private$cache <- private$get_next_normalized(n=abs(extraMeasurementsNeeded))
            return(TRUE)
          }
          FALSE
        },

        getLatestCovariates =function(data) {
          if(nrow(data) != 1){
            throw('Not enough data provided to support all summary measures')
          }
          datas <- unlist(lapply(private$SMG.list, function(smg) {smg$update(copy(data))}))
          as.data.table(t(datas))
        },

        summarizeData = function(data, n = 1){
          if(nrow(data) <= self$minimal.measurements.needed){
            throw('Not enough data provided to support all summary measures')
          }

          datas <- lapply(private$SMG.list, function(smg) {
              result <- smg$process(copy(data))
              tail(result, n)
          })

          Reduce(cbind, datas)
        },

        getNext = function(n = 1) {
          private$checkDataAvailable()

          # TODO: Make this much more efficient
          # TODO: This is an exact copy of the Data.Base function
          filledCache  <- self$fillCache()

          if(!filledCache) {
            # Remove the first n measurements from the dataframe
            private$cache <- tail(private$cache, -n)
          }
          current <- private$get_next_normalized(n=n)
          # Now, this combined with the cache, should be enough to get the new observations
          private$cache <- rbindlist(list(private$cache, current))

          self$summarizeData(private$cache, n=n)

        }
        ),
  active =
    list(
        get_pre_processor = function() {
          private$pre_processor
        },

        is_normalized = function() {
          private$normalized
        },

        getCache = function(){
          return(private$cache)
        },

        get_data_object = function() {
          return(private$data)
        },

        get_smg_list = function() {
          return(private$SMG.list)
        }
        ),
  private =
    list(
        data = NULL,
        cache = data.table(),
        SMG.list = NULL,
        verbose = NULL,
        normalized = NULL,
        pre_processor = NULL,

        get_next_normalized = function(n) {
          # Get the next N observations, rely on the data source to get this data efficient
          current <- private$data$getNextN(n = n)
          if (is.null(current)) return(NULL)

          if (self$is_normalized) current %<>% self$get_pre_processor$normalize(.)
          current
        },

        checkDataAvailable = function() {
          if(is.null(private$data)) {
            throw('Please set the data of the summary measure generator first')
          }
        }
    )
)
