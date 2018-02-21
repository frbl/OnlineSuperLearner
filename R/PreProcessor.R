#' PreProcessor
#'
#' In order to be able to use the binary / logistic loss functions, we convert
#' every value to a value between 0 and 1. This preprocessor class takes care
#' of this conversion by using a set of bounds (the min and max or expected min
#' and max of the data).
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(bounds) }}{ 
#'     Initializes a new \code{PreProcessor} class.
#'
#'     @param bounds list the bounds that should be used when normalizing the
#'     data. This list should contain an entry for each random variable that
#'     should be scaled. Each of those entries should then contain a \code{min}
#'     and \code{max} entry.
#'   } 
#' 
#'   \item{\code{normalize(data) }}{ 
#'     Runs the actual normalization procedure. The data passed in is
#'     normalized according to the bounds specified on initialization.
#'
#'     @param data data.table the non-normalized data.
#'
#'     @return data.table containing the normalized data.
#'   } 
#' 
#'   \item{\code{denormalize(data) }}{ 
#'     Runs the actual normalization procedure. The data passed in is
#'     normalized according to the bounds specified on initialization.
#'
#'     @param data data.table the non-normalized data.
#'
#'     @return data.table containing the normalized data.
#'   } 
#' 
#' }  
#' @export
PreProcessor <- R6Class("PreProcessor",
  public =
    list(
      initialize = function(bounds) {
        private$bounds <- Arguments$getInstanceOf(bounds, 'list')
      },

      normalize = function(data) {
        private$normalization(data, denormalize = FALSE)
      },

      denormalize = function(data) {
        private$normalization(data, denormalize = TRUE)
      }
    ),
  active =
    list(
      get_bounds = function() {
        return(private$bounds) 
      }
    ),
  private =
    list(
      bounds = NULL,

      # Note that data is normalized in place!
      normalization = function(data, denormalize) {
        data <- Arguments$getInstanceOf(data, 'data.table')

        scale_data <- function(data, min_bound,max_bound) {
          (data - min_bound) / (max_bound - min_bound)
        }

        un_scale_data <- function(scaled_data, min_bound,max_bound) {
          scaled_data * (max_bound - min_bound) + min_bound
        }

        if(denormalize) {
          fn <- un_scale_data
        } else {
          fn <- scale_data
        }

        # TODO: Make this more efficient
        # It could be the case that we don't want to normalize all columns. Therefore, loop over the bound names
        # instead of the data names. The reverse is also true, we could also normalize only 1 column at a certain
        # moment in time, hence the intersection.
        normalization_names <- intersect(names(private$bounds), colnames(data))
        for(name in normalization_names) {
          min_bound <- private$bounds[[name]]$min
          max_bound <- private$bounds[[name]]$max
          data[, (name) := lapply(.SD, function(x) fn(x, min_bound, max_bound) ), .SDcols = (name)]
        }
        data
      }
    )
)

#' Static function
#' @param data the data for which to generate the bounds
#' @param eps an extra margin to add to the estimated bounds
#' @export
PreProcessor.generate_bounds <- function(data, eps = 0) {
  if (is(data,'Data.Stream')) {
    throw('Generating bounds based on a stream of data is currently not supported.')
  }

  ## Convert the data.static object to a data.table
  if (is(data,'Data.Static')) data <- data$get_all

  data <- Arguments$getInstanceOf(data, 'data.table')
  bounds <- list()
  for(name in colnames(data)) {
    min_bound = min(data[, name, with=FALSE] )
    max_bound = max(data[, name, with=FALSE] )
    bounds <- append(bounds, list(list(max_bound = max_bound + eps, min_bound = min_bound - eps)))
  }
  names(bounds) <- colnames(data)
  bounds
}
