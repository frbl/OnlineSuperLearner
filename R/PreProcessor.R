#' PreProcessor
#'
#' @docType class
#' @importFrom R6 R6Class
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

        fn <- ifelse(denormalize, un_scale_data, scale_data)

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
