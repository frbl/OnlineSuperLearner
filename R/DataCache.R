#' DataCache
#' @docType class
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(online = TRUE)}}{
#'     Creates a new data cache. Can be online (so new data gets overwritten),
#'     or offline (so new data gets appended).
#'     @param online boolean whether the data cache is online or offline (default TRUE)
#'   }
#'   \item{\code{update_cache(newdata}}{
#'     Updates the data cache with the new observations. This function only
#'     updates the cache if not all estimators are online. It tells the user
#'     if it has updated by the boolean it returns.
#'     @param newdata data.table the new data to add to the cache (or to replace the cache with)
#'     @return boolean whether or not it actually needed to update the cache.
#'   }
#' }
DataCache <- R6Class("DataCache",
  public =
    list(
      initialize = function(online = TRUE) {
        private$online = online
      },

      update_cache = function(newdata) {
        if (self$is_online) {
          ## If we are truly online, just cache the last entry
          private$data_cache <- newdata
          return(FALSE)
        }
        private$data_cache <- rbindlist(list(self$get_data_cache, newdata))
        return(TRUE)
      }
    ),
  active =
    list(
      is_online = function() {
        private$online 
      },

      get_data_cache = function() {
        return(private$data_cache)
      }
    ),
  private =
    list(
      online = NULL,
      data_cache = NULL
    )
)
