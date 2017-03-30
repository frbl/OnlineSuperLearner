#' Base class for any data providing service class. Extend this class if you want to create a new method
#' to read data from
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{This method is used to create object of this class. }
#'
#'   \item{\code{getNext()}}{Method to retrieve the next observation from the data.}
#'   \item{\code{getNextN(n)}}{Method that returns the next \code{n} observations. This function can be used to bootstrap an initial model.}
#'}
Data.Base <- R6Class ("Data.Base",
  public =
    list(
        initialize = function() {
        },

        getNext = function(){
          throw('This method needs to be inherited in a subclass')
        },

        getNextN = function(n = 1){
          # TODO: This method is currently super inefficient, and just a placeholder.
          # This should be updated whenever there is a different way. It also depends on the underlaying class how this method should be implemented.
          data <- data.table()
          while(nrows(data) < n) {
            data <- rbindlist(list(data, self$getNext()))
          }
          data
        }
  )

)
