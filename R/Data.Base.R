#' Base class for any data providing service class. Extend this class if you want to create a new method
#' to read data from
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize() }}{
#'     Creates a new instance of the data.base class
#'   }
#'
#'   \item{\code{getNext()}}{
#'     This method returns the next observation from the underlying data, depending on the implementation this could be
#'     a row from a data frame, or an observation from a stream. Note: this method needs to be inherited in the
#'     subclass.
#'   }
#'
#'   \item{\code{getNextN(n = 1)}}{
#'     This method returns the next \code{n} observations from the underlying data, depending on the implementation this
#'     could be a number of rows from a data frame, or a number of observations from a stream. Note: this method needs
#'     to be inherited in the subclass.
#'     @param n the number of elements to retrieve.
#'   }
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
          throw('This method needs to be inherited in a subclass')
        }
  )

)
