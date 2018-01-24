#' Data.Base
#'
#' Base class for any data providing service class. Extend this class if you want to create a new method
#' to read data from
#'
#' @docType class
#' @importFrom R6 R6Class
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{
#'     Creates a new instance of the data.base class. The data.base class is a
#'     super class that can be extended by different sources of data. We
#'     created this class to allow the OSL to both be able to read from a data
#'     that origiginates from a stream, as well as data originating from a
#'     \code{data.table} or other source.
#'   }
#'
#'   \item{\code{getNext()}}{
#'     This method returns the next observation from the underlying data,
#'     depending on the implementation this could be a row from a data frame,
#'     or an observation from a stream. Note: this method needs to be inherited
#'     in the subclass. If not inherited, it wil raise an error.
#'     @return the next measurement from the underlying data source
#'   }
#'
#'   \item{\code{getNextN(n = 1)}}{
#'     This method returns the next \code{n} observations from the underlying
#'     data, depending on the implementation this could be a number of rows
#'     from a data frame, or a number of observations from a stream. Note: this
#'     method needs to be inherited in the subclass. If not inherited, it wil
#'     raise an error.
#'     @param n the number of elements to retrieve.
#'   }
#'}
#' @export
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
