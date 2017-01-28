#' LibaryFactory
#'
#' This is the main super learner function
#' @slot x A number
#' @slot y A number
#' @return The best model
#' @examples
#' @importFrom methods setClass new
#' @export
LibraryFactory <- setClass("LibraryFactory",
  representation(
    SL.type = "character"),

  prototype(
    SL.type = "h2o"),

  validity = function(object) {
    errors <- character()

    # Test wether provided type is supported
    SL.types.valid <- c('h2o')
    if(!(object@SL.type %in% SL.types.valid)){
      msg <- 'The provided SL.type is not valid'
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
  }
)


#' @param object An object
#' @param data Numeric vector or data.frame
#' @param Fun Function. Default function is \code{sum}
#' @param ... Extra named arguments passed to FUN
#' @rdname fabricate
#' @export
setGeneric("fabricate", function(object, SL.library) {standardGeneric ("fabricate")} )

#' @rdname run
setMethod("fabricate", "LibaryFactory",
  function(object, SL.library) {
    SL.library.built <- c(123)
    # Create objects for each of the objects
    SL.library.built
  }
)
