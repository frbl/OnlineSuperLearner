#' LibraryFactory
#'
#' This is the main super learner function
#' @slot SL.type the type of platform to use for fitting the models
#' @importFrom methods setClass
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

#' Fabricate function
#' 
#' @param object An object
#' @param SL.library the library of different ML methods to use in the SL
#' @rdname fabricate
#' @export
setGeneric("fabricate", function(object, SL.library) {standardGeneric ("fabricate")} )

#' @rdname fabricate
setMethod("fabricate", "LibraryFactory",
  function(object, SL.library) {
    SL.library.built <- c(123)
    # Create objects for each of the objects
    SL.library.built
  }
)
