#' LibraryFactory
#'
#' This is the main super learner function
#' @slot SL.type the type of platform to use for fitting the models
#' @importFrom methods setClass
#' @export
LibraryFactory <- setClass("LibraryFactory",
  representation(ML.models.allowed = 'vector'),
  prototype(
    ML.models.allowed=c('ML.H2O.glm')
  ),
  validity = function(object) {
    errors <- character()
    # TODO: Test if files actually exist.

    # Check if all models are actually ml models
    are.ml.models <- sapply(object@ML.models.allowed, startsWith, 'ML')
    if(!all(are.ml.models)){
      msg <- 'Not all provided models are ML models as models should start with ML, please only use ML models.'
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
setGeneric("fabricate", function(obj, SL.library, data) {standardGeneric ("fabricate")} )

#' @rdname fabricate
#' @include Data.Base.R
setMethod("fabricate", signature = c(obj = "LibraryFactory", SL.library="vector", data = "Data.Base"),
  function(obj, SL.library, data) {
    # Create objects for each of the objects
    lapply(SL.library, function(entry) {
      if (!isValidMlModel(obj, entry)){
        warning(paste('The model', entry,'is not a valid ML model name'))
        next
      }

      # TODO: Find a way to do this better. We cannot call
      # new since we are overriding the constructor.
      # new(entry, data=data)
      constructor <- paste(entry, "(data=data)",sep='')
      eval(parse(text=constructor))
    })
  }
)

#' isValidMlModel
#'
setGeneric("isValidMlModel", function(obj, ML.name) {standardGeneric ("isValidMlModel")} )
setMethod("isValidMlModel", signature = c(obj="LibraryFactory", ML.name="character"),
  function(obj, ML.name) {
    ML.name %in% obj@ML.models.allowed
  }
)
