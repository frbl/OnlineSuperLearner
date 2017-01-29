#' OnlineSuperLearner
#'
#' This is the main super learner function
#' @slot X A number
#' @slot Y A number
#' @return The best model
#' @importFrom methods setClass new
#' @export
OnlineSuperLearner <- setClass("OnlineSuperLearner",
  representation(
    Y = "vector",
    X = "data.frame",
    family = "ANY",
    SL.library = "vector",
    method = "character",
    verbose = "numeric",
    control = "list"),

  prototype(
    family = gaussian(),
    method = "",
    verbose = 0),

  validity = function(object) {
    errors <- character()

    # Test wether the arrays have the same shape
    if(nrow(object@X) != length(object@Y)) {
      msg <- 'Dimensions of X and Y are not the same'
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
  }
)


#' The run function
#'
#' @param obj The superlearner object
#' @docType methods
#' @rdname run
#' @importFrom methods new
#' @export
setGeneric(name="run", def = function(obj) { standardGeneric("run") } )

#' @rdname run
setMethod("run", signature = "OnlineSuperLearner",
  definition = function(obj) {
    # Steps in superlearning:

    # 1. Initialization
    # 1.1 Check parameters (dimensions of X / Y)

    # 1.2 Fabricate the various models
    libraryFactory <- LibraryFactory()
    SL.fabricated.models <- fabricate(libraryFactory, 'methods')

    # 2. Fit different models using cross validation

    # 3. Fit a final model which is a glm of the original models

    # 4. return the final model

    return(NULL)
  }
)
