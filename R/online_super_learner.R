#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#' @slot X A number
#' @slot Y A number
#' @return The best model
#' @importFrom methods setClass new
#' @import h2o
#' @import h2o.demo
#' @export
OnlineSuperLearner <- setClass("OnlineSuperLearner",
  #TODO: this representation should probably accept a datareader object from which
  # it retrieves lines of data. Also, we should add a parameter in which an already
  # fitted model can be inputted, so it gets updated.
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
#' @param obj The superlearner object.
#' @return the fitted superlearner object.
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


    # 1.3 Setup a connection with H2O
    localh2o <- H2oInitializer(host='imac.evionix.org', runlocal = FALSE)
    prostate.hex = h2o.importFile(path = "https://raw.github.com/0xdata/h2o/master/smalldata/logreg/prostate.csv", destination_frame = "prostate.hex")

    # 2. Fit different models using cross validation
    glm <- H2o.model.GLM()
    fitted <- fit(glm, y = "CAPSULE", X = c("AGE","RACE","PSA","DCAPS"), data = prostate.hex)

    print(fitted)
    # 3. Fit a final model which is a glm of the original models

    # 4. return the final model

    return(NULL)
  }
)

start <- function(){
  devtools::load_all()
  ols <- OnlineSuperLearner()
  run(ols)
}
