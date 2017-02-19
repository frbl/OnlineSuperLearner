#' The libraryFactory can instantiate a set of models given to it.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @include ML.XGBoost.glm.R
#' @section Methods:
#' \describe{
#'   \item{\code{new(ML.models.allowed = c('ML.H2O.glm', 'ML.Local.lm', 'ML.XGBoost.glm'))}}{This method is used to create object of this class. It expects a \code{ML.models.allowed} as a list which describes all of the models that are allowed for fabrication. }
#'
#'   \item{\code{getValidity()}}{Method to determine if the object is in a valid state.}
#'   \item{\code{fabricate(SL.library)}}{Method that fabricates the models in the provided \code{SL.library}.}
#'}
LibraryFactory <-
  R6Class (
           "LibraryFactory",
           private =
             list(
                  ML.models.allowed = NULL,
                  isValidMlModel = function(ML.name) {
                    ML.name %in% private$ML.models.allowed
                  }
                  ),

           public =
             list(
                  initialize = function(ML.models.allowed = c('ML.H2O.gbm',
                                                              'ML.H2O.glm',
                                                              'ML.Local.lm',
                                                              'ML.XGBoost.glm')) {
                    private$ML.models.allowed = ML.models.allowed
                    self$getValidity()
                  },

                  getValidity = function() {
                    errors <- character()
                    # TODO: Test if files actually exist.
                    # Check if all models are actually ml models
                    are.ml.models <- sapply(private$ML.models.allowed, startsWith, 'ML')
                    if(!all(are.ml.models)){
                      msg <- 'Not all provided models are ML models as models should start with ML, please only use ML models.'
                      errors <- c(errors, msg)
                    }
                    if (length(errors) == 0) TRUE else throw(errors)
                  },

                  fabricate = function(SL.library) {
                    # Create objects for each of the objects
                    lapply(SL.library, function(entry) {
                             if (!private$isValidMlModel(entry)){
                               throw(paste('The model', entry, 'is not a valid ML model name'))
                             }

                             # TODO: Find a way to do this better.
                             constructor <- paste(entry, "$new()", sep = '')
                             eval(parse(text = constructor)) })
                  }
                  )
           )
