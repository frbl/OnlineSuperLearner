#' LibraryFactory
#'
#' @field ML.Local the type of platform to use for fitting the models
#' @importFrom R6 R6Class
#' @export
LibraryFactory <-
  R6Class (
           "LibraryFactory",
           private =
             list(
                  ML.models.allowed=NULL,
                  isValidMlModel = function(ML.name) {
                    ML.name %in% private$ML.models.allowed
                  }
                  ),

           public =
             list(
                  initialize = function(ML.models.allowed = c('ML.H2O.glm', 'ML.Local.lm')) {
                    private$ML.models.allowed = ML.models.allowed
                  },

                  getValidity = function(object) {
                    errors <- character()
                    # TODO: Test if files actually exist.

                    # Check if all models are actually ml models
                    are.ml.models <- sapply(object@ML.models.allowed, startsWith, 'ML')
                    if(!all(are.ml.models)){
                      msg <- 'Not all provided models are ML models as models should start with ML, please only use ML models.'
                      errors <- c(errors, msg)
                    }
                    if (length(errors) == 0) TRUE else errors
                  },

                  fabricate = function(SL.library, data) {
                    # Create objects for each of the objects
                    print(SL.library)
                    lapply(SL.library, function(entry) {
                             if (!private$isValidMlModel(entry)){
                               trow(paste('The model', entry,'is not a valid ML model name'))
                             }

                             # TODO: Find a way to do this better. We cannot call
                             # new since we are overriding the constructor.
                             # new(entry, data=data)
                             constructor <- paste(entry, "$new(data=data)",sep='')
                             eval(parse(text=constructor)) }
                    )
                  }
                  )
           )
