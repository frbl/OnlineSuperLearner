#' The libraryFactory can instantiate a set of models given to it.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @import purrr
#' @include ML.XGBoost.glm.R
#' @include ML.Local.lm.R
#' @section Methods:
#' \describe{
#'   \item{\code{new(ML.models.allowed = c('ML.H2O.glm', 'ML.Local.lm', 'ML.XGBoost.glm'))}}{This method is used to create object of this class. It expects a \code{ML.models.allowed} as a list which describes all of the models that are allowed for fabrication. }
#'
#'   \item{\code{getValidity()}}{
#'    Method to determine if the object is in a valid state.
#'   }
#'
#'   \item{\code{fabricate(SL.library)}}{
#'    Method that fabricates the models in the provided \code{SL.library}.
#'   }
#'}
LibraryFactory <- R6Class("LibraryFactory",
  private =
    list(
        ML.models.allowed = NULL,

        isValidLibraryGrid = function(SL.library.entry) {
          errors = c()
          if(!('algorithm' %in% names(SL.library.entry)))
            errors <- c(errors, 'Algorithm not specified')
          if(!('description' %in% names(SL.library.entry)))
            errors <- c(errors, 'Description not specified')
          errors
        },

        isValidMlModel = function(ML.name) {
          # TODO: Test if files actually exist.
          ML.name %in% private$ML.models.allowed
        },

        fabricateGrid = function(SL.library) {
          # Create objects for each of the objects
          instances <- lapply(SL.library, function(entry) {
            self$getEntryValidity(entry)

            # If no params are provided, treat the list as a vector
            if (!('params' %in% names(entry))){
              return(list(do.call(get(entry$algorithm)$new, 
                                  args = list())))
            }

            param.list <- entry$params %>% purrr::cross_d()
            data.table::setDT(param.list)
            lapply(1:nrow(param.list),
                    function(i) {
                      # Initialize the actual algorithms
                      do.call(get(entry$algorithm)$new, args = list(param.list[i])) 
                    })
          })

          unlist(instances)
        },

        fabricateDefault = function(SL.library) {
          # Create objects for each of the objects
          lapply(SL.library, function(entry) {
            if (!private$isValidMlModel(entry)){
              throw(paste('The model', entry, 'is not a valid ML model algorithm'))
            }
            do.call(get(entry)$new, args = list())
          })
        }
        ),

  public =
    list(
        initialize = function(ML.models.allowed = c('DensityEstimation',
                                                    'ML.H2O.gbm',
                                                    'ML.H2O.glm',
                                                    'ML.H2O.randomForest',
                                                    'ML.Local.lm',
                                                    'ML.XGBoost.glm')) {
          private$ML.models.allowed = ML.models.allowed
          self$getValidity()
        },

        getEntryValidity = function(entry) {
          if (!is.a(entry, 'list')) {
            return() 
          }

          errors = private$isValidLibraryGrid(entry)
          if (length(errors) != 0){
            throw(paste('The entry', entry, 'is not specified correctly:', errors ))
          }

          if (!private$isValidMlModel(entry$algorithm)){
            throw(paste('The model', entry$algorithm, 'is not a valid ML model algorithm'))
          }
        },

        getDescriptions = function(SL.library) {
          # If we only have a vector, that itself is the description. Otherwise
          # the list contains the descriptions
          if(is.a(SL.library, 'list')){
            SL.library <- sapply(SL.library, function(entry) entry$description )
          } 
          return(SL.library)
        },

        getValidity = function() {
          errors <- character()
          # Check if all models are actually ml models
          #are.ml.models <- sapply(private$ML.models.allowed, startsWith, 'ML')
          #if(!all(are.ml.models)){
            #msg <- 'Not all provided models are ML models as models should start with ML, please only use ML models.'
            #errors <- c(errors, msg)
          #}
          if (length(errors) == 0) return(TRUE) else throw(errors)
        },

        fabricate = function(SL.library) {
          # If we receive a list, assume that we have to create a parameter grid for each model
          if(is.a(SL.library, 'list')){
            fabricatedLibrary <- private$fabricateGrid(SL.library)
          } else {
            # if it is not a list, assume it is a vector or string
            SL.library <- Arguments$getCharacters(SL.library)
            fabricatedLibrary <- private$fabricateDefault(SL.library)
          }
          descriptions <- self$getDescriptions(SL.library)
          names(fabricatedLibrary) <- descriptions
          return(fabricatedLibrary)
        }
  )
)
