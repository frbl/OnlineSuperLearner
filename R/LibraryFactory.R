#' The libraryFactory can instantiate a set of models given to it.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import data.table
#' @import purrr
#' @include ML.XGBoost.R
#' @include ML.Local.lm.R
#' @section Methods:
#' \describe{
#'   \item{\code{new(ML.models.allowed = c('ML.H2O.glm', 'ML.Local.lm', 'ML.XGBoost.glm'))}}{This method is used to create object of this class. It expects a \code{ML.models.allowed} as a list which describes all of the models that are allowed for fabrication. }
#'
#'   \item{\code{get_validity}}{
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
        verbose = NULL,

        is_valid_ml_model = function(ML.name) {
          # TODO: Test if files actually exist.
        },

        create_estimator_grid = function(entry) {
            name <- entry$algorithm

            # If no params are provided, treat the list as a vector
            if (!('algorithm_params' %in% names(entry))){
              name <- paste(name, 'vanilla', sep = '-')
              algorithm_instances <- list(create_object_from_string(entry$algorithm, args = list()))
              names(algorithm_instances) <- name
            } else {
              algorithm_param_list <- entry$algorithm_params %>% purrr::cross_d()
              data.table::setDT(algorithm_param_list)

              # Initialize the actual algorithms
              # We iterate over the numbers, we we still have a datatable when indexing.
              algorithm_instances <- lapply(1:nrow(algorithm_param_list), function(i) {
                current_params <- list()
                params_name <- paste(name, 'vanilla' , sep='-')
                if(nrow(algorithm_param_list[i]) > 0) {
                  current_params <- as.list(algorithm_param_list[i])
                  params_name <- paste(names(current_params), current_params, collapse='_',sep='-') %>%
                    paste(name, ., sep='-')
                }
                result <- list(create_object_from_string(entry$algorithm, args = current_params))
                names(result) <- params_name
                result
              })
              algorithm_instances <- unlist(algorithm_instances)
            }
            algorithm_instances
        },

        create_density_estimator_grid = function(entry, algorithm_instances) {
          result <- lapply(algorithm_instances, function(algorithm_instance) {
              # We shouldn't do gridsearch when no parameters are specified, just use the defaults
              if (!('params' %in% names(entry))){
                result <- list('vanillaDE' = DensityEstimation$new(bin_estimator = algorithm_instance))
              } else {
                param_list <- entry$params %>% purrr::cross_d()
                data.table::setDT(param_list)

                # We iterate over the numbers, we still keep the correct names when we pass the list
                result <- lapply(1:nrow(param_list), function(i) {
                  # TODO: Extract this to a function
                  current_params <- list()
                  params_name <- 'vanillaDE'
                  if(nrow(param_list[i]) > 0) {
                    current_params <- as.list(param_list[i])
                    params_name <- paste(names(current_params), current_params, collapse='_',sep='-')
                  }

                  current_params$bin_estimator <- algorithm_instance
                  current_params$verbose <- private$verbose
                  result <- list(create_object_from_string('DensityEstimation', args = current_params))
                  names(result) <- params_name
                  result
                })
                result <- unlist(result)
              }
              result
            })
            unlist(result)
        },

        fabricateGrid = function(SL.library) {
          #SL.library <- Arguments$getList(SL.library)
          # We have to fabricate a grid on two levels, 1st for the algorithm, then for the density estimation
          # algotithm_params contains the params for the algorithm
          # params contains the params for the density estimation

          # Create objects for each of the objects
          #naming:
          # algorithmname-paramname-param
          instances <- lapply(SL.library, function(entry) {
            self$check_entry_validity(entry)
            algorithm_instances <- private$create_estimator_grid(entry)
            result <- private$create_density_estimator_grid(entry, algorithm_instances)
          })
          unlist(instances)
        },

        fabricateDefault = function(SL.library) {
          SL.library <- Arguments$getCharacters(SL.library)

          # Create objects for each of the objects
          fabricatedLibrary <- lapply(SL.library, function(entry) {
            self$check_entry_validity(entry)

            bin_estimator <- create_object_from_string(entry, args = list())
            result <- create_object_from_string('DensityEstimation',
                                                args = list(bin_estimator = bin_estimator))
          })
          names(fabricatedLibrary) <- SL.library
          return(fabricatedLibrary)
        }
        ),
  active =
    list(
        get_validity = function() {
          errors <- character()
          # Check if all models are actually ml models
          #are.ml.models <- sapply(private$ML.models.allowed, startsWith, 'ML')
          #if(!all(are.ml.models)){
            #msg <- 'Not all provided models are ML models as models should start with ML, please only use ML models.'
            #errors <- c(errors, msg)
          #}
          if (length(errors) == 0) return(TRUE) else throw(errors)
        }
        ),
  public =
    list(
        initialize = function(ML.models.allowed = c('condensier::speedglmR6',
                                                    'condensier::glmR6',
                                                    'ML.H2O.gbm',
                                                    'ML.H2O.glm',
                                                    'ML.GLMnet',
                                                    'ML.H2O.randomForest',
                                                    'ML.Local.Speedlm',
                                                    'ML.Local.lm',
                                                    'ML.XGBoost'), verbose = FALSE) {
          private$ML.models.allowed = ML.models.allowed
          private$verbose <- verbose
          self$get_validity
        },

        fabricate = function(SL.library) {
          # If we receive a list, assume that we have to create a parameter grid for each model
          if(is.a(SL.library, 'list')){
            fabricatedLibrary <- private$fabricateGrid(SL.library)
          } else {
            # if it is not a list, assume it is a vector or string
            fabricatedLibrary <- private$fabricateDefault(SL.library)
          }
          return(fabricatedLibrary)
        },

        check_entry_validity = function(SL.library.entry) {
          errors = c()
          if (is.a(SL.library.entry, 'list')) {
            allowed_entries <- c('algorithm', 'params', 'algorithm_params')
            for(name in names(SL.library.entry)) {
              if(!(name %in% allowed_entries)) {
                errors <- c(errors, paste('Entry in SL specification:', name,'not supported!'))
              }
            }

            if('algorithm' %in% names(SL.library.entry)) {
              if (!(SL.library.entry$algorithm %in% private$ML.models.allowed)) {
                errors <- c(errors, paste('The model', SL.library.entry$algorithm, 'is not a valid ML model algorithm'))
              }
            } else {
              errors <- c(errors, 'Algorithm not specified')
            }

            check_valid_params <- function(SL.library.entry, entry_name) {
              errors <- c()
              if(entry_name %in% names(SL.library.entry)) {
                if (!is(SL.library.entry[[entry_name]], 'list')){
                  errors <- c(errors, paste('The', entry_name, 'entry should be a list'))
                } else if (is.null(SL.library.entry[[entry_name]])){
                  errors <- c(errors, paste('If you add a',entry_name,'entry, also add params to it.'))
                } else if (length(SL.library.entry[[entry_name]]) == 0){
                  errors <- c(errors, paste('If you add a',entry_name,'entry, also add params to it.'))
                } else if (!all(unlist(lapply(SL.library.entry[[entry_name]], length)) > 0)) {
                  errors <- c(errors, paste('If you add a',entry_name,'entry, also add params to it.'))
                }
              }
              errors
            }
            errors <- c(errors, check_valid_params(SL.library.entry, 'params'))
            errors <- c(errors, check_valid_params(SL.library.entry, 'algorithm_params'))

          } else if (is.character(SL.library.entry)) {
            if (!(SL.library.entry %in% private$ML.models.allowed)) {
              errors <- c(errors, paste('The model', SL.library.entry, 'is not a valid ML model algorithm'))
            }
          } else {
            errors <- c(errors, 'Entry is not a character nor a list')
          }

          if (length(errors) != 0){
            message <- paste('The entry', paste(SL.library.entry, collapse=' '), 'is not specified correctly:', errors)
            throw(message)
          }
        }
  )
)
