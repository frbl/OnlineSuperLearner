#' The libraryFactory can instantiate a set of models given to it.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom purrr cross_df
#' @include ML.H2O.R
#' @include ML.XGBoost.R
#' @include ML.NeuralNet.R
#' @include ML.Local.lm.R
#' @include ML.randomForest.R
#' @include ML.SVM.R
#' @section Methods:
#' \describe{
#'   \item{\code{new(ML.models.allowed = c('ML.H2O.glm', 'ML.Local.lm', 'ML.XGBoost.glm'))}}{
#'     This method is used to create object of this class. It expects a \code{ML.models.allowed} as a list which describes all of the models that are allowed for fabrication. 
#'     @param ML.models.allowed = the list of ML models allowed to create objects for. The default is a list of all models in the OSL package
#'     @return a new instance of the library factory
#'   }
#'
#'   \item{\code{get_validity}}{
#'     Method to determine if the object is in a valid state. The method will throw whenever the state is not valid
#'   }
#'
#'   \item{\code{fabricate(SL.library)}}{
#'     Method that fabricates the models in the provided \code{SL.library}.
#'     @param SL.library can be either a list of ML models, for which we will then choose the default hyper parameters,
#'     or a more specific specification of the models. This is in the form of a list and should be specified as follows:
#'     \code{list(list(algorithm = 'the class of the algorithm',
#'                     algorithm_params = list(hyper_parameter1=c(1,2,3)),
#'                     params = list(nbins = c(39, 40), online = FALSE))))} in which \code{algorithm_params} are the
#'                     hyperparameters for the learner, and params are the hyper parameters for the density estimator.
#'   }
#'}
LibraryFactory <- R6Class("LibraryFactory",
  public =
    list(
        initialize = function(ML.models.allowed = c('condensier::speedglmR6',
                                                    'condensier::glmR6',
                                                    'ML.H2O.gbm',
                                                    'ML.H2O.glm',
                                                    'ML.GLMnet',
                                                    'ML.H2O.randomForest',
                                                    'ML.randomForest',
                                                    'ML.SVM',
                                                    'ML.Local.Speedlm',
                                                    'ML.Local.lm',
                                                    'ML.NeuralNet',
                                                    'ML.XGBoost'), verbose = FALSE) {
          private$allowed_ml_models = ML.models.allowed
          private$verbose <- Arguments$getVerbose(verbose)
          self$get_validity
        },

        fabricate = function(SL.library) {
          ## If we receive a list, assume that we have to create a parameter grid for each model
          if(is.a(SL.library, 'list')){
            fabricatedLibrary <- self$fabricate_grid(SL.library)
          } else {
            ## if it is not a list, assume it is a vector or string
            fabricatedLibrary <- self$fabricate_default(SL.library)
          }

          # Set a name in each of the fabricated estimators.
          self$inject_names_in_estimators(fabricatedLibrary)
          return(fabricatedLibrary)
        },

        fabricate_grid = function(SL.library) {
          ## We have to fabricate a grid on two levels, 1st for the algorithm, then for the density estimation
          ## algotithm_params contains the params for the algorithm
          ## params contains the params for the density estimation

          ## Create objects for each of the objects
          ##naming:
          ## algorithmname-paramname-param
          instances <- lapply(SL.library, function(entry) {
            self$check_entry_validity(entry)
            algorithm_instances <- self$fabricate_single_estimator_from_grid(entry)
            result <- self$fabricate_single_density_estimator_from_grid(entry, algorithm_instances)
          })
          unlist(instances)
        },

        fabricate_single_estimator_from_grid = function(entry) {
            name <- entry$algorithm

            ## If no params are provided, treat the list as a vector
            if (!('algorithm_params' %in% names(entry))){
              name <- paste(name, 'vanilla', sep = '-')
              algorithm_instances <- list(create_object_from_string(entry$algorithm, args = list()))
              names(algorithm_instances) <- name
            } else {
              algorithm_param_list <- entry$algorithm_params %>% purrr::cross_df()
              data.table::setDT(algorithm_param_list)

              ## Initialize the actual algorithms
              ## We iterate over the numbers, we we still have a datatable when indexing.
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

        fabricate_single_density_estimator_from_grid = function(entry, algorithm_instances) {
          result <- lapply(algorithm_instances, function(algorithm_instance) {
              ## We shouldn't do gridsearch when no parameters are specified, just use the defaults
              if (!('params' %in% names(entry))){
                result <- list('vanillaDE' = DensityEstimation$new(bin_estimator = algorithm_instance))
              } else {
                param_list <- entry$params %>% purrr::cross_df()
                data.table::setDT(param_list)

                ## We iterate over the numbers, we still keep the correct names when we pass the list
                result <- lapply(1:nrow(param_list), function(i) {
                  ## TODO: Extract this to a function
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

        fabricate_default = function(SL.library) {
          SL.library <- Arguments$getCharacters(SL.library)

          ## Create objects for each of the objects
          fabricatedLibrary <- lapply(SL.library, function(entry) {
            self$check_entry_validity(SL.library.entry = entry)

            bin_estimator <- create_object_from_string(entry, args = list())
            result <- create_object_from_string('DensityEstimation',
                                                args = list(bin_estimator = bin_estimator))
          })
          names(fabricatedLibrary) <- SL.library
          return(fabricatedLibrary)
        },

        check_entry_validity = function(SL.library.entry) {
          errors = c()
          if (is.a(SL.library.entry, 'list')) {
            errors <- self$check_entry_validity_of_list(SL.library.entry)
          } else if (is.character(SL.library.entry)) {
            errors <- self$check_entry_validity_of_character(SL.library.entry)
          } else {
            errors <- c(errors, 'Entry is not a character nor a list')
          }

          if (length(errors) != 0){
            message <- paste('The entry', paste(SL.library.entry, collapse=' '), 'is not specified correctly:', errors)
            throw(message)
          }
        }, 

        check_entry_validity_of_character = function(SL.library.entry) {
          errors = c()
          if (!(SL.library.entry %in% self$get_allowed_ml_models)) {
            errors <- c(errors, paste('The model', SL.library.entry, 'is not a valid ML model algorithm'))
          }
          errors
        },

        check_entry_validity_of_list = function(SL.library.entry) {
          errors = c()

          ## Check the arguments in the entry
          allowed_entries <- c('algorithm', 'params', 'algorithm_params')
          for(name in names(SL.library.entry)) {
            if(!(name %in% allowed_entries)) {
              errors <- c(errors, paste('Entry in SL specification:', name,'not supported!'))
            }
          }

          ## Check the algorithm validity
          if('algorithm' %in% names(SL.library.entry)) {
            if (!(SL.library.entry$algorithm %in% self$get_allowed_ml_models)) {
              errors <- c(errors, paste('The model', SL.library.entry$algorithm, 'is not a valid ML model algorithm'))
            }
          } else {
            errors <- c(errors, 'Algorithm not specified')
          }

          ## Create inner function, as this one is both used for the params and the algorithm params
          check_valid_params <- function(SL.library.entry, entry_name) {
            errors <- c()
            if(entry_name %in% names(SL.library.entry)) {
              if (!is(SL.library.entry[[entry_name]], 'list')){
                errors <- c(errors, paste('The', entry_name, 'entry should be a list'))
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
        },

        inject_names_in_estimators = function(fabricatedLibrary) {
          # Not a very elegant nor efficient solution, but clear and simple
          for (i in seq_along(fabricatedLibrary)) {
            fabricatedLibrary[[i]]$set_name(names(fabricatedLibrary)[[i]])
          }
        }
  ),
  active =
    list(
        get_validity = function() {
          errors <- character()
          ## Check if all models are actually ml models
          ##are.ml.models <- sapply(private$allowed_ml_models, startsWith, 'ML')
          ##if(!all(are.ml.models)){
            ##msg <- 'Not all provided models are ML models as models should start with ML, please only use ML models.'
            ##errors <- c(errors, msg)
          ##}
          if (length(errors) == 0) return(TRUE) else throw(errors)
        },

        get_allowed_ml_models = function() {
          return(private$allowed_ml_models)
        }
        ),
  private =
    list(
        allowed_ml_models = NULL,
        verbose = NULL,

        is_valid_ml_model = function(ML.name) {
          ## TODO: Test if files actually exist.
        }
    )
)
