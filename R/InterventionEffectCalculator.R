#' InterventionEffectCalculator
#'   \item{\code{perform_initial_estimation(data, intervention, tau) }}{ 
#'     This function can be used to generate an initial estimation, calculated using the plain OSL. This method then
#'     returns a value given the provided data, tau, and intervention.
#'     @param data the data to seed the sampling procedure.
#'     @param intervention the intervention itself, see \code{InterventionParser} for more details
#'     @param tau the time at which we want to evaluate the intervention
#'   } 
#' @docType class
#' @importFrom R6 R6Class
#' @import magrittr
InterventionEffectCalculator <- R6Class("InterventionEffectCalculator",
  public =
    list(
      initialize = function(bootstrap_iterations, randomVariables, outcome_variable, parallel = TRUE, verbose = FALSE) {
        private$bootstrap_iterations <- Arguments$getInteger(bootstrap_iterations, c(1, Inf))
        private$randomVariables <- Arguments$getInstanceOf(randomVariables,'list')
        private$outcome_variable <- Arguments$getCharacters(outcome_variable)
        private$parallel <- Arguments$getLogical(parallel)
        private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
      },

      calculate_intervention_effect = function(osl, interventions, discrete, initial_data, tau, check = FALSE) {
        if (check) {
          for (intervention in interventions) {
            assert_that(InterventionParser.valid_intervention(intervention = intervention))
          }
          osl <- Arguments$getInstanceOf(osl, 'OnlineSuperLearner')
        }

        if (is.null(names(interventions))) {
          warning('Provided interventions do not have a name. Please name them for data management. Continuing without any names.')
        }

        private$verbose && enter(private$verbose, 'Starting evaluation for ', ifelse(discrete, 'discrete', 'continuous'), ' superlearner.')

        result <- lapply(interventions, function(intervention) {
          private$verbose && cat(private$verbose, 'Runninng new intervention')
          self$evaluate_single_intervention(
            osl = osl,
            initial_data = initial_data,
            intervention = intervention,
            tau = tau,
            discrete = discrete
          )
        })
        private$verbose && exit(private$verbose)
        result
      },

      evaluate_single_intervention = function(osl, initial_data, intervention, tau, discrete) {
        `%looping_function%` <- private$get_looping_function()

        if(self$is_parallel) {
          private$verbose && cat(private$verbose, 'Approximating estimation (under intervention, in parallel)')
        } else {
          private$verbose && cat(private$verbose, 'Approximating estimation (under intervention, not parallel)')
        }

        # Note that this won't work when we have an H2O estimator in the set. The parallelization will fail.
        result <- foreach(i=seq(self$get_bootstrap_iterations), .combine=rbind) %looping_function% {
          private$verbose && cat(private$verbose, 'Approximation iteration for:', i)
          sample <- osl$sample_iteratively(
            data = initial_data,
            randomVariables = self$get_random_variables,
            intervention = intervention,
            discrete = discrete,
            tau = tau
          )
          sample[tau, self$get_outcome_variable, with=FALSE]
        } %>% unlist %>% unname
      },

      perform_initial_estimation = function(osl, interventions, discrete, initial_data,  tau) {
        private$verbose && enter(private$verbose, 'Performing initial estimation of parameter of interest')

        result <- self$calculate_intervention_effect(osl, interventions, discrete, initial_data, tau)
        result <- lapply(result, mean)

        private$verbose && exit(private$verbose)
        result
      }

    ),
  active =
    list(
      is_parallel = function() {
        private$parallel
      },
      get_bootstrap_iterations = function() {
        private$bootstrap_iterations
      },
      get_random_variables = function() {
        private$randomVariables
      },
      get_outcome_variable = function() {
        private$outcome_variable
      }
    ),
  private =
    list(
      parallel = NULL,
      bootstrap_iterations = NULL,
      randomVariables = NULL,
      outcome_variable = NULL,
      verbose = NULL,

      get_looping_function = function() {
        if(self$is_parallel) {
          return(`%dopar%`)
        }
        return(`%do%`)
      }

    )
)
