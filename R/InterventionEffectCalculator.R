#' InterventionEffectCalculator
#'
#' Class that can be used to calculate the effect of a specific intervention.
#' For the specification of how to define interventions, look at the
#' \code{InterventionParser} class.
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @include InterventionParser.R
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(bootstrap_iterations,  outcome_variable}}{ 
#'     Creates a new InterventionEffectCalculator.
#'     
#'     @param bootstrap_iterations integer the number of bootstrap iterations
#'      to use when calculating the effect of an intervention. If there are
#'      more iterations, the result will be more reliable, but calculating will
#'      take significantly longer.
#' 
#'     @param outcome_variable string the name of the variable for which the
#'      outcome should be measured. This is usualy Y (depending on you setup).
#' 
#'     @param parallel (default = TRUE) should the effect calculator run in a
#'      multithreaded / multicore way?
#' 
#'     @param verbose (default = FALSE) the verbosity to use when running the
#'      intervention calculation
#'   } 
#' 
#'   \item{\code{calculate_intervention_effect(osl, interventions, discrete, initial_data, tau, check = FALSE) }}{ 
#'     Calculates the actual effect of a set of interventions. This function
#'     will call the \code{evaluate_single_intervention} for each of the
#'     interventions in the list of interventions passed to this functions.
#'     When calling this function one can specify whether to use the discrete
#'     OSL (\code{discrete = TRUE}) or whether to uyse the contionous OSL
#'     (\code{discrete = FALSE}). 
#'
#'     @param osl OnlineSuperLearner object the fitted
#'      \code{OnlineSuperLearner} instance. This instance is used to perform
#'      the predictions with.
#' 
#'     @param interventions list a list of interventions to use when
#'      calculating the effect of the interventions. Each of these interventions
#'      is processed in sequence and the result is stored in a list.
#' 
#'     @param discrete boolean should the discrete superlearner (or the cts
#'      superlearner) be used?
#' 
#'     @param initial_data data.table the first row / set of blocks needed to
#'      initialize the intervention estimation algorithm. This block needs to
#'      be large enough to provide enough historical data for all summary
#'      measures.
#' 
#'     @param tau integer the time t at which we'd like to measure the effect
#'      of the intervention
#' 
#'     @param check (default = FALSE) boolean, should the input parameters be
#'      checked for correctness? This might make the process slighly slower.
#' 
#'     @return a list with intervention effects for each of the specified
#'      interventions. Each intervention entry then contains a vector of
#'      intervention effects.
#'   } 
#' 
#'   \item{\code{evaluate_single_intervention(osl, initial_data, intervention, tau, discrete) }}{ 
#'     Calculates the effect of a single intervention. The specification of the
#'     intervention needs to comply with the definition in the
#'     \code{InterventionParser} class. One can specify whether or not to use
#'     the discrete super learner by toggling the \code{discrete} argument.
#'
#'     @param osl OnlineSuperLearner object the fitted
#'      \code{OnlineSuperLearner} instance. This instance is used to perform
#'      the predictions with.
#'
#'     @param initial_data data.table the first row / set of blocks needed to
#'      initialize the intervention estimation algorithm. This block needs to
#'      be large enough to provide enough historical data for all summary
#'      measures.
#'
#'     @param intervention an intervention to run on the data.
#'
#'     @param tau integer the time t at which we'd like to measure the effect
#'      of the intervention
#'
#'     @param discrete boolean should the discrete superlearner (or the cts
#'      superlearner) be used?
#'
#'     @return a vector of intervention effects for each iteration of
#'      intervention.
#'   } 
#'
#'   \item{\code{perform_initial_estimation(data, intervention, tau) }}{ 
#'     This function can be used to generate an initial estimation, calculated
#'     using the plain OSL. This method then returns a value given the provided
#'     data, tau, and intervention. Essentially, this does the same as the
#'     \code{calculate_intervention_effect} function, but returns the mean of
#'     all intervention bootstraps.
#'
#'     @param data.table the data to seed the sampling procedure.
#'
#'     @param intervention the intervention itself, see
#'      \code{InterventionParser} for more details
#'
#'     @param tau integer the time at which we want to evaluate the intervention
#'
#'     @return double the estimated mean over all bootstrap iterations
#'   } 
#'
#'   \item{\code{is_parallel() }}{ 
#'     Active method. Function to check whether the current object runs in
#'     parallel or not.
#' 
#'     @return boolean \code{TRUE} if parallel.
#'   } 
#'
#'   \item{\code{get_bootstrap_iterations() }}{ 
#'     Active method. Returns the number of bootstrap iterations specified.
#'
#'     @return integer the number of bootstrap iterations.
#'   } 
#'
#'   \item{\code{get_relevant_variables) }}{ 
#'     Active method. The relevant variables specified when intializing the
#'     object.
#'
#'     @return list of \code{RelevantVariable} objects.
#'   } 
#'
#'   \item{\code{get_outcome_variable() }}{ 
#'     Active method. Returns the outcome variable specified on initialization
#'     (string).
#'
#'     @return string the string representation of the name of the outcome
#'      variable.
#'   } 
#' }  
#' @export
InterventionEffectCalculator <- R6Class("InterventionEffectCalculator",
  public =
    list(
      initialize = function(bootstrap_iterations, outcome_variable, parallel = TRUE, verbose = FALSE) {
        private$bootstrap_iterations <- Arguments$getInteger(bootstrap_iterations, c(1, Inf))
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

        ## Note that this won't work when we have an H2O estimator in the set.
        ## The parallelization will fail.
        result <- foreach(i=seq(self$get_bootstrap_iterations), .combine=rbind) %looping_function% {
          private$verbose && cat(private$verbose, 'Approximation iteration for:', i)
          drawn_sample <- osl$sample_iteratively(
            data = initial_data,
            intervention = intervention,
            discrete = discrete,
            tau = tau
          )
          drawn_sample[tau, self$get_outcome_variable, with=FALSE]
        } %>% unlist %>% unname
      },

      perform_initial_estimation = function(osl, interventions, discrete, initial_data, tau) {
        private$verbose && enter(private$verbose, 'Performing initial estimation of parameter of interest')

        initial_data <- Arguments$getInstanceOf(initial_data, 'data.table')

        result <- self$calculate_intervention_effect(
          osl = osl,
          interventions = interventions,
          discrete = discrete,
          initial_data = initial_data,
          tau = tau,
          check = TRUE
        )
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
      get_outcome_variable = function() {
        private$outcome_variable
      }
    ),
  private =
    list(
      parallel = NULL,
      bootstrap_iterations = NULL,
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
