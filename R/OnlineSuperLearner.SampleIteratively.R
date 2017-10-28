#' OnlineSuperLearner.SampleIteratively
#'
#' @docType class
#' @importFrom R6 R6Class
OnlineSuperLearner.SampleIteratively <- R6Class("OnlineSuperLearner.SampleIteratively",
  public =
    list(
      initialize = function(osl, randomVariables, summary_measure_generator, remove_future_variables = FALSE, verbose = FALSE) {
        private$online_super_learner <- osl

        ## The variables need to be ordered when sampling from them
        private$random_variables <- RandomVariable.find_ordering(randomVariables)

        private$random_variable_names <- lapply(unname(self$get_random_variables), function(rv) rv$getY) %>% unlist
        private$summary_measure_generator <- summary_measure_generator
        private$remove_future_variables <- remove_future_variables
        private$verbose <- Arguments$getVerbose(verbose)
      },

      validate_parameters = function(start_from_variable, start_from_time, tau, discrete, return_type, intervention) {
        start_from_variable <- Arguments$getInstanceOf(start_from_variable, 'RandomVariable')
        randomVariables <- Arguments$getInstanceOf(randomVariables, 'list')
        tau <- Arguments$getNumerics(tau, c(1,Inf))
        start_from_time <- Arguments$getNumerics(start_from_time, c(1,tau))
        discrete <- Arguments$getLogical(discrete)

        ## Check whether the return type is one of the prespecified ones
        return_type <- Arguments$getCharacters(return_type)
        valid_return_types <- c('observations', 'full', 'summary_measures')
        if (!return_type %in% valid_return_types) {
          throw('Return type should be in', valid_return_types)
        }

        ## Check if the provided intervention is correct
        if(!is.null(intervention) && !InterventionParser.valid_intervention(intervention)) {
          throw('The intervention specified is not correct! it should have a
                when (specifying t), a what (specifying the intervention) and
                a variable (specifying the name of the variable to intervene on).')
        }
      },

      ## Samples the data iteratively from the fitted distribution, and
      ## applies an intervention if necessary. Note that some preliminary
      ## analysis showed that for this function approx 94% - 99% of time is
      ## spent in the predict function.
      ##
      ## @param data the data to start the sampling from
      ## @param randomvariables the list of randomvariables to use for the sampling procedure
      ## @param tau is the time at which we want to measure the outcome (default is 10)
      ## @param intervention is the intervention we want to give (default is null)
      ## @param discrete, boolean, should we use the discrete superlearner or the continous one
      ## @param return_type default is observations, should be one of
      ## observations, full, summary_measures. When observations, we only
      ## return the denormalized observations (not the summaries), when
      ## summary_measures, we only return the normalized summary_measured,
      ## and when full, we return both (normalized)
      ## @param start_from_variable the randomvariable to start the sampling from (default null)
      ## @param start_from_time the start time to start from (default 1)
      ## @param check boolean perform checks on the provided variables
      ## @param order_variables boolean should this function order the variables? This is neccessary, but can be done beforehand.
      sample_iteratively = function(data, tau = 10, intervention = NULL, discrete = TRUE,
                                    return_type = 'observations',
                                    start_from_variable = NULL,
                                    start_from_time = 1, check=TRUE) {

        ## If no random variable to start from is provided, just start from
        ## the first one. Note the ordering which is done at the top of this
        ## function.
        start_from_variable <- self$set_start_from_variable(start_from_variable)

        if(check) {
          ## Check whether the parameters are correct
          self$validate_parameters(
            start_from_variable = start_from_variable,
            start_from_time = start_from_time,
            tau = tau,
            return_type = return_type,
            discrete = discrete,
            intervention = intervention
          )
        }

        ## Initialize the tables in which we will store the results
        result <- data.table()
        result_denormalized_observations <- data.table(matrix(nrow=0, ncol = length(self$get_random_variables)))
        colnames(result_denormalized_observations) <- names(self$get_random_variables)

        ## We need to sample sequentially here, just so we can plugin the value everytime in the next evaluation
        private$verbose && enter(private$verbose, 'Started sampling procedure')
        for (t in seq(start_from_time, tau)) {
          current_block <- self$sample_single_block(
            current_time = t,
            start_from_variable = start_from_variable,
            data = data,
            intervention = intervention,
            discrete = discrete
          )

          ## Save the observations to the total list of observations. This is
          ## the list that eventually will be returned. We use the rbindlist
          ## function so the final result is a datatable.
          result_denormalized_observations <- rbindlist(
            list(result_denormalized_observations, current_block$denormalized),
            fill=TRUE
          )

          result <- rbindlist(
            list(result, current_block$normalized)
          )

          ## Now we have stored the new data in the dataframe, we can use it to get the new covariates
          if(t < tau) data <- private$get_latest_covariates(data)
        }

        private$verbose && exit(private$verbose)
        return(self$create_correct_result(
          result = result, 
          result_denormalized_observations = result_denormalized_observations,
          return_type = return_type
        ))
      },

      sample_single_block = function(current_time, start_from_variable, data, intervention, discrete) {
        current_denormalized_observation <- list()
        private$verbose && enter(private$verbose,'Sampling at ', current_time)

        if(nrow(data) > 1) warning('[sample_single_block] Only using the first block of data!')

        ## Just to be certain we don't use future data, we remove a subset:
        remove_vars <- self$get_random_variable_names
        started     <- FALSE

        for (rv in self$get_random_variables) {
          current_outcome <- rv$getY

          ## We move forward through the ordering till we find the variable we want to start from
          if (self$is_removing_future_variables && !started) {
            if (!equals(current_outcome, start_from_variable$getY)) {
              ## By default the remove_vars list contains all variables. However
              ## the current outcome lies in the past, so it might be that we
              ## need it later, don't remove it
              remove_vars <- remove_vars[remove_vars != current_outcome]
              next
            }
            ## Set the variables we don't need to NA
            data[current_time, remove_vars] <- NA
            started <- TRUE
          }

          ## Sample or intervene the current RV in the current block
          outcome <- self$sample_or_intervene_current_rv(
            data = data,
            intervention = intervention,
            current_time = current_time,
            current_rv = rv,
            discrete = discrete
          )

          ## Now we actually add the recently predicted value to the
          ## dataframe, so it can be used in the next sampling step. This
          ## is important!  We need to add the [[1]] because the result is
          ## a list of lists (1st norm/denorm, then estimator, then values)
          data[, (current_outcome) := as.numeric(outcome$normalized[[1]]) ]
          current_denormalized_observation[[current_outcome]] <- outcome$denormalized[[1]] %>% as.numeric
          private$verbose && exit(private$verbose)
        }
        private$verbose && exit(private$verbose)

        ## TODO: I don't think the as.data.table is necessary. Is it very slow?
        return(list(denormalized = as.data.table(current_denormalized_observation), normalized = data))
      },

      sample_or_intervene_current_rv = function(data, intervention, current_time, current_rv, discrete) {
        current_outcome <- current_rv$getY
        ## Here we select whether the current node is an intervention
        ## node.
        private$verbose && enter(private$verbose, 'Working on randomvariable ', current_outcome)
        parsed_intervention <- InterventionParser.parse_intervention(
          intervention = intervention,
          current_time = current_time,
          current_outcome = current_outcome
        )

        ## If the current t is an intervention t, apply the proposed intervention.
        if (parsed_intervention$should_intervene) {
          outcome <- self$perform_intervention(parsed_intervention = parsed_intervention)
        } else {
          outcome <- self$perform_sample(data = data, current_rv = current_rv, discrete = discrete)
        }
        outcome
      },

      perform_intervention = function(parsed_intervention) {
        outcome <- list(normalized = parsed_intervention$what, denormalized = parsed_intervention$what)
        private$verbose && cat(private$verbose, 
                               'Setting intervention ', parsed_intervention$what,
                               ' on time ', parsed_intervention$when)
        outcome
      },

      perform_sample = function(data, current_rv, discrete) {
        osl <- self$get_online_super_learner
        outcome <- osl$predict(
          data = data,
          randomVariables = c(current_rv),
          discrete = discrete,
          continuous = !discrete,
          all_estimators = FALSE,
          sample = TRUE
        )
        private$verbose && cat(private$verbose,'Predicted ', current_outcome,
                                ' using ', paste(current_rv$getX, collapse=', '),
                                ' and the prediction was ', outcome$denormalized[[1]])
        outcome
      },

      create_correct_result = function(result, result_denormalized_observations, return_type) {
        if (return_type == 'observations') {
          ## Return the denormalized observations?
          result <- (result_denormalized_observations[, self$get_random_variable_names, with = FALSE])
        } else if (return_type == 'summary_measures') {
          ## Return the denormalized observations?
          result <- (result[, !self$get_random_variable_names, with = FALSE])
        }
        return(result)
      },

      set_start_from_variable = function(start_from_variable = NULL) {
        if (is.null(start_from_variable)) start_from_variable <- self$get_random_variables[[1]]
        start_from_variable
      }
    ),
  active =
    list(
      get_online_super_learner = function() {
        return(private$online_super_learner)
      },

      is_removing_future_variables = function(){
        return(private$remove_future_variables)
      },

      get_random_variables = function() {
        private$random_variables
      },

      get_random_variable_names = function() {
        private$random_variable_names 
      },

      get_summary_measure_generator = function() {
        private$summary_measure_generator
      }
    ),
  private =
    list(
      online_super_learner = NULL,
      random_variables = NULL,
      random_variable_names = NULL,
      remove_future_variables = NULL,
      summary_measure_generator = NULL,
      verbose = NULL,
      get_latest_covariates = function(data) {
        self$get_summary_measure_generator$getLatestCovariates(data)
      }
    )
)

