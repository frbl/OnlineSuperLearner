#' OnlineSuperLearner.SampleIteratively
#'
#' This class offers the functionality to sample from a fitted
#' \code{OnlineSuperLearner} instance. This is also the class used when calling
#' the \code{sampleIteratively} function on that class. 
#'
#' @docType class
#' @importFrom R.oo equals
#' @importFrom R6 R6Class
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(osl, randomVariables, summary_measure_generator, remove_future_variables = FALSE, verbose = FALSE)}}{ 
#'     Creates a new instance of the OSL sampleiteratively class. When creating
#'     a new instance, we expect to receive the fitted instance of the OSL, as
#'     it is used to sample data from. This instance is stored so you don't
#'     have to provide it with every sampling call. If you update the OSL, this
#'     instance should be recreated with the correct osl and attributes.
#'
#'     @param osl OnlineSuperLearner a fitted instance of the
#'      \code{OnlineSuperLearner} class.
#'
#'     @param randomVariables list a list of \code{RandomVariable} objects.
#'      These are the randomVariables we have fit a conditional density for.
#'
#'     @param summaryMeasureGenerator SummaryMeasureGenerator an object of the type
#'      SummaryMeasureGenerator. This generator is used to get new observations
#'      with the correct aggregated columns.
#'
#'     @param remove_future_variables boolean (default = FALSE) it is possible
#'      to remove variables that are in the future. These can be automatically
#'      removed by setting this variable to \code{TRUE}. Removing future
#'      variables is an extra check on the code. What it actually does is when
#'      the sampling procedure starts, it will check which data is needed for
#'      the first run, and remove all data that is not needed (and should be
#'      sampled). If we made a mistake, and for example use a future value, this
#'      can be detected by setting this variable to \code{TRUE}. The only reason
#'      for setting this to \code{FALSE} is because it might make the sampling
#'      procedure a bit slower (but probably not significantly).
#'
#'     @param verbose (default = FALSE) the verbosity (how much logging). Note that this might
#'      be propagated to other classes.
#'   } 
#' 
#'   \item{\code{validate_parameters(start_from_variable, start_from_time, tau, discrete, return_type, intervention)}}{ 
#'     A function that could help to validate the provided variables. This
#'     function is called from the sample functions whenever the corresponding
#'     \code{check} parameters are \code{TRUE}.
#'
#'     @param start_from_variable RandomVariable should be an instance of a
#'      \code{RandomVariable} class.
#'
#'     @param start_from_time integer should be an integer value $x$ where
#'      $1 <= x < tau$.
#'
#'     @param tau integer a value when the intervention is measured. Should be
#'      greater or equal to 1.
#'
#'     @param discrete boolean should we run the discrete superlearner? should
#'      be in \code{c(TRUE, FALSE)}.
#'
#'     @param return_type string we use a string to specify different return
#'      types of our sampling algorithm.  A return type should be either one of
#'      \code{c('observations', 'full', 'summary_measures')}. Each of them cause
#'      a different set of data to be returned from the sampling procedure. See
#'      the \code{sample_iteratively} function for more details.
#'
#'     @param intervention list (an intervention instance) each intervention
#'      provided to this (and any other class) should follow the intervention
#'      specification guidelines in the \code{InterventionParser} class.
#'   } 
#' 
#'   \item{\code{sample_iteratively(data, tau = 10, intervention = NULL, discrete = TRUE, return_type = 'observations', start_from_variable = NULL, start_from_time = 1, check=TRUE)}}{ 
#'     A function to actually sample from the \code{OnlineSuperLearner}, and
#'     applies an intervention if necessary.  This function will grasp the OSL
#'     from the instance variable of the object and runs (iteratively) a
#'     sampling procedure. That is, it starts from the first variable (or
#'     \code{start_from_variable}) at time 1 (or time \code{start_from_time})
#'     and samples the next variable, and the next, until it reaches the last
#'     variable in the series. It will then overflow to the next unit of time,
#'     until it reaches $tau$, the time at which the intervention should be
#'     recorded.  Note that some preliminary  analysis showed that for this
#'     function approx 94% - 99% of time is  spent in the predict function.
#'
#'     @param data data.table the initial data to initialize the sampling
#'      procedure with ($O_0$).
#'
#'     @param tau integer (default = 10) the time at which one wants to measure
#'      the outcome of the sampling procedure.
#'
#'     @param intervention list (default = NULL) (Should be an intervention
#'     instance) each intervention provided to this (and any other class)
#'     should follow the intervention specification guidelines in the
#'     \code{InterventionParser} class.
#'
#'     @param discrete boolean (default = TRUE) should we run the discrete (=
#'      \code{TRUE}) or the cts superlearner (= \code{FALSE}).
#'
#'     @param return_type default is observations, should be one of
#'     observations, full, summary_measures. When observations, we only
#'     return the denormalized observations (not the summaries), when
#'     summary_measures, we only return the normalized summary_measured,
#'     and when full, we return both (normalized and denormalized)
#'
#'     @param start_from_variable RandomVariable (default = NULL) the
#'     randomvariable to start the sampling from. When \code{NULL}, we'll just
#'     start from the first in the sequence.
#'
#'     @param start_from_time the start time to start from (default 1)
#'
#'     @param check boolean (default = TRUE) perform checks on the provided
#'      variables and shows if they are valid.
#'
#'     @return data.table a \code{data.table} with the sampled values. The size
#'      and shape of the \code{data.table} differs according to the
#'      \code{return_type} specified.
#'   } 
#' 
#'   \item{\code{sample_single_block(current_time, start_from_variable, data, intervention)}}{ 
#'     Samples a single block from the \code{OnlineSuperLearner}. This function
#'     is used internaly in the \code{sample_iteratively} function, and samples
#'     a single block (instead of a full series).
#'
#'     @param current_time integer because we sample a single block, we need to
#'      know where we are in the sampling process. This variable representes
#'      the current time in the sampling process.
#'
#'     @param start_from_variable RandomVariable the \code{RandomVariable} from
#'      which we should start the sampling procedure.
#'
#'     @param data data.table the current data needed to initialize the
#'      sampling of this block.
#'
#'     @param intervention list the intervention that might (or might not) be
#'      used in the sampling of the present block. This takes into account the
#'      actual current time provided to this function. If the intervention
#'      specified should be performed at a different time, no intervention is
#'      performed for now. See for the correct specification of this element the
#'      \code{InterventionParser} class
#'
#'      @return list of data.tables with the sampled block, both normalized and
#'       denormalized.
#'   } 
#' 
#'   \item{\code{sample_or_intervene_current_rv(data, intervention, current_time, current_rv, discrete)}}{ 
#'     On an even lower level (a lower level than sampling a block), we need to
#'     sample \code{RandomVariables}. This is the heart of the sampling
#'     procedure, and is used to sample (if the current time and RV are not an
#'     intervention node) or intervene (if they are) and get a new value for
#'     the next random variable in line.
#'
#'     @param data data.table the current data needed to initialize the
#'      sampling of the randomvariable.
#'
#'     @param intervention list the intervention that might (or might not) be
#'      used in the sampling of the present block. This takes into account the
#'      actual current time provided to this function. If the intervention
#'      specified should be performed at a different time, no intervention is
#'      performed for now. See for the correct specification of this element the
#'      \code{InterventionParser} class
#'
#'     @param current_time integer because we sample a single block, we need to
#'      know where we are in the sampling process. This variable representes
#'      the current time in the sampling process.
#'
#'     @param current_rv RandomVariable the \code{RandomVariable} we are
#'      currently working with.
#'
#'     @param discrete boolean (default = TRUE) should we run the discrete (=
#'      \code{TRUE}) or the cts superlearner (= \code{FALSE}).
#'
#'     @return list containing a noramlized and denormalized value.
#'   } 
#' 
#'   \item{\code{perform_intervention(parsed_intervention)}}{ 
#'     Instead of sampling data, this function takes care of setting an
#'     intervention.
#'
#'     @param parsed_intervention list (intervention instance) the intervention
#'      to perform on the data.
#'   } 
#' 
#'   \item{\code{perform_sample(data, current_rv, discrete) }}{ 
#'     Actually runs the prediction and performs the sample from the OSL.
#'
#'     @param data data.table the current data needed to initialize the
#'      sampling of the randomvariable.
#'
#'     @param current_rv RandomVariable the \code{RandomVariable} we are
#'      currently working with.
#'
#'     @param discrete boolean (default = TRUE) should we run the discrete (=
#'      \code{TRUE}) or the cts superlearner (= \code{FALSE}).
#'
#'     @return list the normalized and denormalized outcome.
#'   } 
#' 
#'   \item{\code{create_correct_result(result, result_denormalized_observations, return_type) }}{ 
#'     As described in the \code{sample_iteratively} function, we provide a
#'     result based on a return type. This function actually takes care of
#'     transforming the full result in this specified return type result.
#'
#'     @param result data.table the result after running a sampling procedure
#'      (the normalized results)
#'
#'     @param result_denormalized_observations data.table the result after
#'      running a sampling procedure (the denormalized results)
#'
#'     @param return_type the specification which return_type one wants.
#'
#'     @return data.table the \code{data.table} containing the results
#'      requested.
#'   } 
#' 
#'   \item{\code{set_start_from_variable(start_from_variable = NULL) }}{ 
#'     Small helper function to set the start_from variable in the
#'     \code{sample_iteratively} function. If no variable is provided (==
#'     \code{NULL}), we have to select the firest one. If one is provided, we
#'     have to use that one. This is exactly what this function takes care of.
#'
#'     @param start_from_variable RandomVariable (default = NULL) the
#'      \code{RandomVariable} to start from. If NULL, we return the first one.
#'
#'     @return RandomVariable the actual start_from_variable which is a
#'      \code{RandomVariable} to start from. If NULL is provided, we return the
#'      first one.
#'   } 
#' 
#'   \item{\code{get_online_super_learner }}{ 
#'     Active method. Returns the provided instance of the
#'     \code{OnlineSuperLearner} (the one provided when initializing the
#'     object).
#'   } 
#'
#'   \item{\code{is_removing_future_variables}}{ 
#'     Active method. Is the current instance removing future variables?
#'   } 
#'
#'   \item{\code{get_random_variables}}{ 
#'     Active method. Returns the list of \code{RandomVariable} objects
#'     provided on initialization.
#'   } 
#'
#'   \item{\code{get_random_variable_names}}{ 
#'     Active method. Returns a list with names of the \code{RandomVariable}s
#'     provided on initialization.
#'   } 
#'
#'   \item{\code{get_summary_measure_generator}}{ 
#'     Active method. Returns the \code{summary_measure_generator} provided on
#'     initialization.
#'   } 
#' 
#' }  
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
        randomVariables <- Arguments$getInstanceOf(self$get_random_variables, 'list')
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
        result_denormalized_observations <- data.table(matrix(nrow = 0, ncol = length(self$get_random_variables)))
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
            data[1, remove_vars] <- NA
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
          data[, (current_outcome) := as.numeric(outcome$normalized) ]
          current_denormalized_observation[[current_outcome]] <- outcome$denormalized %>% as.numeric
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
        if(discrete) {
          return(list(normalized = outcome$normalized$dosl.estimator,
                      denormalized = outcome$denormalized$dosl.estimator))
        } else {
          return(list(normalized = outcome$normalized$osl.estimator,
                      denormalized = outcome$denormalized$osl.estimator))
        }
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

