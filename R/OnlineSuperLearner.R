#devtools::load_all('~/Workspace/osofr/condensier')

#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import future
#' @include zzz.R
#' @include LibraryFactory.R
#' @include DataSplitter.R
#' @include DensityEstimation.R
#' @include SummaryMeasureGenerator.R
#' @include WeightedCombinationComputer.R
#' @include DataCache.R
#' @include WCC.NMBFGS.R
#' @include WCC.CG.R
#' @include WCC.SGD.Simplex.R
#' @include CrossValidationRiskCalculator.R
#' @include InterventionParser.R
#' @include OnlineSuperLearner.SampleIteratively.R
#' @include OnlineSuperLearner.S3.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(SL.library.definition = c("ML.Local.lm", "ML.H2O.glm"), summaryMeasureGenerator, verbose = FALSE)}}{
#'     starts a new OnlineSuperLearner. The provided
#'     \code{SL.library.definition} contains the machine learning models to use.
#'
#'     @param SL.library.definition list a list of machine learning algorithms.
#'      This could be either a vector with with the name of each estimator or a
#'      list according to the libraryFactroy.  Look in the LibraryFactory class
#'      for the specification of this list.
#'
#'     @param summaryMeasureGenerator SummaryMeasureGenerator an object of the type
#'      SummaryMeasureGenerator. This generator is used to get new observations
#'      with the correct aggregated columns.
#'
#'     @param should_fit_osl boolean (default = TRUE) should an instance of the
#'      OSL fit the online version of the osl?
#'
#'     @param should_fit_dosl boolean (default = TRUE) should an instance of the
#'      OSL fit the discrete online version of the osl?
#'
#'     @param pre_processor PreProcessor (default = NULL) an instance of the
#'      \code{PreProcessor} which is used to normalize the in and output values
#'      for the OSL.
#'
#'     @param test_set_size integer (default = 1) the size of the test set to
#'      use.
#'
#'     @param verbose (default = FALSE) the verbosity (how much logging). Note that this might
#'      be propagated to other classes.
#'   }
#'
#'   \item{\code{set_verbosity(verbosity) }}{
#'     Method that can change the verbosity of a superlearner instance
#'
#'     @param verbose the new verbosity (how much logging) to use.
#'   }
#' 
#'   \item{\code{fit(data, initial_data_size = 5, max_iterations = 20}}{ 
#'     Fits an instance of the OnlineSuperLearner class. This is the main
#'     method used for training the online SuperLearner (and this will actually
#'     trigger the training process). This will fit the provided
#'     \code{SL.library.definition} estimators as well as the
#'     OnlineSuperLearner and the DiscreteOnlineSuperLearner.
#'
#'     @param data Data.Base the data to use to train the instance on. Note
#'      that this can be any instance of a \code{Data.Base} superclass, as long
#'      as it extends the correct functions.
#'
#'     @param initial_data_size integer (default = 5) when training an online
#'      algoritm, one first needs to specify a small number of rows of data that
#'      can be used for calculating the initial versions of the estimators
#'      (these are essentially trained as batch algorithms). How much blocks are
#'      used for training this initial set can be specified here.
#'
#'     @param max_iterations integer (default = 20) the maximum number of
#'      iterations the algorithm can use to train the algorithm. If this is more
#'      than the number of blocks in the data, the OSL will stop gracefully.
#'      This is useful when there is a stream of data which would provide data
#'      indefinitely.
#'
#'     @param mini_batch_size integer (default = 20) the size of the mini batch
#'      to use for each update. Note that this needs to be larger than the
#'      specified \code{test_set_size} on initialization. Part of this
#'      collection of blocks / mini batch will be used as a validation set
#'      while training.
#'
#'     @return data.table a data.table with the risks of each estimator.
#'   } 
#' 
#'   \item{\code{predict(data, relevantVariables = NULL, all_estimators = TRUE, discrete = TRUE, continuous = TRUE, sample = FALSE, plot = FALSE)}}{
#'     Method to perform a prediction on the estimators. It can run in
#'     different configurations. It can be configured to predict the outcome
#'     using all estimators (the \code{all_estimators} flag), using the
#'     discrete superlearner (the \code{discrete} flag), or using the
#'     continuous online superlearner (the \code{continous} flag). At least one
#'     of these three flags must be true.
#'
#'     Note that the predict function in this case yields the predicted probability of an outome.
#'     That is, it does NOT predict an actual outcome, just the probability.
#'
#'     @param data Data.Base the data to use to train the instance on. Note
#'      that this can be any instance of a \code{Data.Base} superclass, as long
#'      as it extends the correct functions.
#'
#'     @param relevantVariables list (default = NULL) the relevant variables used
#'     for doing the predictions (these should be the same as the ones used for
#'     fitting). If \code{NULL}, we will use the list provided on
#'     initialization.
#'
#'     @param all_estimators boolean (default = TRUE) whether or not to include
#'      the output of all candidate estimators in the output 
#'
#'     @param discrete boolean (default = TRUE) = whether or not to include the
#'      output of the discrete super learner in the output
#'
#'     @param continuous boolean (default = TRUE) whether or not to include the
#'      output of the continuous super learner in the output
#'
#'     @param sample boolean (default = FALSE) is the goal to sample from the
#'      underlying densities, or should we predict a probability of an outcome?
#'
#'     @param plot (default = FALSE) if set to true, the algorithm will plot
#'      the outcomes to a file for further inspection. This is useful when
#'      inspecting the performance of the estimators.
#'
#'     @return list a list with two entries; normalized and denormalized. The
#'      normalized outcomes are the values scaled between 0-1 (using the
#'      \code{PreProcessor}), the denormalized outcomes are the values
#'      transformed back to their original range.
#'   }
#'
#'   \item{\code{sample_iteratively(data, tau = 10, intervention = NULL)}}{
#'     Method to sample iteratively from the densities. It works by providing
#'     an initial observation (\code{data}), from which iteretitatively the
#'     next measurement is estimated. This is done until \code{tau} steps in
#'     the future. Furthermore, this sampling step can be augmented with an
#'     intervention. That is, we could set a given time step (or all) to a
#'     certain value. The \code{intervention} provided should be a list
#'     containing a \code{when} and \code{what} entry.  the \code{when} entry
#'     should show when the intervention is performed, the \code{what} entry
#'     shows what should be done.
#'
#'     @param data = the initial data to start the sampling from. At most 1 row of data.
#'
#'     @param tau integer (default = 10) the timestep at which you want to evaluate the output
#'
#'     @param intervention list/intervention (default = NULL) the intervention,
#'     e.g.: \code{list(when = c(1,2), what = c(1,0), variable = 'A')}. See the
#'     \code{InterventionParser} for more details.
#'
#'     @param return_type string (default = 'observations') the
#'      \code{OnlineSuperlearner.SampleIteratively} can return data in different
#'      configurations. It can return all data, only a subset, or denormalized
#'      outcomes. Check the \code{OnlineSuperlearner.SampleIteratively} class for more
#'      details.
#'
#'     @param start_from_variable RelevantVariable (default = NULL) if we don't
#'      start with sampling from the first argument in the sequence of
#'      variables, specify which one to start from.
#'
#'     @param start_from_time integer (default = 1) generally the sampling
#'     procedure starts from $t = 1$, but sometimes one might want to sample
#'     from a different point in time. This can be specified here.
#'
#'     @param check boolean (default = FALSE) should we perform a check on the
#'      provided arguments?
#'
#'     @return list/dataframe of sampled values.
#'   }
#'
#'   \item{\code{train_library(data_current)}}{
#'     Function to train the initial set of models
#'
#'     @param data_current data.table the dataset to train the estimators on.
#'   }
#'
#'   \item{\code{update_library(max_iterations, mini_batch_size)}}{
#'     Updates the initial / trained estimators with the available data. This
#'     data does not need to be provided as it is already part of the Data.Base
#'     provided on initialization / fitting.
#'
#'     @param max_iterations integer the maximal number of iterations of online
#'     learning to do.
#'
#'     @param mini_batch_size integer (default = 20) the size of the mini batch
#'      to use for each update.
#'   }
#'
#'   \item{\code{fit_dosl()}}{
#'     Finds the best estimator among the current set, for each of the
#'     densities (WAY)
#'   }
#'
#'   \item{\code{get_cv_risk()}}{
#'     Method to retrieve the current \code{cv_risk} (note that this is not an active
#'     method, so it can easiliy be stubbed).
#'
#'     @return an overview of the CV risk of the different estimators
#'   }
#'
#'   \item{\code{set_relevant_variables(relevant_variables)}}{
#'     Method to set the relevant_variables in the osl class. Generally not needed (apart from initialization).
#'   }
#'
#'   \item{\code{retrieve_list_of_relevant_variables(relevant_variables)}}{
#'     Retrieves a list of relevant variables according to a specification. This
#'     function allows for a more flexible way of retrieving relevant variables
#'     from the OSL model.
#'     @param relevant_variables the relevant_variables for which we want to
#'      receive the list of variables, in a form that our model accepts. This
#'      can be specified as follows:
#'      - List of \code{RelevantVariable} objects to predict
#'      - Single \code{RelevantVariable} object to predict
#'      - List of strings with the names of the outputs (\code{list('X','Y')})
#'      - Single string with the name of the output (\code{'Y'})
#'
#'     @return a list of \code{RelevantVariable} objects to use in the prediction function.
#'   }
#'
#'   \item{\code{is_fitted}}{
#'     Active method. Returns whether the OSL has been fitted or not
#'
#'     @return boolean true if it has been fitted, false if not
#'   }
#'
#'   \item{\code{is_online}}{
#'     Active method to deterimine whether the actual algorithm is fitted in an online way. That is to say, that all of
#'     the estimators are in fact online.
#'
#'     @return boolean true if it all algorithms are online, false if not
#'   }
#'
#'   \item{\code{fits_osl}}{
#'     Active method to know whether the current OSL fits an online super learner (that is, the weighted combination).
#'     This setting comes from the initialization step of OSL.
#'
#'     @return boolean true if it fits an osl (false if not)
#'   }
#'
#'   \item{\code{fits_dosl}}{
#'     Active method to know whether the current OSL fits a discrete online super learner.  This setting comes from the
#'     initialization step of OSL.
#'
#'     @return boolean true if it fits a discrete osl (false if not)
#'   }
#'
#'   \item{\code{info}}{
#'     Active method to print some general info related to the current OSL
#'   }
#'
#'   \item{\code{get_estimators}}{
#'     Active method to retrieve a list of estimators. These can be the fitted versions (if the osl is fitted), or the
#'     plain unfitted versions. Check the is_online version for that.
#'
#'     @return list a list object containing all estimators.
#'   }
#'
#'   \item{\code{get_osl_weights}}{
#'     Active method to retrieve a vector of weights that the OSL has found for its continuous online super learner fit.
#'
#'     @return vector a vector containing the estimates of the OSL weights
#'   }
#'
#'   \item{\code{get_dosl}}{
#'     Active method to retrieve the actual DOSL fit. this could be nil if no dosl has been fit yet.
#'
#'     @return list a list containing the best estimator for each of the relevant variables.
#'   }
#'
#'   \item{\code{get_cv_risk}}{
#'     Active method to retrieve the crossvalidated risk of each of the estimators
#'
#'     @return list a list containing the risk estimates for each of hte estimators.
#'   }
#'
#'   \item{\code{get_relevant_variables}}{
#'     Active method. Returns all \code{RelevantVariables} in the OSL object.
#'
#'     @return list a list containing all \code{RelevantVariable}s
#'   }
#'
#'   \item{\code{get_valididy}}{
#'     Active method that throws an error if the current state of the OSL is not valid (i.e., that it has invalid
#'     parameters in it).
#'   }
#'
#'   \item{\code{get_osl_sampler}}{
#'     Active method. Returns the OSL sampler (which is an instance of the
#'     \code{OnlineSuperLearner.SampleIteratively} object.
#'   }
#' }
#' @export
OnlineSuperLearner <- R6Class ("OnlineSuperLearner",
  #class = FALSE,
  cloneable = FALSE,
  portable = FALSE,
  public =
    list(
        ## Functions
        ## =========
        initialize = function(SL.library.definition = c('ML.Local.lm', 'ML.H2O.glm'),
                              summaryMeasureGenerator, relevant_variables, should_fit_osl = TRUE,
                              should_fit_dosl = TRUE, pre_processor = NULL,
                              test_set_size = 1, verbose = FALSE, parallel = FALSE, ...) {

          self$set_verbosity(Arguments$getVerbose(verbose, timestamp = TRUE))

          ## Initialize the relevant_variables early, as we use them in many places
          self$set_relevant_variables(relevant_variables)

          private$fitted <- FALSE
          private$summary_measure_generator <- Arguments$getInstanceOf(summaryMeasureGenerator, 'SummaryMeasureGenerator')
          private$should_fit_dosl <- Arguments$getLogical(should_fit_dosl)
          private$should_fit_osl <- Arguments$getLogical(should_fit_osl)

          ## Cross validation initialization
          private$cv_risk <- list()
          private$cv_risk_count <- 0
          private$cv_risk_calculator <- CrossValidationRiskCalculator$new()
          private$data_splitter <- DataSplitter$new(test_set_size = test_set_size)

          ## Initialization, Fabricate the various models
          libraryFactory <- LibraryFactory$new(verbose = verbose)
          private$fabricated_estimators <- libraryFactory$fabricate(SL.library.definition)
          private$SL.library.descriptions <- names(self$get_estimators)
          private$all_estimators_online <- DensityEstimation.are_all_estimators_online(self$get_estimators)
          private$data_cache <- DataCache$new(self$is_online)

          ## We need a weighted combination computer for each of the relevantvariables.
          ## We could reuse the WCC, and just save the weights here. However, this way we do allow
          ## to use a different wcc for each of the relevant variables.
          private$weightedCombinationComputers <- list()
          private$online_super_learner_predict <- OnlineSuperLearner.Predict$new(pre_processor = pre_processor,
                                                                                 verbose = verbose)
          private$historical_cv_risk <- list()


          ## We initialize the WCC's
          private$initialize_weighted_combination_calculators()

          private$is_parallel <- parallel

          private$osl_sampler <- OnlineSuperLearner.SampleIteratively$new(
            osl = self,
            relevantVariables = self$get_relevant_variables,
            summary_measure_generator = self$get_summary_measure_generator,
            remove_future_variables = TRUE
          )

          self$get_validity
        },

        set_verbosity = function(verbosity) {
          private$verbose = verbosity
        },

        ## Data = the data object from which the data can be retrieved
        ## initial_data_size = the number of observations needed to fit the initial estimator
        fit = function(data, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20, ...) {
          tic <- Sys.time()

          initial_data_size <- Arguments$getInteger(initial_data_size, c(1,Inf))
          max_iterations <- Arguments$getInteger(max_iterations, c(0,Inf))
          mini_batch_size <- Arguments$getInteger(mini_batch_size, c(1,Inf))
   
          ## We are taking part of the minibatch away to do the testing of the
          ## algorithm. As such, the specified minibatch size should be more
          ## than the specified test size.
          if(mini_batch_size <= self$get_data_splitter$get_test_set_size) {
            throw('We select a number of ', self$get_data_splitter$get_test_set_size,
                  ' block(s) from the mini_batch to be used as part of the test_set.',
                  ' As such, the mini batch size needs to be at least ',
                  self$get_data_splitter$get_test_set_size + 1)
          }

          data <- Arguments$getInstanceOf(data, 'Data.Base')
          ## Initialize the summary measure generator with the correct trajectories
          self$get_summary_measure_generator$set_trajectories(data = data)

          ## Check whether enough data is available to supply all relevant variables with data
          if(!self$get_summary_measure_generator$check_enough_data_available(relevantVariables = self$get_relevant_variables)) {
            stop('For some reason, there is not enough data available for all relevant variables.')
          }

          private$verbose && cat(private$verbose, 
            'Starting fitting the OnlineSuperLearner with a library: ', paste(self$get_estimator_descriptions, collapse = ', '),
            ' and we use an initial data size of ', initial_data_size,
            ' with ',max_iterations,' iterations,',
            ' and a minibatch of ',mini_batch_size
          )

          ## Get the initial data for fitting the first estimator and train the initial models
          trajectories <- self$get_summary_measure_generator$getNext(n = initial_data_size)

          ## Note that there could be multiple trajectories, so we need to iterate
          for(next_data in trajectories) {
            ## Create the initial fit
            private$verbose && enter(private$verbose, 'Fitting initial estimators')
            self$train_library(data_current = next_data)
            private$verbose && exit(private$verbose)
          }


          ## Update the library of models using a given number of max_iterations
          private$verbose && enter(private$verbose, 'Updating estimators')
          self$update_library(
            max_iterations = max_iterations,
            mini_batch_size = mini_batch_size
          )
          private$verbose && exit(private$verbose)

          toc <- Sys.time()
          private$verbose && cat(private$verbose, 'The whole procedure took ', (toc - tic), ' seconds.')
          return(self$get_cv_risk())
        },

        ## Predict should return a nrow(data) * 1 matrix, where the predictions are multiplied by
        ## the weights of each estimator.
        predict = function(data, relevantVariables = NULL,
                           all_estimators = TRUE, discrete = TRUE, continuous = TRUE,
                           sample = FALSE, plot = FALSE) {

          if (is.null(relevantVariables)) {
            relevantVariables = self$get_relevant_variables
          }

          ## Pass the function to the separate class so it won't fill up this class
          self$get_online_super_learner_predict$predict(
            osl = self,
            data = data,
            relevantVariables = relevantVariables,
            all_estimators = all_estimators,
            discrete = discrete,
            continuous = continuous,
            sample = sample,
            plot = plot
          )
        },

        ## TODO: Make S3 Function of this
        sample_iteratively = function(data, tau = 10, intervention = NULL, discrete = TRUE, 
                                      return_type = 'observations', start_from_variable = NULL, 
                                      start_from_time = 1, check = FALSE) {

          self$get_osl_sampler$sample_iteratively(
            data = data,
            tau = tau,
            intervention = intervention,
            discrete = discrete,
            return_type = return_type,
            start_from_variable = start_from_variable,
            start_from_time = start_from_time,
            check = check
          )
        },

        ## XXX: Move function to separate file?
        ## Fit or update the estimators
        train_library = function(data_current) {
          data_table <- Arguments$getInstanceOf(data_current, 'data.table')

          ## 0. Split data in test and training set
          data.splitted <- self$get_data_splitter$split(data_current)

          ## 1. Train the algorithms on the initial training set
          outcome.variables <- names(self$get_relevant_variables)
          private$build_all_estimators(data = data.splitted$train)

          private$fitted <- TRUE

          ## 2. Train the SL algorithm based on the predictions on a left out validation set
          ## Make a prediction using the learners on the test data
          predicted.outcome <- self$predict(data = data.splitted$test, discrete = FALSE,
                                            continuous = FALSE, all_estimators = TRUE)
          observed.outcome <- data.splitted$test[, outcome.variables, with=FALSE]

          if (FALSE) {
            # This is a piece of testing code that calculates the loss for each
            # entry
            sampled.outcome <- self$predict(data = data.splitted$test, discrete = FALSE,
                                              continuous = FALSE, all_estimators = TRUE, sample = TRUE)

            loss <- Evaluation.get_evaluation_function('binomial',FALSE)(data.observed = observed.outcome,
                                                                data.predicted = sampled.outcome$denormalized)

            private$verbose && cat(private$verbose, 'Loss:')
            private$verbose && cat(private$verbose, loss)
          }


          ## Extract the level 1 data and use it to fit the osl
          private$fit_osl(predicted.outcome = predicted.outcome, observed.outcome = observed.outcome)

          ## TODO: We essentially use the training data to test the online super learner
          ## We want to change this so we get a more honest guess of the preformance of
          ## the osl itself.
          ## Moreover, the way the predictions are created now is a bit inefficient, as we 
          ## are running many of the same predictions twice.
          predicted.outcome <- self$predict(data = data.splitted$test, discrete = FALSE,
                                            continuous = TRUE, all_estimators = TRUE)

          ## 3. Update the CV risk of each of the algorithms (based on test set), except for dosl
          ## We need to store the dosl risk, as we will update it later.
          pre_dosl_risk <- private$cv_risk$dosl.estimator

          ## Calculate the error using the normalized predictions
          private$update_risk(predicted.outcome = predicted.outcome,
                              observed.outcome = observed.outcome)


          ## 4. Select the discrete SL algorithm based on the left out validation set
          if (self$fits_dosl) {
            self$fit_dosl()

            ## Put the CV risk back to what it was before the update. We can now actually fit it correctly.
            private$cv_risk$dosl.estimator <- pre_dosl_risk

            ## In order to get the initial estimate of the CV error of the
            ## DOSL, we first need to fit the other estimators, and after that
            ## calculate the dosl error separately.
            predicted.outcome <- self$predict(data = data.splitted$test,
                                              discrete = TRUE, continuous = FALSE, all_estimators = FALSE)

            ## Note that we are using the cv_risk_calculator here to update the risk, not the wrapper function, hence
            ## not affecting our earlier risk score.
            private$cv_risk$dosl.estimator <-
              private$cv_risk_calculator$update_risk(predicted.outcome = predicted.outcome,
                                                     observed.outcome = observed.outcome,
                                                     relevantVariables  = self$get_relevant_variables,
                                                     current_count    = self$get_cv_risk_count-1,
                                                     current_risk     = self$get_cv_risk())$dosl.estimator
          }

          private$update_historical_cv_risk()
          private$update_oos_fit(new_data = data_current)

        },

        ## Function to update the models with the available data
        ## Params:
        ## @param Y: the column names used for the outcome
        ## @param A: the column names used for the treatment
        ## @param W: the column names used for the covariates
        ## @param max_iterations: the number of iterations we can maximaly run for training
        ## @param mini_batch_size: size of the batch we use
        ## TODO: Move function to separate file
        update_library = function(max_iterations, mini_batch_size, log_rate = 5, osl_update_risk_threshold = 0){
          private$verbose && enter(private$verbose, 'Starting estimator updating')
          if(!self$is_fitted){ throw('Fit the inital DOSL and OSL before updating them.') }

          ## Set the current timestep to 1
          t <- 0
          stopped <- FALSE

          while(t < max_iterations && !stopped) {
            ## Get the new row of data
            trajectories <- self$get_summary_measure_generator$getNext(mini_batch_size)

            ## Note that there could be multiple trajectories, so we need to iterate
            for(data_current in trajectories) {
              if(is.null(data_current) || nrow(data_current) <= self$get_data_splitter$get_test_set_size + 1) {
                ## TODO: Check wether the stopping criteria are met (e.g., improvement < theta)
                stopped <- TRUE
                break
              }

              ## Only show this log every log_rate times
              if(private$verbose && t %% log_rate == 0) {
                risk <- self$get_cv_risk()
                lapply(names(risk), function(cv_name) {
                  paste(
                    'Updating OSL at iteration', t,
                    ', CV risk for', cv_name,
                    'is', risk[cv_name]
                  ) %>% cat(private$verbose, .)
                })
              }

              self$train_library(data_current = data_current)
            }

            if (FALSE && (t %% osl_update_risk_threshold) == 0) {
              ## TODO: Build a function that only updates the risk of the OSL and
              ## DOSL once every osl_update_risk_threshold blocks, based on
              ## a subset of unseen data. This should be different from other
              ## datasources, and should be an extra step in the training
              ## procedure.

              ## 0. Create two variables to store the actual risk in of the OSL
              ## and DOSL.
              ## 1. Test on an unseen validation set
              ## 2. Update the risk of the OSL and DOSL
            }

            output = paste('performance_iteration', t, sep='_')
            OutputPlotGenerator.create_risk_plot(self$get_cv_risk(), output)
            t <- t + 1
          }
          private$verbose && exit(private$verbose)
        },

        fit_dosl = function() {
          if(!self$fits_dosl) return(FALSE)

          private$verbose && enter(private$verbose, 'Finding best estimators from the candidates')
          current_risk <- self$get_cv_risk()

          private$dosl.estimators <- rbindlist(current_risk) %>%
            ## Get the first if there are multiple best ones
            sapply(., function(algorithm_scores) {
              ids <- sort(algorithm_scores, index.return=TRUE, na.last = TRUE)$ix
              ## We do it this way as the OSL might also get selected. This
              ## might be something we want, but for now the discrete SL can
              ## only be one of the candidates, and not the OSL.
              for(name in names(current_risk)[ids]) {
                if(name %in% names(self$get_estimators)) {
                  #private$verbose && cat(private$verbose, 'Selecting ', name)
                  cat(private$verbose, 'Selecting ', name)
                  return(self$get_estimators[[name]])
                }
              }
            })

          ## DEBUGGING ONLY
          ## This function checks the fitted DOSL and checks if each of the
          ## selected estimators indeed has the lowest risk.
          if(FALSE) {
            for (i in seq_along(private$dosl.estimators)) {
              estimator <- private$dosl.estimators[[i]]
              scores <- current_risk[[i]]
              best_risk <- current_risk[[estimator$get_name]][[i]]
              all_risks <- lapply(current_risk, function(x) x[[i]])
              idx <- names(all_risks) == 'osl.estimator' | names(all_risks)== 'dosl.estimator'
              min_risks <- all_risks[!(idx)] %>% unlist %>% min
              assert_that(min_risks == best_risk)
            }
          }

          private$verbose && exit(private$verbose)
          return(TRUE)
        },


        ## Not an active function so it can be mocked!
        get_cv_risk = function() {
          return(private$cv_risk)
        },

        set_relevant_variables = function(relevant_variables) {
          variable_names <- sapply(relevant_variables, function(rv) rv$getY)
          names(relevant_variables) <- variable_names
          private$relevant_variables <- relevant_variables
        },

        retrieve_list_of_relevant_variables = function(relevant_variables) {
          ## Convert Y to relevant variable
          if (is(relevant_variables, 'list')) {
            if(length(relevant_variables) == 0) throw('There should be at least one entry in the outcomes specified')

            relevant_variables <- lapply(relevant_variables, function(rv) {
              if (!is(rv, 'RelevantVariable')) {
                ## convert it to the relevantvariable
                rv <- self$get_relevant_variables[[rv]]
              }
              rv
            })
          } else {
            if (is(relevant_variables, 'RelevantVariable')) {
              ## Encapsulate it
              relevant_variables <- list(relevant_variables)

            ## Check to see if something is actually a string or a vector
            } else if (length(nchar(relevant_variables)) == 1) {
              ## convert it to the relevantvariable
              relevant_variables <- self$get_relevant_variables[[relevant_variables]]
            } else {
              throw('Either provide strings, or relevantVariables')
            }
          }
        }
  ),
  active =
    list(
        is_online = function() {
          return(private$all_estimators_online)
        },

        is_fitted = function() {
          return(private$fitted)
        },

        fits_osl = function() {
          return(private$should_fit_osl)
        },

        fits_dosl = function() {
          return(private$should_fit_dosl)
        },

        get_historical_cv_risk = function() {
          return(private$historical_cv_risk)
        },

        get_data_cache = function() {
          return(private$data_cache)
        },

        get_online_super_learner_predict = function() {
          return(private$online_super_learner_predict)
        },

        info = function() {
          if (!self$is_fitted) {
            print('Fit the algorithm first')
            return(-1)
          }
          print('Information about the current OSL fit')
          print('=====================================')
          print('The online super learner (CTS) was fit using the following weights:')
          print(self$get_osl_weights)
          print('\n-------------------------------------')
          print('The online super learner (DSC) was fit using the following estimators:')
          print('-------------------------------------')
          lapply(self$get_dosl, function(estimator) {list(estimator$get_estimator_type, bins=estimator$get_nbins)}) %>%
            print
          print('\n-------------------------------------')
          print('The cross validated risk of each estimator is')
          print('-------------------------------------')
          print(self$get_cv_risk())
          print('=====================================')
        },

        get_estimators = function() {
          return(private$fabricated_estimators)
        },

        get_estimator_descriptions = function() {
          return(private$SL.library.descriptions)
        },

        get_weighted_combination_computers = function() {
          return(private$weightedCombinationComputers)
        },

        get_osl_weights = function() {
          sapply(self$get_weighted_combination_computers, function(wcc) wcc$get_weights) %>%
            unlist
        },

        get_dosl = function() {
          return(private$dosl.estimators)
        },

        get_summary_measure_generator = function() {
          private$summary_measure_generator
        },

        get_validity = function() {
          if (length(self$get_estimator_descriptions) == 0 || length(self$get_estimators) == 0 ) {
            throw("There should be at least one estimator in the library")
          }
          if (is.null(self$get_summary_measure_generator) || class(self$get_summary_measure_generator) != 'SummaryMeasureGenerator') {
            throw("You need to provide a summary measure generator of class SummaryMeasureGenerator")
          }
          if (!is.a(self$get_weighted_combination_computers, 'list')) {
            throw("The WCC's should be in a list, one for each RV")
          }
          private$historical_cv_risk <- Arguments$getInstanceOf(private$historical_cv_risk, 'list')
        },

        get_cv_risk_count = function() {
          return(private$cv_risk_count)
        },

        get_cv_risk_calculator = function() {
          return(private$cv_risk_calculator)
        },

        get_data_splitter = function() {
          return(private$data_splitter)
        },

        get_relevant_variables = function() {
          return(private$relevant_variables)
        },

        get_osl_sampler = function() {
          return(private$osl_sampler)
        },

        get_verbosity = function() {
          return(private$verbose)
        }

        ),
  private =
    list(
        ## Variables
        ## =========
        ## The R.cv score of the current fit
        default_wcc = WCC.CG,

        # The relevant_variables to use throughout the osl object
        relevant_variables = NULL,

        #default_wcc = WCC.SGD.Simplex,
        cv_risk = NULL,
        historical_cv_risk = NULL,
        cv_risk_count = NULL,
        cv_risk_calculator = NULL,

        ## The online discrete super learners. One for each outcome variable.
        dosl.estimators = NULL,

        ## ML Library
        SL.library.names = NULL,
        SL.library.descriptions = NULL,
        fabricated_estimators = NULL,
        fitted = NULL,

        ## Options for fitting
        should_fit_osl = NULL,
        should_fit_dosl = NULL,

        ## Splitter for the data
        data_splitter = NULL,

        ## A cache for the data (used whenever the algorithm is not online)
        data_cache = NULL,
        all_estimators_online = NULL,

        ## Summary measures and a generator
        summary_measure_generator = NULL,

        ## Verbosity of the logs
        verbose = FALSE,

        ## The computer for the SuperLearner combination
        weightedCombinationComputers = NULL,

        ## The class to make predictions on the data
        online_super_learner_predict = NULL,

        ## Train / update CV in parallel?
        is_parallel = NULL,

        ## The sampler
        osl_sampler = NULL,

        ## Functions
        ## =========

        ## Update the cross validation risk
        update_risk = function(predicted.outcome, observed.outcome, update_counter = TRUE) {
          private$cv_risk <- private$cv_risk_calculator$update_risk(predicted.outcome = predicted.outcome,
                                                                    observed.outcome = observed.outcome,
                                                                    relevantVariables = self$get_relevant_variables,
                                                                    current_count = private$cv_risk_count,
                                                                    current_risk = self$get_cv_risk())
          if (update_counter) {
            private$cv_risk_count <- self$get_cv_risk_count + 1
          }
          self$get_cv_risk_count
        },

        update_historical_cv_risk = function() {
          private$historical_cv_risk <- append(self$get_historical_cv_risk, list(private$cv_risk))
        },

        update_oos_fit = function(new_data) {
          new_data <- Arguments$getInstanceOf(new_data, 'data.table')
          N <- nrow(new_data)

          ## TODO: Implement
        },

        ## Initializes the weighted combination calculators. One for each relevantvariable.
        initialize_weighted_combination_calculators = function() {
          lapply(self$get_relevant_variables, function(rv) {
            weights.initial <- rep(1 / length(self$get_estimator_descriptions), 
                                   length(self$get_estimator_descriptions))

            ## TODO: DIP the WCC
            private$weightedCombinationComputers[[rv$getY]] <- private$default_wcc$new(weights.initial = weights.initial)
          })
        },

        ## Build using all estimators separately.
        ## Postcondition: each of our density estimators will have a fitted conditional
        ## density in them for each of our random vars WAY 
        ## *AND IT SHOULD DO THIS FOR ALL $w \in W$*
        ## Params:
        ## @param data: the dataset to build the estimators with
        build_all_estimators = function(data) {
          private$verbose && enter(private$verbose, 'Training all estimators')

          # If not all estimators are online, we have to keep track of a cache of data.
          self$get_data_cache$update_cache(newdata = data)

          # Retrieve the cached data, so we can reuse it
          cache <- data
          if(!self$is_online) {
            cache <- self$get_data_cache$get_data_cache
          }

          `%looping_function%` <- private$get_looping_function()
          #private$fabricated_estimators <- mclapply(self$get_estimators, function(estimator) {

          estimators = tryCatch({
            #for(estimator in self$get_estimators) {
            estimators <- foreach(estimator=self$get_estimators) %do% {
              private$verbose && enter(private$verbose, 'Training ', estimator$get_name)
              if(self$is_fitted && estimator$is_online) {
                # Note that we use the data here, and not the cache, as
                # essentially this cache will be  empty if none of the algorithms
                # is online, and we only want to use the new observations to
                # update the estimator.
                estimator$update(data)
              } else {
                estimator$fit(cache, relevantVariables = self$get_relevant_variables)
              }
              private$verbose && exit(private$verbose)
              estimator
            }
            
          }, error = function(e) {
            print('Something went wrong with running all estimators, starting debugger.')
            browser()
          })
          private$verbose && cat(private$verbose, 'Trained all estimators for this RV in this iteration.')
          names(estimators) <- names(self$get_estimators)
          private$fabricated_estimators <- estimators
          #}, mc.cores = 23)
          private$verbose && exit(private$verbose)
        },


        fit_osl = function(predicted.outcome, observed.outcome){
          if(!self$fits_osl) return(NULL)
          ## If we have 1 estimator, the weighted combination of that estimator
          ## is just the estimator itself.
          ## Actually fit a estimator here
          ## Tratidional way:
          ## We run each of the models on the (full?) dataset
          ## to determine their performance, and we build a design matrix from
          ## their predictions and the true observed outcome. This design matrix
          ## is then used to fit the new 'SuperLearner' estimator on.
          ##
          ## Online way:
          ## We fit our initial superlearner estimator in a similar way as described
          ## above, and we update this initial estimator using the new observations
          ## as they come in.

          ## If there is no estimator, we need to fit a estimator based on Nl observations.
          ## If we already have a estimator, we update the old one, given the new measurement
          relevant_variable_names <- Arguments$getCharacters(colnames(observed.outcome))
          if(is.null(relevant_variable_names)) throw('Something went wrong, the relevant_variable_names are not defined')

          lapply (relevant_variable_names, function(relevant_variable_name) {
            observed_outcome <- observed.outcome[, relevant_variable_name, with=FALSE]

            ## Convert the predictions to wide format so we can use them per column
            predicted_outcomes <- do.call(cbind, predicted.outcome$normalized) %>%
              subset(., select = grep(paste(relevant_variable_name,"$",sep=""), names(.)))

            if(is.null(colnames(predicted_outcomes))) throw('Something went wrong, the predicted_outcome colnames are not defined')

            self$get_weighted_combination_computers[[relevant_variable_name]]$process(Z = as.matrix(predicted_outcomes),
                                                        Y = as.matrix(observed_outcome),
                                                        self$get_estimator_descriptions)
          })

        },

        get_looping_function = function() {
          if(private$is_parallel) {
            return(`%dopar%`)
          }
          return(`%do%`)
        }
    )
)
