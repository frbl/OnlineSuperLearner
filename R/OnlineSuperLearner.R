devtools::load_all('~/Workspace/osofr/condensier')
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
#' @include SummaryMeasureGenerator.R
#' @include WeightedCombinationComputer.R
#' @include WCC.NMBFGS.R
#' @include WCC.SGD.Simplex.R
#' @include CrossValidationRiskCalculator.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(SL.library.definition = c("ML.Local.lm", "ML.H2O.glm", summaryMeasureGenerator, verbose = FALSE)}}{
#'     starts a new OnlineSuperLearner. The provided \code{SL.library.definition} contains the machine learning models to use
#'     @param SL.library.definition = a list of machine learning algorithms. This could be either a vector with
#'                                    with the name of each estimator or a list according to the libraryFactroy.
#'                                    Look in the LibraryFactory class for the specification of this list.
#'     @param summaryMeasureGenerator = an object of the type SummaryMeasureGenerator. This generator is used to
#'                                      get new observations with the correct aggregated columns.
#'     @param verbose = the verbosity (how much logging). Note that this might be propagated to other classes.
#'   }
#'
#'   \item{\code{evaluateModels(data, randomVariables) }}{
#'     Performs a basic evaluation on the data, given a list of random variables
#'     @param data = the data to use for performing the evaluation
#'     @param randomVariables = the randomVariables for which one wants to see the evaluation. Note that this needs
#'                              to be equal to, or a subset of, the random variables used to train the estimators.
#'   }
#'
#'   \item{\code{sample_iteratively(data, randomVariables, tau = 10, intervention = NULL}}{
#'     Method to sample iteratively from the densities. It works by providing an initial observation (\code{data}), from which
#'     iteretitatively the next measurement is estimated. This is done until \code{tau} steps in the future. Furthermore,
#'     this sampling step can be augmented with an intervention. That is, we could set a given time step (or all)
#'     to a certain value. The \code{intervention} provided should be a list containing a \code{when} and \code{what} entry.
#'     the \code{when} entry should show when the intervention is performed, the \code{what} entry shows what should be done.
#'     @param data = the initial data to start the sampling from. At most 1 row of data.
#'     @param randomVariables = the randomvariables used when fitting the data
#'     @param tau = the timestep at which you want to evaluate the output
#'     @param intervention = the intervention, e.g.: \code{list(when = c(1,2), what = c(1,0))}
#'   }
#'
#'   \item{\code{fit(data, randomVariables, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20}}{
#'     The actual method to fit the OnlineSuperLearner. This will fit the provided \code{SL.library.definition}
#'     estimators as well as the OnlineSuperLearner and the DiscreteOnlineSuperLearner.
#'     @param data = the data to fit the estimator on. Should be a \code{Data.Base} subclass.
#'     @param randomVariables = the random variables to fit the densities / estmators for (W,A,Y)
#'     @param initial_data_size = the size of the dataset to use for the initial fit (pre-update)
#'     @param max_iterations = the number of iterations to run for updating the data
#'     @param mini_batch_size = the size of the mini batch to use for each update.
#'   }
#'
#'   \item{\code{predict(data, randomVariables, all_estimators = TRUE, discrete = TRUE, continuous = TRUE}}{
#'     Method to perform a prediction on the estimators. It can run in different configurations. It can be configured
#'     to predict the outcome using all estimators (the \code{all_estimators} flag), using the discrete superlearner
#'     (the \code{discrete} flag), or using the continuous online superlearner (the \code{continous} flag). At least
#'     one of these three flags must be true.
#'     @param data = the data to use for doing the predictions
#'     @param randomVariables = the random variables used for doing the predictions (these should be the same as the
#'                              ones used for fitting).
#'     @param all_estimators = whether or not to include the output of all candidate estimators in the output
#'     @param discrete = whether or not to include the output of the discrete super learner in the output
#'     @param continuous = whether or not to include the output of the continuous super learner in the output
#'   }
#' }
#' @export
OnlineSuperLearner <- R6Class ("OnlineSuperLearner",
  private =
    list(
        # Variables
        # =========
        # The R.cv score of the current fit
        #default_wcc = WCC.SGD.Simplex,
        default_wcc = WCC.NMBFGS,
        cv_risk = NULL,
        cv_risk_count = NULL,
        cv_risk_calculator = NULL,

        # The online discrete super learners. One for each outcome variable.
        dosl.estimators = NULL,

        # ML Library
        SL.library.names = NULL,
        SL.library.descriptions = NULL,
        SL.library.fabricated = NULL,
        fitted = NULL,

        # Options for fitting
        should_fit_osl = NULL,
        should_fit_dosl = NULL,

        # Splitter for the data
        data_splitter = NULL,

        # Summary measures and a generator
        summaryMeasureGenerator = NULL,

        # Verbosity of the logs
        verbose = FALSE,

        # The computer for the SuperLearner combination
        weightedCombinationComputers = NULL,

        # The class to make predictions on the data
        online_super_learner_predict = NULL,

        # The data processor to convert the results back to their original format
        pre_processor = NULL,

        # Functions
        # =========

        # Update the cross validation risk
        update_risk = function(predicted.outcome, observed.outcome, randomVariables, update_counter = TRUE) {
          private$cv_risk <- private$cv_risk_calculator$update_risk(predicted.outcome = predicted.outcome,
                                                                    observed.outcome = observed.outcome,
                                                                    randomVariables = randomVariables,
                                                                    current_count = private$cv_risk_count,
                                                                    current_risk = self$get_cv_risk)
          if (update_counter) {
            private$cv_risk_count <- private$cv_risk_count + 1
          }
          private$cv_risk_count
        },

        # Initializes the weighted combination calculators. One for each randomvariable.
        initialize_weighted_combination_calculators = function(randomVariables) {
          lapply(randomVariables, function(rv) {
            weights.initial <- rep(1 / length(private$SL.library.descriptions), length(private$SL.library.descriptions))

            # TODO: DIP the WCC
            private$weightedCombinationComputers[[rv$getY]] <- private$default_wcc$new(weights.initial = weights.initial)
          })
        },

        # Train using all estimators separately.
        # Postcondition: each of our density estimators will have a fitted conditional
        # density in them for each of our random vars WAY *AND IT SHOULD DO THIS FOR ALL
        # $w \in W$*
        # Params:
        # @param data_current: the initial dataset to train the estimators on
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        # @return a vector of outcomes, each entry being the predicted outcome of an estimator on the test set
        train_all_estimators = function(data, randomVariables) {
          private$verbose && enter(private$verbose, 'Training all estimators')
          #dataH2o <- as.h2o(data)
          #private$verbose && cat(private$verbose, 'Uploaded data to h2o')
          result <- lapply(private$SL.library.fabricated, function(estimator) {
            #if(is.a(estimator, 'ML.H2O')){
              #current <- dataH2o
            #} else {
            current <- data
            #}
            estimator$process(current, randomVariables = randomVariables, update = self$is_fitted)
          })
          private$verbose && exit(private$verbose)
          result
        },

        # Functions
        # Function to train the initial set of models
        # Params:
        # @param data_current: the initial dataset to train the estimators on
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        train_library = function(data_current, randomVariables) {
          # Fit or update the  estimators
          data.splitted <- private$data_splitter$split(data_current)
          private$train_all_estimators(data = data.splitted$train, randomVariables = randomVariables)

          outcome.variables <- sapply(randomVariables, function(rv) rv$getY)

          # Extract the level 1 data and use it to fit the osl
          predicted.outcome <- private$predict_using_all_estimators(data = data.splitted$train,
                                                                    sl_library = private$SL.library.fabricated,
                                                                    denormalize = TRUE)

          observed.outcome <- data.splitted$train[,outcome.variables, with=FALSE]
          private$fit_osl(predicted.outcome = predicted.outcome, observed.outcome = observed.outcome)
          private$fitted <- TRUE

          # Make a prediction using the learners on the test data
          observed.outcome <- data.splitted$test[,outcome.variables, with=FALSE]
          predicted.outcome <- self$predict(data = data.splitted$test,
                                          randomVariables = randomVariables,
                                          discrete = TRUE, continuous = TRUE, all_estimators = TRUE)

          # Calculate the error
          private$update_risk(predicted.outcome = predicted.outcome,
                              observed.outcome = observed.outcome,
                              randomVariables = randomVariables)

          # Update the discrete superlearner (take the first if there are multiple candidates)
          private$fit_dosl()

          # In order to get the initial estimate of the CV error of the DOSL, we first need to fit the other 
          # estimators, and after that calculate the dosl error separately. 
          predicted.outcome <- self$predict(data = data.splitted$test,
                                          randomVariables = randomVariables,
                                          discrete = TRUE, continuous = FALSE, all_estimators = FALSE)
          private$cv_risk$dosl.estimator <- private$cv_risk_calculator$calculate_risk(predicted.outcome = predicted.outcome,
                              observed.outcome = observed.outcome,
                              randomVariables = randomVariables)$dosl.estimator

        },

        # Function to update the models with the available data
        # Params:
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        # @param max_iterations: the number of iterations we can maximaly run for training
        # @param mini_batch_size: size of the batch we use
        update_library = function(randomVariables, max_iterations, mini_batch_size){
          private$verbose && enter(private$verbose, 'Starting estimator updating')
          if(!self$is_fitted){
            throw('Fit the inital D-OSL and OSL first')
          }

          # Set the current timestep to 1
          t <- 0

          data_current <- private$summaryMeasureGenerator$getNext(mini_batch_size)

          # TODO: Check wether the stopping criteria are met (e.g., improvement < theta)
          while(t < max_iterations && nrow(data_current) >= 1 && !is.null(data_current)) {

            # Only show this log every 5 times
            if(t %% 5 == 0 && private$verbose) {
              lapply(names(self$get_cv_risk), function(cv_name) {
                cat(private$verbose, paste('Updating OSL at iteration', t,
                                          'error for', cv_name,
                                          'is', self$get_cv_risk[cv_name]))
              })
            }

            private$train_library(data_current = data_current, randomVariables = randomVariables)

            # Get the new row of data
            data_current <- private$summaryMeasureGenerator$getNext(mini_batch_size)
            t <- t + 1
          }
          private$verbose && exit(private$verbose)
        },

        fit_osl = function(predicted.outcome, observed.outcome){
          if(!self$fits_osl) return(NULL)
          # If we have 1 estimator, the weighted combination of that estimator
          # is just the estimator itself.
          # Actually fit a estimator here
          # Tratidional way:
          # We run each of the models on the (full?) dataset
          # to determine their performance, and we build a design matrix from
          # their predictions and the true observed outcome. This design matrix
          # is then used to fit the new 'SuperLearner' estimator on.
          #
          # Online way:
          # We fit our initial superlearner estimator in a similar way as described
          # above, and we update this initial estimator using the new observations
          # as they come in.

          # If there is no estimator, we need to fit a estimator based on Nl observations.
          # If we already have a estimator, we update the old one, given the new measurement
          random_variable_names <- Arguments$getCharacters(colnames(observed.outcome))
          if(is.null(random_variable_names)) throw('Something went wrong, the random_variable_names are not defined')

          lapply (random_variable_names, function(random_variable_name) {
            observed_outcome <- observed.outcome[, random_variable_name, with=FALSE]

            # Convert the predictions to wide format so we can use them per column
            predicted_outcomes <- do.call(cbind, predicted.outcome) %>%
              subset(., select = grep(paste(random_variable_name,"$",sep=""), names(.)))

            if(is.null(colnames(predicted_outcomes))) throw('Something went wrong, the predicted_outcome colnames are not defined')

            private$weightedCombinationComputers[[random_variable_name]]$process(Z = as.matrix(predicted_outcomes),
                                                        Y = as.matrix(observed_outcome),
                                                        private$SL.library.descriptions)
          })
        },

        # Find the best estimator among the current set, for each of the densities (WAY)
        fit_dosl = function() {
          if(!self$fits_dosl) return(NULL)

          private$verbose && enter(private$verbose, 'Finding best estimators from the candidates')
          current_risk <- self$get_cv_risk
          private$dosl.estimators <- rbindlist(current_risk) %>%
            # Get the first if there are multiple best ones
            sapply(., function(algorithm_scores) {
              # We do it this way as the OSL might also get selected. This might be something we want, but for now
              # the discrete SL can only be one of the candidates, and not the OSL.
              ids <- sort(algorithm_scores, index.return=TRUE)$ix
              for(name in names(current_risk)[ids]) {
                if(name %in% names(private$SL.library.fabricated)) {
                  return(private$SL.library.fabricated[[name]])
                }
              }
            })
          private$verbose && exit(private$verbose)
        }


        ## Fit the actual super learner on all models in the current set of models
        ##
        ## Params:
        ## @param predicted.outcome: the predicted.outcome by the learning algorithms
        ## @param observed.outcome: the outcome we observed in the actual dataset
        #fit = function(predicted.outcome, observed.outcome) {

          #fit_dosl(predicted.outcome, observed.outcome)
          #private$dosl.estimators <- private$fit_dosl()

          ## Do gradient descent update
          ## Something like:
          ##Xmat <- model.matrix(formula, train)
          ##suppressWarnings(prediction <- self$predict(train, A, W))
          ##gradient <- (t(Xmat) %*% (prediction - Ymat))
          ##self$estimator <- self$estimator - private$learning.rate * gradient

          ##prediction <- self$predict(train, A, W)
        #}

        ),
  active =
    list(
        is_fitted = function() {
          private$fitted
        },

        fits_osl = function() {
          private$should_fit_osl
        },

        fits_dosl = function() {
          private$should_fit_dosl
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
          print(self$get_dosl)
          print('\n-------------------------------------')
          print('The cross validated risk of each estimator is')
          print('-------------------------------------')
          print(self$get_cv_risk)
          print('=====================================')
        },

        get_estimators = function() {
          return(private$SL.library.fabricated)
        },

        get_osl_weights = function() {
          sapply(private$weightedCombinationComputers, function(wcc) wcc$get_weights)
        },

        get_dosl = function() {
          return(private$dosl.estimators)
        },

        get_cv_risk = function() {
          return(private$cv_risk)
        },

        get_validity = function() {
          if (length(private$SL.library.descriptions) == 0 || length(private$SL.library.fabricated) == 0 ) {
            throw("There should be at least one estimator in the library")
          }
          if (is.null(private$summaryMeasureGenerator) || class(private$summaryMeasureGenerator) != 'SummaryMeasureGenerator') {
            throw("You need to provide a summary measure generator of class SummaryMeasureGenerator")
          }
          if (!is.a(private$weightedCombinationComputers, 'list')) {
            throw("The WCC's should be in a list, one for each RV")
          }
        }
        ),
  public =
    list(
        # Functions
        # =========
        initialize = function(SL.library.definition = c('ML.Local.lm', 'ML.H2O.glm'),
                              summaryMeasureGenerator, should_fit_osl = TRUE, should_fit_dosl = TRUE, pre_processor = NULL,
                              verbose = FALSE ) {
          private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
          private$fitted = FALSE
          private$summaryMeasureGenerator <- Arguments$getInstanceOf(summaryMeasureGenerator, 'SummaryMeasureGenerator')
          private$should_fit_dosl <- Arguments$getLogical(should_fit_dosl)
          private$should_fit_osl <- Arguments$getLogical(should_fit_osl)

          # Cross validation initialization
          private$cv_risk = list()
          private$cv_risk_count = 0
          private$cv_risk_calculator = CrossValidationRiskCalculator$new()
          private$data_splitter <- DataSplitter$new()

          # Initialization, Fabricate the various models
          libraryFactory <- LibraryFactory$new(verbose = verbose)
          private$SL.library.fabricated <- libraryFactory$fabricate(SL.library.definition)
          private$SL.library.descriptions <- names(private$SL.library.fabricated)

          # We need a weighted combination computer for each of the randomvariables.
          # We could reuse the WCC, and just save the weights here. However, this way we do allow
          # to use a different wcc for each of the random variables.
          private$weightedCombinationComputers <- list()
          private$online_super_learner_predict <- OnlineSuperLearner.Predict$new(verbose, pre_processor)

          self$get_validity
        },

        # Samples the data iteratively from the fitted distribution, and applies an intervention if necessary
        # @param tau is the time at which we want to measure the outcome
        sample_iteratively = function(data, randomVariables, variable_of_interest, tau = 10, intervention = NULL, discrete = TRUE) {

          randomVariables <- Arguments$getInstanceOf(randomVariables, 'list')
          randomVariables <- RandomVariable.find_ordering(randomVariables)

          if(!is.null(intervention)) {
            intervention <- Arguments$getInstanceOf(intervention, 'list')
            valid_intervention <- is.numeric(intervention$when) &
            is.numeric(intervention$what) &
            is.character(intervention$variable) &
            length(intervention$when) == length(intervention$what)

            if(!valid_intervention) throw('The intervention specified is not correct! it should have a when (specifying t), a what (specifying the intervention) and a variable (specifying the name of the variable to intervene on).')
          }

          result <- data.table()

          # We need to sample sequentially here, just so we can plugin the value everytime in the next evaluation
          for (t in seq(tau)) {
            data[,names(randomVariables)] <- NA
            for (rv in randomVariables) {
              current_outcome <- rv$getY
              if (!is.null(intervention) & current_outcome == intervention$variable & t %in% intervention$when) {
                when.idx <- which(intervention$when == t)
                outcome <- intervention$what[when.idx]
                private$verbose && cat(private$verbose, 'Setting intervention on ', current_outcome,' with ', outcome, ' on time ', t)
              } else {
                # We only want to denormalize the eventual outcome
                denormalize = t == tau && rv == variable_of_interest

                outcome <- self$predict(data = data, randomVariables = c(rv),
                                        discrete = discrete,
                                        continuous = !discrete,
                                        all_estimators = FALSE, sample = TRUE, denormalize = denormalize)[[1]]

                private$verbose && cat(private$verbose,'Predicting ', current_outcome, ' using ', paste(rv$getX, collapse=', '))
              }
              data[,  (current_outcome) := outcome ]
            }
            result <- rbind(result, data)
            if(t != tau)  data <- private$summaryMeasureGenerator$getLatestCovariates(data)
          }
          return(result[,names(randomVariables), with = FALSE])
        },


        # Data = the data object from which the data can be retrieved
        # initial_data_size = the number of observations needed to fit the initial estimator
        fit = function(data, randomVariables, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20) {

          tic <- Sys.time()
          initial_data_size <- Arguments$getInteger(initial_data_size, c(1,Inf))
          max_iterations <- Arguments$getInteger(max_iterations, c(0,Inf))

          data <- Arguments$getInstanceOf(data, 'Data.Base')
          private$summaryMeasureGenerator$setData(data = data)

          # TODO: Move to check validity? Needs moving of the equations as well.
          private$summaryMeasureGenerator$checkEnoughDataAvailable(randomVariables = randomVariables)

          # We initialize the WCC's here because we need to have the randomVriables
          private$initialize_weighted_combination_calculators(randomVariables)

          print(paste('Fitting OnlineSuperLearner with a library:', paste(private$SL.library.descriptions, collapse = ', '),
                      'and we use an initial data size of', initial_data_size,
                      'with',max_iterations,'iterations,',
                      'and a minibatch of',mini_batch_size))

          # Get the initial data for fitting the first estimator and train the initial models
          private$verbose && enter(private$verbose, 'Fitting initial estimators')
          private$summaryMeasureGenerator$getNext(initial_data_size) %>%
            private$train_library(data_current = ., randomVariables = randomVariables)
          private$verbose && exit(private$verbose)

          private$verbose && enter(private$verbose, 'Updating estimators')
          # Update the library of models using a given number of max_iterations
          private$update_library(randomVariables = randomVariables, max_iterations = max_iterations,
                                mini_batch_size = mini_batch_size)

          # Return the cross validated risk
          private$verbose && exit(private$verbose)

          toc <- Sys.time()
          private$verbose && cat(private$verbose, 'The whole procedure took ', (toc - tic), ' seconds.')
          return(self$get_cv_risk)
        },

        # Predict should return a nrow(data) * 1 matrix, where the predictions are multiplied by
        # the weights of each estimator.
        predict = function(data, randomVariables, all_estimators = TRUE, discrete = TRUE, continuous = TRUE, sample = FALSE, plot = FALSE, denormalize = TRUE) {

          # Pass the function to the separate class so it won't fill up this class
          private$online_super_learner_predict(osl = self, data = data, randomVariables = randomVariables,
                                               all_estimators = all_estimators, discrete = discrete, continuous = continuous,
                                               sample = sample, plot = plot, denormalize = denormalize)

        }
  )
)
