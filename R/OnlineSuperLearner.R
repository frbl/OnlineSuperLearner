#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include zzz.R
#' @include LibraryFactory.R
#' @include DataSplitter.R
#' @include SummaryMeasureGenerator.R
#' @include WeightedCombinationComputer.R
#' @include WCC.NMBFGS.R
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

        # Splitter for the data
        data_splitter = NULL,

        # Summary measures and a generator
        summaryMeasureGenerator = NULL,

        # Verbosity of the logs
        verbose = FALSE,

        # The computer for the SuperLearner combination
        weightedCombinationComputers = NULL,

        # Functions
        # =========

        # Find the best estimator among the current set, for each of the densities (WAY)
        find_current_best_estimator = function() {
          private$verbose && enter(private$verbose, 'Finding best estimators from the candidates')
          current_risk <- self$get_cv_risk
          result <- rbindlist(current_risk) %>%
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
          return(result)
        },

        # Update the cross validation risk
        update_risk = function(predicted.outcome, observed.outcome, randomVariables) {
          private$cv_risk <- private$cv_risk_calculator$update_risk(predicted.outcome = predicted.outcome,
                                                                    observed.outcome = observed.outcome,
                                                                    randomVariables = randomVariables,
                                                                    current_count = private$cv_risk_count,
                                                                    current_risk = self$get_cv_risk)
          private$cv_risk_count <- private$cv_risk_count + 1
        },

        # Initializes the weighted combination calculators. One for each randomvariable.
        initialize_weighted_combination_calculators = function(randomVariables) {
          lapply(randomVariables, function(rv) {
            weights.initial <- rep(1 / length(private$SL.library.descriptions), length(private$SL.library.descriptions))

            # TODO: DIP the WCC
            private$weightedCombinationComputers[[rv$getY]] <- private$default_wcc$new(weights.initial = weights.initial)
          })
        },

        # Predict using all estimators separately.
        # Params:
        # @param data_current: the initial dataset to train the estimators on
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        # @return a list of outcomes, each entry being a data.table with the outcomes of an estimator
        predictUsingAllEstimators = function(data) {
          private$verbose && enter(private$verbose, 'Predicting with all models')
          #dataH2o <- as.h2o(data)
          #private$verbose && cat(private$verbose, 'Uploaded data to h2o')
          result <- lapply(private$SL.library.fabricated,
            function(estimator) {
              #if(is.a(estimator, 'ML.H2O')){
                #current <- dataH2o
              #} else {
                current <- data
              #}
              # TODO: Unity in export formats. Probably the best is to enforce a data.table output
              estimator$predict(current, sample = TRUE)
            })

          # convert the list of results into a data.table
          result <- lapply(result, function(res) as.data.table(do.call(cbind, res)))
          private$verbose && exit(private$verbose)
          result
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

          lapply(private$SL.library.fabricated, function(estimator) {
            #if(is.a(estimator, 'ML.H2O')){
              #current <- dataH2o
            #} else {
            current <- data
            #}
            estimator$process(current, randomVariables = randomVariables, update = self$is_fitted)
          })
          private$verbose && exit(private$verbose)
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
          predicted.outcome <- private$predictUsingAllEstimators(data = data.splitted$train)
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
          private$dosl.estimators <- private$find_current_best_estimator()
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
          t <- mini_batch_size

          data_current <- private$summaryMeasureGenerator$getNextN(mini_batch_size)

          # TODO: Check wether the stopping criteria are met (e.g., improvement < theta)
          while(t < max_iterations && nrow(data_current) >= 1 && !is.null(data_current)) {

            # Only show this log every X times (X * mini_batch_size)
            if(t %% (1 * mini_batch_size) == 0 && private$verbose) {
              lapply(names(self$get_cv_risk), function(cv_name) {
                        cat(private$verbose, paste('Updating OSL at iteration', t,
                                                  'error for', cv_name,
                                                  'is', self$get_cv_risk[cv_name]))
                  })
            }

            private$train_library(data_current = data_current, randomVariables = randomVariables)

            # Get the new row of data
            data_current <- private$summaryMeasureGenerator$getNextN(mini_batch_size)
            t <- t + mini_batch_size
          }
          private$verbose && exit(private$verbose)
        },

        fit_osl = function(predicted.outcome, observed.outcome){
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
        }

        ## Fit the actual super learner on all models in the current set of models
        ##
        ## Params:
        ## @param predicted.outcome: the predicted.outcome by the learning algorithms
        ## @param observed.outcome: the outcome we observed in the actual dataset
        #fit = function(predicted.outcome, observed.outcome) {

          #fit_dosl(predicted.outcome, observed.outcome)
          #private$dosl.estimators <- private$find_current_best_estimator()

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
        is_fitted = function(){
          private$fitted
        },

        get_estimators = function() {
          return(private$SL.library.fabricated)
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
                              summaryMeasureGenerator, verbose = FALSE) {
          private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
          private$fitted = FALSE
          private$summaryMeasureGenerator <- Arguments$getInstanceOf(summaryMeasureGenerator, 'SummaryMeasureGenerator')

          # Cross validation initialization
          private$cv_risk = list()
          private$cv_risk_count = 0
          private$cv_risk_calculator = CrossValidationRiskCalculator$new()
          private$data_splitter <- DataSplitter$new()

          # Initialization, Fabricate the various models
          libraryFactory <- LibraryFactory$new(verbose = verbose)
          private$SL.library.fabricated <- libraryFactory$fabricate(SL.library.definition)
          private$SL.library.descriptions <- names(private$SL.library.fabricated)
          private$verbose && cat(private$verbose, paste('Creating super learner with a library:', private$SL.library.descriptions))

          # We need a weighted combination computer for each of the randomvariables.
          # We could reuse the WCC, and just save the weights here. However, this way we do allow
          # to use a different wcc for each of the random variables.
          private$weightedCombinationComputers <- list()

          self$get_validity
        },

        evaluateModels = function(data, randomVariables) {
          predicted.outcome <- self$predict(data = data, randomVariables = randomVariables, 
                                            discrete = TRUE, continuous = TRUE, all_estimators = TRUE)

          outcome.variables <- sapply(randomVariables, function(rv) rv$getY)
          observed.outcome <- data[, outcome.variables, with=FALSE]

          private$cv_risk_calculator$calculate_evaluation(predicted.outcome = predicted.outcome,
                                                          observed.outcome = observed.outcome, 
                                                          randomVariables = randomVariables)
        },

        # Samples the data iteratively from the fitted distribution, and applies an intervention if necessary
        # @param tau is the time at which we want to measure the outcome
        sample_iteratively = function(data, randomVariables, tau = 10, intervention = NULL,
                                      discrete = TRUE) {


          randomVariables <- Arguments$getInstanceOf(randomVariables, 'list')
          randomVariables <- RandomVariable.find_ordering(randomVariables)

          intervention <- Arguments$getInstanceOf(intervention, 'list')
          valid_intervention <- is.numeric(intervention$when) &
           is.numeric(intervention$what) &
           is.character(intervention$variable) &
           length(intervention$when) == length(intervention$what)
          if(!valid_intervention) throw('The intervention specified is not correct! it should have a when (specifying t), a what (specifying the intervention) and a variable (specifying the name of the variable to intervene on).')
          result <- data.table()

          # We need to sample sequentially here, just so we can plugin the value everytime in the next evaluation
          for (t in seq(tau)) {
            data[,names(randomVariables)] <- NA
            for (rv in randomVariables) {
              current_outcome <- rv$getY
              if (!is.null(intervention) & current_outcome == intervention$variable & t %in% intervention$when) {
                when.idx <- which(intervention$when==t)
                outcome <- intervention$what[when.idx]
                private$verbose && cat(private$verbose, 'Setting intervention on ', current_outcome,' with ', outcome, ' on time ', t)
              } else {
                outcome <- self$predict(data = data, randomVariables = c(rv), 
                                        discrete = discrete, 
                                        continuous = !discrete,
                                        all_estimators = FALSE)[[1]]

                private$verbose && cat(private$verbose,'Predicting ', current_outcome, ' using ', paste(rv$getX, collapse=', '))
              }
              data[,  (current_outcome) := outcome ]
            }
            result <- rbind(result, data)
            if(t != tau) data <- private$summaryMeasureGenerator$getLatestCovariates(data)
          }
          return(result[,names(randomVariables), with = FALSE])
        },


        # Data = the data object from which the data can be retrieved
        # initial_data_size = the number of observations needed to fit the initial estimator
        fit = function(data, randomVariables, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20) {

          data <- Arguments$getInstanceOf(data, 'Data.Base')
          private$summaryMeasureGenerator$setData(data = data)

          # TODO: Move to check validity? Needs moving of the equations as well.
          private$summaryMeasureGenerator$checkEnoughDataAvailable(randomVariables = randomVariables)

          # We initialize the WCC's here because we need to have the randomVriables
          private$initialize_weighted_combination_calculators(randomVariables)

          # Get the initial data for fitting the first estimator and train the initial models
          private$verbose && enter(private$verbose, 'Fitting initial models')
          private$summaryMeasureGenerator$getNextN(initial_data_size) %>%
            private$train_library(data_current = ., randomVariables = randomVariables)
          private$verbose && exit(private$verbose)

          # Update the library of models using a given number of max_iterations
          private$update_library(randomVariables = randomVariables, max_iterations = max_iterations,
                                mini_batch_size = mini_batch_size)

          # Return the cross validated risk
          return(self$get_cv_risk)
        },

        # Predict should return a nrow(data) * 1 matrix, where the predictions are multiplied by
        # the weights of each estimator.
        predict = function(data, randomVariables, all_estimators = TRUE, discrete = TRUE, continuous = TRUE) {
          if (!self$is_fitted){
            return(NA)
          }

          if (!any(c(discrete, all_estimators, continuous))) {
            throw('At least one option should be selected: discrete, all_estimators, or continuous')
          }

          private$verbose && enter(private$verbose, 'Predicting for',
                                  ifelse(all_estimators, ', all estimators',''),
                                  ifelse(discrete, ', discrete superlearner',''),
                                  ifelse(continuous, ', continuous superlearner','')
                                 )
          result <- list()

          if (all_estimators | continuous) {
            predictions <- private$predictUsingAllEstimators(data = data)

            if (all_estimators) {
              private$verbose && cat(private$verbose, 'All Estimators')
              result <- append(result, predictions) 
            }

            if(continuous) {
              private$verbose && cat(private$verbose, 'continuous SL')
              weights <- sapply(private$weightedCombinationComputers, function(wcc) wcc$get_weights)

              # TODO: What if A ends up not being binary?
              # TODO: More important, what if a variable is discrete?
              result$osl.estimator <- lapply(randomVariables, function(rv) {
                current_rv_name <- rv$getY
                result <- do.call(cbind, predictions) %>%
                  subset(., select = grep(paste(current_rv_name,"$",sep=""), names(.))) %>%
                  as.matrix(.) %*% weights[,current_rv_name]

                colnames(result) <- current_rv_name
                result
              }) %>% 
               do.call(cbind, .) %>%
                as.data.table 
            }
          }

          if (discrete) {
            private$verbose && cat(private$verbose, 'discrete SL')
            result$dosl.estimator <- lapply(randomVariables, function(rv) {
              outcome_name <- rv$getY

              # This is for the first iteration, we don't have a dosl yet as it get's selected based
              # on the other estimator's CV score
              if (outcome_name %in% names(private$dosl.estimators)) {
                outcome_name
                private$dosl.estimators
                prediction <- private$dosl.estimators[[outcome_name]]$predict(data = data)[[outcome_name]]
              } else {
                prediction <- c(outcome = NA)
              }
              prediction %<>% as.matrix(prediction)
              colnames(prediction) <- outcome_name
              prediction
            }) %>%
                do.call(cbind, .) %>%
                as.data.table 
          }
          private$verbose && exit(private$verbose)
          result
        }
  )
)

