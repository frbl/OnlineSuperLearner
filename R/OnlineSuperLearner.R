devtools::load_all('~/Workspace/frbl/tmlenet')


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
#' @include CrossValidationRiskCalculator.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(SL.library.definition)}}{
#'     starts a new OnlineSuperLearner. The provided /code{SL.library.definition} contains the machine learning models to use
#'   }
#'
#'   \item{\code{run(data, Y, A, W,  initial_data_size = 10)}}{
#'     Runs the actual OnlineSuperLearning calculation
#'   }
#'   \item{\code{getModel()}}{
#'     Returns the final OnlineSuperLearner model
#'   }
#'   \item{\code{predict(data, X)}}{
#'     returns an actual prediction using the superlearning model
#'   }
#' }
#' @export
OnlineSuperLearner <- R6Class ("OnlineSuperLearner",
  private =
    list(
        # Variables
        # =========
        # The R.cv score of the current fit
        cv_risk = NULL,
        cv_risk_count = NULL,
        cv_risk_calculator = NULL,

        # The online discrete super learners. One for each outcome variable.
        odsl.estimators = NULL,

        # ML Library
        SL.library.names = NULL,
        SL.library.descriptions = NULL,
        SL.library.fabricated = NULL,
        fitted = NULL,

        # Splitter for the data
        dataSplitter = NULL,

        # Summary measures and a generator
        summaryMeasureGenerator = NULL,

        # Verbosity of the logs
        verbose = FALSE,

        # The computer for the SuperLearner combination
        weightedCombinationComputers = NULL,

        # Functions
        # =========

        # Find the best estimator among the current set, for each of the outcomes (WAY)
        find_current_best_estimator = function() {
          current_risk <- self$get_cv_risk
          rbindlist(current_risk) %>%
            # Get the first if there are multiple best ones
            sapply(., function(x) which.min(x)[1]) %>%
            lapply(., function(algorithm) {
              names(current_risk)[algorithm] %>%
                private$SL.library.fabricated[[.]]
            })
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

        initialize_weighted_combination_calculators = function(randomVariables) {
          lapply(randomVariables, function(rv) {
            weights.initial <- rep(1 / length(private$SL.library.descriptions), length(private$SL.library.descriptions))

            # TODO: DIP the WCC
            private$weightedCombinationComputers[[rv$getY]] <- WCC.SGD$new(weights.initial = weights.initial)
          })
        },

        # Predict using all estimators separately.
        # Params:
        # @param data.initial: the initial dataset to train the estimators on
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        # @return a list of outcomes, each entry being a data.table with the outcomes of an estimator
        predictUsingAllEstimators = function(data) {
          private$verbose && enter(private$verbose, 'Predicting with all models')
          #dataH2o <- as.h2o(data)
          #private$verbose && cat(private$verbose, 'Uploaded data to h2o')
          result <- lapply(private$SL.library.fabricated,
            function(model) {
              #if(is.a(model, 'ML.H2O')){
                #current <- dataH2o
              #} else {
                current <- data
              #}
              # TODO: Unity in export formats. Probably the best is to enforce a data.table output
              model$sample(current)
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
        # @param data.initial: the initial dataset to train the estimators on
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        # @return a vector of outcomes, each entry being the predicted outcome of an estimator on the test set
        trainAllEstimators = function(data, randomVariables) {
          private$verbose && enter(private$verbose, 'Training all estimators')
          #dataH2o <- as.h2o(data)
          #private$verbose && cat(private$verbose, 'Uploaded data to h2o')

          lapply(private$SL.library.fabricated,
            function(model) {
              #if(is.a(model, 'ML.H2O')){
                #current <- dataH2o
              #} else {
              current <- data
              #}
              model$process(current, randomVariables = randomVariables, update = self$isFitted)
            }
          )
          private$verbose && exit(private$verbose)
        },

        # Functions
        # Function to train the initial set of models
        # Params:
        # @param data.initial: the initial dataset to train the estimators on
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        trainLibrary = function(data.initial, randomVariables) {
          if(self$isFitted){
            warning('DOSL and OSL already fitted (initially), skipping initialization')
            return(FALSE)
          }

          private$verbose && enter(private$verbose, 'Creating initial fits')

          data.splitted <- private$dataSplitter$split(data.initial)
          outcome.variables <- sapply(randomVariables, function(rv) rv$getY)

          # Fit the initial models
          private$trainAllEstimators(data = data.splitted$train,
                                      randomVariables = randomVariables)

          predicted.outcome <- private$predictUsingAllEstimators(data = data.splitted$test)
          observed.outcome <- data.splitted$test[,outcome.variables, with=FALSE]

          # Calculate the error
          private$update_risk(predicted.outcome = predicted.outcome,
                                                  observed.outcome = observed.outcome, 
                                                  randomVariables = randomVariables)

          # Fit the super learner on the current data
          #Ymat <- as.matrix(data.initial[, Y, with = FALSE])

          # Update the discrete superlearner (take the first if there are multiple candidates)
          private$fit(predicted.outcome = predicted.outcome, observed.outcome = observed.outcome )
          private$fitted = TRUE
          private$verbose && exit(private$verbose)

          TRUE
        },

        # Function to update the models with the available data
        # Params:
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        # @param max_iterations: the number of iterations we can maximaly run for training
        # @param mini_batch_size: size of the batch we use
        updateLibrary = function(randomVariables, max_iterations, mini_batch_size){
          private$verbose && enter(private$verbose, 'Starting model updating')
          if(!self$isFitted){
            throw('Fit the inital D-OSL and OSL first')
          }

          # Set the current timestep to 1
          t <- mini_batch_size

          data.current <- private$summaryMeasureGenerator$getNextN(mini_batch_size)

          # TODO: Check wether the stopping criteria are met (e.g., improvement < theta)
          while(t < max_iterations && nrow(data.current) >= 1 && !is.null(data.current)) {

            # Only show this log every X times (X * mini_batch_size)
            if(t %% (1 * mini_batch_size) == 0 && private$verbose) {
              lapply(names(self$get_cv_risk), function(cv_name) {
                        cat(private$verbose, paste('Updating OSL at iteration', t,
                                                  'error for', cv_name,
                                                  'is', self$get_cv_risk[cv_name]))
                  })
            }

            # Update all models on the previous data set, and get their prediction on the current data.
            data.splitted <- private$dataSplitter$split(data.current)
            private$trainAllEstimators(data = data.splitted$train, randomVariables = randomVariables)
            outcome_variables <- sapply(randomVariables, function(rv) rv$getY)

            # Make a prediction using our (super)learners
            predicted.outcome <- private$predictUsingAllEstimators(data = data.splitted$test)
            observed.outcome  <- data.splitted$test[,outcome_variables, with=FALSE]

            osl.prediction <- self$predict(data = data.splitted$test, randomVariables = randomVariables)
            odsl.prediction <- self$predict(data = data.splitted$test,
                                            randomVariables = randomVariables,
                                            discrete = TRUE)

            # Calculate the updated cross validated risk using the new measurement
            # Predicted.outcome is now the level 1 'matrix' of each estimator. It contains
            # their predicted outcomes on the test set
            #predicted.outcome <- as.data.frame(c(CVpredictions, dosl = dosl.prediction, osl = osl.prediction))
            #observed.outcome <- as.matrix(tail(data.current[, Y, with = FALSE], 1))

            # Update the cross-validated risk
            private$update_risk(predicted.outcome = predicted.outcome,
                                observed.outcome = observed.outcome, 
                                randomVariables = randomVariables)

            # ReFit the super learner on the current data
            private$fit(predicted.outcome = predicted.outcome, observed.outcome = observed.outcome)

            # Get the new row of data
            data.current <- private$summaryMeasureGenerator$getNextN(mini_batch_size)
            t <- t + mini_batch_size
          }
          private$verbose && exit(private$verbose)
        },

        # Fit the actual super learner on all models in the current set of models
        #
        # Params:
        # @param predicted.outcome: the predicted.outcome by the learning algorithms
        # @param observed.outcome: the outcome we observed in the actual dataset
        fit = function(predicted.outcome, observed.outcome) {

          # If we have 1 estimator, the weighted combination of that estimator
          # is just the estimator itself.
          # Actually fit a model here
          # Tratidional way:
          # We run each of the models on the (full?) dataset
          # to determine their performance, and we build a design matrix from
          # their predictions and the true observed outcome. This design matrix
          # is then used to fit the new 'SuperLearner' model on.
          #
          # Online way:
          # We fit our initial superlearner model in a similar way as described
          # above, and we update this initial model using the new observations
          # as they come in.

          # If there is no model, we need to fit a model based on Nl observations.
          # If we already have a model, we update the old one, given the new measurement
          

          lapply (colnames(observed.outcome), function(randomVariableName) {
            observedRandomVariable <- observed.outcome[, randomVariableName, with=FALSE]

            # Convert the predictions to wide format so we can use them per column
            predictedRandomVariable <- do.call(cbind, predicted.outcome) %>%
              subset(., select = grep(paste(randomVariableName,"$",sep=""), names(.)))
            names(predictedRandomVariable) <- names(predicted.outcome)
            private$weightedCombinationComputers[[randomVariableName]]$process(Z = predictedRandomVariable,
                                                        Y = observedRandomVariable,
                                                        private$SL.library.descriptions)
          })

          private$odsl.estimators <- private$find_current_best_estimator()

          # Do gradient descent update
          # Something like:
          #Xmat <- model.matrix(formula, train)
          #suppressWarnings(prediction <- self$predict(train, A, W))
          #gradient <- (t(Xmat) %*% (prediction - Ymat))
          #self$model <- self$model - private$learning.rate * gradient

          #prediction <- self$predict(train, A, W)
        }

        ),
  active =
    list(
        isFitted = function(){
          private$fitted
        },
        get_cv_risk = function() {
          return(private$cv_risk)
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
          private$summaryMeasureGenerator <- summaryMeasureGenerator
          private$dataSplitter <- DataSplitter$new()

          # Cross validation initialization
          private$cv_risk = list()
          private$cv_risk_count = 0
          private$cv_risk_calculator = CrossValidationRiskCalculator$new()

          libraryFactory <- LibraryFactory$new()

          # Initialization, Fabricate the various models
          private$SL.library.fabricated <- libraryFactory$fabricate(SL.library.definition)
          private$SL.library.descriptions <- libraryFactory$getDescriptions(SL.library.definition)
          private$verbose && cat(private$verbose, paste('Creating super learner with a library:', private$SL.library.descriptions))

          # We need a weighted combination computer for each of the randomvariables.
          # We could reuse the WCC, and just save the weights here. However, this way we do allow
          # to use a different wcc for each of the random variables.
          private$weightedCombinationComputers <- list()

          self$getValidity()
        },

        getValidity = function() {
          if (length(private$SL.library.descriptions) == 0 || length(private$SL.library.fabricated) == 0 ) {
            throw("There should be at least one estimator in the library")
          }
          if (is.null(private$summaryMeasureGenerator) || class(private$summaryMeasureGenerator) != 'SummaryMeasureGenerator') {
            throw("You need to provide a summary measure generator of class SummaryMeasureGenerator")
          }
          if (!is.a(private$weightedCombinationComputers, 'list')) {
            throw("The WCC's should be in a list, one for each RV")
          }
        },

        evaluateModels = function(data, randomVariables) {
          names_outcome <-  c('dosl','osl')
          result <- lapply(names_outcome, function(sl_name) {
            discrete <- ifelse(sl_name == 'dosl', TRUE, FALSE)
            predicted.outcome <- self$predict(data = data, randomVariables = randomVariables, discrete = discrete)
            observed.outcome <- data[,colnames(predicted.outcome), with=FALSE]
            outcome <- private$cv_risk_calculator$calculate_evaluation(predicted.outcome = predicted.outcome,
                                                      observed.outcome = observed.outcome, 
                                                      randomVariables = randomVariables)
          })
          names(result) <- names_outcome
          result
        },

        # Samples the data iteratively from the fitted distribution, and applies an intervention if necessary
        # @param tau is the time at which we want to measure the outcome
        sample_iteratively = function(data, randomVariables, tau = 10, intervention = NULL,
                                      discrete = TRUE) {

          randomVariables <- RandomVariable.find_ordering(randomVariables)
          valid_intervention <- is.numeric(intervention$when) &
           is.numeric(intervention$what) &
           is.character(intervention$variable) &
           length(intervention$when) == length(intervention$what)
          if(!valid_intervention) throw('The intervention specified is not correct! it should have a when (specifying t), a what (specifying the intervention) and a variable (specifying the name of the variable to intervene on).')
          result <- data.table()

          for (t in seq(tau)) {
            data[,names(randomVariables)] <- NA
            for (rv in randomVariables) {
              current_outcome <- rv$getY
              if (!is.null(intervention) & current_outcome == intervention$variable & t %in% intervention$when) {
                when.idx <- which(intervention$when==t)
                outcome <- intervention$what[when.idx]
                private$verbose && cat(private$verbose, 'Setting intervention on ', current_outcome,' with ', outcome, ' on time ', t)
              } else {
                outcome <- self$predict(data = data, randomVariables = c(rv), discrete = discrete)
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
        # initial_data_size = the number of observations needed to fit the initial model
        run = function(data, W, A, Y, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20) {

          private$summaryMeasureGenerator$setData(data = data)

          # TODO: Move to check validity? Needs moving of the equations as well.
          if(!private$summaryMeasureGenerator$checkEnoughDataAvailable(randomVariables = c(W, A, Y))) {
            throw('Not all provided variables are included in the SMGs, include the correct SMGs')
          }

          # We initialize the WCC's here because we need to have the WAY's
          private$initialize_weighted_combination_calculators(c(W,A,Y))

          # Get the initial data for fitting the first model and train the initial models
          private$summaryMeasureGenerator$getNextN(initial_data_size) %>%
            private$trainLibrary(data.initial = ., randomVariables = c(W, A, Y))

          # Update the library of models using a given number of max_iterations
          private$updateLibrary(randomVariables = c(W,A,Y), max_iterations = max_iterations,
                                mini_batch_size = mini_batch_size)

          # Return the cross validated risk
          return(self$get_cv_risk)
        },

        # Predict should return a nrow(data) * 1 matrix, where the predictions are multiplied by
        # the weights of each estimator.
        predict = function(data, randomVariables, discrete = FALSE) {
          if (!self$isFitted){
            return(NA)
          }

          # I'm using a for loop here because I want to set the name of the predictions.
          # It would be great if we could do this in a lapply, but I don't know if that's
          # possible. Another option is to do 2 lapply;s (1 for name, 1 for predict), but 
          # that's worse than this I guess.
          if (discrete) {
            predictions <- list()
            for (rv in randomVariables) {
              outcome <- rv$getY
              prediction <- private$odsl.estimators[[outcome]]$predict(datO = data,
                                                                       X = rv$getX,
                                                                       Y = rv$getY)
              predictions[[outcome]] <- prediction
            }

            do.call(cbind,predictions) %>%
              as.data.table %>%
              return(.)
          }

          predictions <- private$predictUsingAllEstimators(data=data)
          weights <- sapply(private$weightedCombinationComputers, function(wcc) wcc$get_weights)

          # TODO: What if A ends up not being binary?
          # TODO: More important, what if a variable is discrete?
          lapply(randomVariables, function(rv) {
            current_rv_name <- rv$getY
            result <- do.call(cbind, predictions) %>%
              subset(., select = grep(paste(current_rv_name,"$",sep=""), names(.))) %>%
              as.matrix(.) %*% weights[,current_rv_name]

            colnames(result) <- current_rv_name
            if(rv$getFamily == 'binomial') {
              result2 <- ifelse(result > 0.5, 1,0)
            }
            result
          }) %>% 
            do.call(cbind, .) %>%
            as.data.table %>%
            return(.)
        },

        getModels = function() {
          return(private$SL.library.fabricated)
        }
  )
)

