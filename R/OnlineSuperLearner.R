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
        weightedCombinationComputer = NULL,

        # Functions
        # =========

        # Find the best estimator among the current set, for each of the outcomes (WAY)
        find_current_best_estimator = function() {
          lapply(self$get_cv_risk, function(risk) {
            names(risk[which(risk == min(risk))])[1] %>%
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

        # Predict using all estimators separately.
        # Params:
        # @param data.initial: the initial dataset to train the estimators on
        # @param Y: the column names used for the outcome
        # @param A: the column names used for the treatment
        # @param W: the column names used for the covariates
        # @return a vector of outcomes, each entry being the predicted outcome of an estimator
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
              model$sample(current)
            })

          # convert the list of results into a data.table
          result <- rbindlist(result)
          rownames(result) <- private$SL.library.descriptions
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
          private$verbose && enter(private$verbose, 'Training all models')
          #dataH2o <- as.h2o(data)
          #private$verbose && cat(private$verbose, 'Uploaded data to h2o')

          lapply(private$SL.library.fabricated,
                  function(model) {
                    #if(is.a(model, 'ML.H2O')){
                      #current <- dataH2o
                    #} else {
                    current <- data
                    #}
                    model$process(current, randomVariables = randomVariables)
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

          # Fit the initial models
          private$trainAllEstimators(data = data.splitted$train,
                                      randomVariables = randomVariables)

          # TODO: Make sure that all predictions are returned in the same format
          predicted.outcome <- private$predictUsingAllEstimators(data = data.splitted$test)
          observed.outcome <- data.splitted$test[,colnames(predicted.outcome), with=FALSE]

          # Calculate the error
          private$update_risk(predicted.outcome = predicted.outcome,
                                                  observed.outcome = observed.outcome, 
                                                  randomVariables= randomVariables)

          # Fit the super learner on the current data
          #Ymat <- as.matrix(data.initial[, Y, with = FALSE])
          #private$fit(predicted.outcome = predictions, observed.outcome = Ymat )

          # Update the discrete superlearner (take the first if there are multiple candidates)
          private$odsl.estimators <- private$find_current_best_estimator()
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

            # Make a prediction using our (super)learners
            predicted.outcome <- private$predictUsingAllEstimators(data = data.splitted$test)
            observed.outcome <- data.splitted$test[,colnames(predicted.outcome), with=FALSE]

            #osl.prediction <- self$predict(data = data.splitted$test)
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
            #private$fit(predicted.outcome = CVpredictions, observed.outcome = observed.outcome)

            # Update the discrete superlearner (take the first if there are multiple candidates)
            private$odsl.estimators <- private$find_current_best_estimator()

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
          private$weightedCombinationComputer$process(Z = predicted.outcome,
                                                      Y = observed.outcome,
                                                      private$SL.library.descriptions)

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


          # TODO: DIP the WCC
          weights.initial <- rep(1 / length(private$SL.library.descriptions), length(private$SL.library.descriptions))
          private$weightedCombinationComputer <- WCC.NLopt$new(weights.initial = weights.initial)

          self$getValidity()
        },

        getValidity = function() {
          if (length(private$SL.library.descriptions) == 0 || length(private$SL.library.fabricated) == 0 ) {
            throw("There should be at least one estimator in the library")
          }
          if (is.null(private$summaryMeasureGenerator) || class(private$summaryMeasureGenerator) != 'SummaryMeasureGenerator') {
            throw("You need to provide a summary measure generator of class SummaryMeasureGenerator")
          }
          if (!is.a(private$weightedCombinationComputer, 'WeightedCombinationComputer')) {
            throw("The provided WCC is not a WCC")
          }
        },

        evaluateModels = function(data, randomVariables) {
          predicted.outcome <- self$predict(data = data, randomVariables = randomVariables, discrete = TRUE)
          observed.outcome <- data[,names(predicted.outcome), with=FALSE]
          print('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
          private$cv_risk_calculator$calculate_risk(predicted.outcome = predicted.outcome,
                                                    observed.outcome = observed.outcome, 
                                                    randomVariables = randomVariables,
                                                    useAsLoss = FALSE)
        },

        # Samples the data iteratively from the fitted distribution, and applies an intervention if necessary
        # @param tau is the time at which we want to measure the outcome
        sample_iteratively = function(data, randomVariables, tau = 10, intervention = NULL,
                                      discrete = TRUE) {

          randomVariables <- RandomVariable.find_ordering(randomVariables)

          for (t in seq(tau)) {
            data[,names(randomVariables)] <- NA
            for (rv in randomVariables) {
              current_outcome <- rv$getY
              if (!is.null(intervention) & current_outcome == intervention$variable & t %in% intervention$when) {
                when.idx <- which(intervention$when==t)
                what <- intervention$what[when.idx]
                private$verbose && cat(private$verbose, 'Setting intervention on ', current_outcome,' with ', what, ' on time ', t)
                data[[current_outcome]] <- what
              } else {
                private$verbose && cat(private$verbose,'Predicting ', current_outcome, ' using ', paste(rv$getX, collapse=', '))
                data[[current_outcome]] <- self$predict(data = data,
                                                         randomVariables = c(rv),
                                                         discrete = discrete)
              }
            }
            if(t != tau) data <- private$summaryMeasureGenerator$getLatestCovariates(data)
          }
          return(data[,names(randomVariables), with = FALSE])
        },


        # Data = the data object from which the data can be retrieved
        # initial_data_size = the number of observations needed to fit the initial model
        run = function(data, W, A, Y, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20) {

          private$summaryMeasureGenerator$setData(data = data)

          # TODO: Move to check validity? Needs moving of the equations as well.
          if(!private$summaryMeasureGenerator$checkEnoughDataAvailable(randomVariables = c(W, A, Y))) {
            throw('Not all provided variables are included in the SMGs, include the correct SMGs')
          }

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
          if(!self$isFitted){
            return(NA)
          }

          if(discrete){
            # I'm using a for loop here because I want to set the name of the predictions.
            # It would be great if we could do this in a lapply, but I don't know if that's
            # possible. Another option is to do 2 lapply;s (1 for name, 1 for predict), but 
            # that's worse than this I guess.
            predictions <- list()
            for (rv in randomVariables) {
              outcome <- rv$getY
              predictions[[outcome]] <- private$odsl.estimators[[outcome]]$predict(datO = data,
                                                           X = rv$getX,
                                                           Y = rv$getY)
            }
            return(predictions)
          }

          # This will be the convex combination of the data and the models (and weights)
          # something like
          # private$summaryMeasureGenerator$setData(data = data)
          # data <- private$summaryMeasureGenerator$getNext()
          #predictions <- as.matrix(private$predictUsingAllEstimators(data=data, A=A, W=W))
          #weights <- as.matrix(private$weightedCombinationComputer$getWeights)

          #return(predictions %*% weights)
        },

        getModels = function() {
          return(private$SL.library.fabricated)
        }

  )
)

