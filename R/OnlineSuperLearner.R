#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include Global.R
#' @include LibraryFactory.R
#' @include SummaryMeasureGenerator.R
#' @include WeightedCombinationComputer.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(SL.Library)}}{
#'     starts a new OnlineSuperLearner. The provided /code{SL.Library} contains the machine learning models to use
#'   }
#'
#'   \item{\code{run(data, Y, A, W,  initial.data.size = 10)}}{
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
OnlineSuperLearner <-
  R6Class (
           "OnlineSuperLearner",
           private =
             list(
                  # Variables
                  # The superLearnerModel itself (the weights).
                  osl.weights = NULL,
                  dosl.estimator = NULL,
                  fitted = NULL,

                  # The R.cv score of the current fit
                  risk.cv = NULL,

                  # ML Library
                  SL.Library = NULL,
                  SL.Library.Fabricated = NULL,

                  # The family of data we are working with
                  family = NULL,

                  # Summary measures and a generator
                  summaryMeasureGenerator = NULL,

                  # Verbosity of the logs
                  verbose = FALSE,

                  # The computer for the SuperLearner combination
                  weightedCombinationComputer = NULL,

                  # Functions
                  # Function to train the initial set of models
                  # Params:
                  # @param data.initial: the initial dataset to train the estimators on
                  # @param Y: the column names used for the outcome
                  # @param A: the column names used for the treatment
                  # @param W: the column names used for the covariates
                  trainLibrary = function(data.initial, Y, A, W) {
                    private$verbose && enter(private$verbose, 'Starting model training')
                    if(self$fitted){
                      warning('DOSL and OSL already fitted (initially), skipping initialization')
                      return(FALSE)
                    }


                    # Fit the initial models
                    # TODO: Make sure that all predictions are returned in the same format
                    predictions <- predictUsingAllEstimators(data = data.current, Y = Y, A = A, W = W)

                    # Fit the super learner on the current data
                    Ymat <- as.matrix(data.initial[, Y, with = FALSE])
                    private$fit(predicted.outcome = predictions, observed.outcome = Ymat )

                    # Update the discrete superlearner (take the first if there are multiple candidates)
                    private$dosl.estimator <-
                      private$SL.Library.Fabricated[which(predictions == min(predictions))[1]]

                    private$fitted = TRUE
                    private$verbose && exit(private$verbose)
                    TRUE
                  },

                  # Predict using all estimators separately.
                  # Params:
                  # @param data.initial: the initial dataset to train the estimators on
                  # @param Y: the column names used for the outcome
                  # @param A: the column names used for the treatment
                  # @param W: the column names used for the covariates
                  # @return a vector of outcomes, each entry being the predicted outcome of an estimator
                  predictUsingAllEstimators = function(data, Y, A, W) {
                    unlist(lapply(private$SL.Library.Fabricated,
                                  function(model) { model$process(data = data, Y = Y, A = A, W = W) }))

                  },


                  # Function to update the models with the available data
                  # Params:
                  # @param Y: the column names used for the outcome
                  # @param A: the column names used for the treatment
                  # @param W: the column names used for the covariates
                  # @param max.iterations: the number of iterations we can maximaly run for training
                  updateLibrary = function(Y, A, W, max.iterations){
                    private$verbose && enter(private$verbose, 'Starting model updating')
                    if(!self$fitted){
                      throw('Fit the inital D-OSL and OSL first')
                    }

                    # Set the current timestep to 0
                    t <- 0

                    data.current <- private$summaryMeasureGenerator$getNext()

                    # TODO: Check wether the stopping criteria are met (e.g., improvement < theta)
                    while(t < max.iterations && nrow(data.current) >= 1 && !is.null(data.current)) {

                      # Update all models on the previous data set, and get their prediction on the current data.
                      predictions <- predictUsingAllEstimators(data = data.current, Y = Y, A = A, W = W)

                      # Make a prediction using our (discrete) superlearner
                      osl.prediction <- self$predict(data = data.current, A = A, W = W)
                      dosl.prediction <- self$predict(data = data.current, A = A, W = W, discrete = TRUE)

                      # Calculate the updated cross validated risk using the new measurement
                      predicted.outcome <- c(predictions, osl.prediction, dosl.prediction)
                      observed.outcome <- tail(data.current[, Y, with = FALSE], 1)

                      # Update the cross-validated risk
                      risk.cv.update <- self$lossFunction(data.observed = observed.outcome, predicted.outcome) / t
                      private$R.cv <- (t / (t + 1)) * private$risk.cv + risk.cv.update

                      # ReFit the super learner on the current data
                      private$fit(predicted.outcome = predicted.outcome, observed.outcome = observed.outcome )

                      # Update the discrete superlearner (take the first if there are multiple candidates)
                      private$dosl.estimator <- private$SL.Library.Fabricated[which(predictions == min(predictions))[1]]

                      # Get the new row of data
                      data.current <- private$summaryMeasureGenerator$getNext()

                      t <- t + 1
                    }
                    private$verbose && exit(private$verbose)
                  },

                  # Fit the actual super learner on all models in the current set of models
                  #
                  # Params:
                  # @param predicted.outcome: the outcome predicted by the learning algorithms
                  # @param observed.outcome: the outcome we observed in the actual dataset
                  fit = function(predicted.outcome, observed.outcome) {

                    # If we have 1 estimator, the weighted combination of that estimator
                    # is just the estimator itself.
                    if (length(private$SL.Library.Fabricated) == 1) {
                      private$osl.weights <- c(1)
                      return()
                    }

                    private$verbose && enter(private$verbose, 'Starting training super learner')

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
                    if(self$fitted){

                      # Fit initial model
                      self$osl.weights <- weightedCombinationComputer$compute(X= predicted.outcome, Y=observed.outcome, private$SL.Library)

                    } else {

                      # Do gradient descent update
                      # Something like:
                      #Xmat <- model.matrix(formula, train)
                      #suppressWarnings(prediction <- self$predict(train, A, W))
                      #gradient <- (t(Xmat) %*% (prediction - Ymat))
                      #self$model <- self$model - private$learning.rate * gradient

                    }
                    suppressWarnings(prediction <- self$predict(train, A, W))

                    # New model
                    private$verbose && exit(private$verbose)
                  }

                  ),
           active =
             list(
                  fitted = function(){
                    private$fitted
                  },

                  lossFunction = function() {
                    if (private$family == 'gaussian') {
                      return(Evaluation.MeanSquaredError)
                    } else if(private$familty == 'binomial') {
                      return(Evaluation.Accuracy)
                    } else {
                      throw('No evaluation measure implemented for family', private$family)
                    }
                  }),
           public =
             list(
                  initialize = function(SL.Library = c('ML.Local.lm', 'ML.H2O.glm'), summaryMeasureGenerator, verbose = FALSE, family='gaussian') {
                    private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
                    private$verbose && cat(private$verbose, paste('Created super learner with a library:', private$SL.Library))

                    private$family <- family
                    private$SL.Library <- SL.Library

                    # Initialization, Fabricate the various models
                    LibraryFactory <- LibraryFactory$new()
                    private$SL.Library.Fabricated <- LibraryFactory$fabricate(private$SL.Library)

                    # Wrap the data into a summary measurement generator.
                    private$summaryMeasureGenerator <- summaryMeasureGenerator

                    # TODO: DIP the WCC
                    private$osl.weights <- rep(1 / length(private$SL.Library), length(private$SL.Library))
                    private$weightedCombinationComputer <- WCC.NNLS$new(obsWeights = private$osl.weights)

                    # The initial risk is 0. Should probably be Inf
                    private$risk.cv = 0

                    self$getValidity()
                  },

                  getValidity = function() {
                    if (length(private$SL.Library) == 0 || length(private$SL.Library.Fabricated) == 0 ) {
                      throw("There should be at least one estimator in the library")
                    }
                    if (is.null(private$summaryMeasureGenerator) || class(private$summaryMeasureGenerator) != 'SummaryMeasureGenerator') {
                      throw("You need to provide a summary measure generator of class SummaryMeasureGenerator")
                    }
                    if (is.null(private$family) || private$family == "") {
                      throw("The provided family is not valid")
                    }
                    if (class(private$weightedCombinationComputer) != 'WeightedCombinationComputer') {
                    }

                  },

                  evaluateModels = function(data, W, A, Y) {
                    lapply(private$SL.Library.Fabricated,
                           function(model) {
                             data.predicted <-  model$predict(data = data, A = A,  W = W)
                             data.observed <- data[, Y, with = FALSE][[Y]]
                             self$lossFunction(data.observed = data.observed,
                                               data.predicted = data.predicted)
                           })
                  },

                  run = function(data, W, A, Y, initial.data.size = 5, max.iterations = 20) {
                    # Data = the data object from which the data can be retrieved
                    # initial.data.size = the number of observations needed to fit the initial model

                    # Steps in superlearning:
                    # Get the initial data for fitting the first model
                    private$summaryMeasureGenerator$setData(data = data)
                    data.initial <- private$summaryMeasureGenerator$getNextN(initial.data.size)

                    # Train the library of models using the initial dataset
                    private$trainLibrary(data.initial = data.initial, Y = Y, A = A, W = W)

                    # Update the library of models using a given number of max.iterations
                    private$updateLibrary(Y = Y, A = A, W = W, max.iterations = max.iterations)

                    # Return the cross validated risk
                    return(private$risk.cv)
                  },

                  predict = function(newdata, A = NULL, W = NULL, discrete = FALSE) {
                    if(is.null(private$dosl.estimator) || is.null(private$osl.weights)){
                      return(NA)
                    }

                    if(discrete){
                      return(private$dosl.estimator$predict(data = newdata, A = A, W = W))
                    }

                    # This will be the convex combination of the data and the models (and weights)
                    # something like
                    # private$summaryMeasureGenerator$setData(data = data)
                    # data <- private$summaryMeasureGenerator$getNext()
                    outcomes <-
                      newdata[, X] %*% private$osl.weights
                    #
                    # Where the weights W are trained / fitted
                    return(1)
                  },

                  getModels = function() {
                    return(private$SL.Library.Fabricated)
                  }

                  )
           )

