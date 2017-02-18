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
#' @include Simulator.Simple.R
#' @include Simulator.GAD.R
#' @include Simulator.Slow.R
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
                  # The superLearnerModel itself
                  superLearnerEstimator = NULL,

                  # ML Library
                  SL.Library = NULL,
                  SL.Library.Fabricated = NULL,

                  family = NULL,

                  # Summary measures and a generator
                  summaryMeasureGenerator = NULL,

                  # Verbosity of the logs
                  verbose = FALSE,

                  # Function to train the whole set of models
                  trainLibrary = function(Y, A, W, iterations, data.initial){
                    private$verbose && enter(private$verbose, 'Starting model training')

                    data.current <- data.initial
                    while(iterations > 0 && nrow(data.current) >= 1 && !is.null(data.current)) {
                      private$verbose && enter(private$verbose, paste('Iterations left', iterations ))

                      # Fit the models on the current data
                      lapply(private$SL.Library.Fabricated,
                             function(model) { model$process(data = data.current, Y = Y, A = A, W = W) })

                      # Fit the super learner on the current data
                      private$fitSuperlearner(data = data.current, Y = Y, A = A, W = W)

                      # Get the new row of data
                      data.current <- private$summaryMeasureGenerator$getNext()
                      iterations <- iterations - 1

                      # Print some of the results
                      if(private$verbose) {
                        accuracy <- self$evaluateModels(data = data.current,
                                                        Y = Y,
                                                        A = A,
                                                        W = W)

                        private$verbose && cat(private$verbose, paste('Performance on trainingset', accuracy))
                      }
                      private$verbose && exit(private$verbose)
                    }
                    private$verbose && exit(private$verbose)
                  },

                  # Fit the actual super learner on all models in the current set of models
                  fitSuperlearner = function(data, Y, A, W) {
                    if (length(private$SL.Library.Fabricated) == 1) {
                      # If we have 1 estimator, the weighted combination of that estimator
                      # is just the estimator itself.
                      private$superLearnerEstimator <- private$SL.Library.Fabricated[[1]]
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
                    # above, and we update this initial model useing the new observations
                    # as they come in.
                    private$superLearnerEstimator <- NULL # New model
                    private$verbose && exit(private$verbose)
                  }

                  ),
           active =
             list(
                  evaluationFunction = function() {
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
                  },

                  evaluateModels = function(data, W, A, Y) {
                    lapply(private$SL.Library.Fabricated,
                           function(model) {
                             self$evaluationFunction(model, data = data, Y = Y, A = A, W = W)
                           })
                  },

                  run = function(data, W, A, Y, initial.data.size = 5, iterations.max = 20) {
                    # Data = the data object from which the data can be retrieved
                    # initial.data.size = the number of observations needed to fit the initial model

                    # Steps in superlearning:
                    # Get the initial data for fitting the first model
                    private$summaryMeasureGenerator$setData(data = data)
                    data <- private$summaryMeasureGenerator$getNextN(initial.data.size)

                    # Fit the library of models using a given number of iterations
                    private$trainLibrary(Y = Y, A = A, W = W, iterations = iterations.max, data = data)

                    TRUE
                  },

                  predict = function(newdata, A = NULL, W = NULL, onlySL = FALSE, ...) {
                    # This will be the convex combination of the data and the models (and weights)
                    # something like
                    # private$summaryMeasureGenerator$setData(data = data)
                    # data <- private$summaryMeasureGenerator$getNext()
                    # data[, X] %*% W * private$models
                    #
                    # Where the weights W are trained / fitted
                    return(NULL)
                  },

                  getModels = function() {
                    return(private$SL.Library.Fabricated)
                  }


                  )
           )

