#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#' @docType class
#' @import R.oo
#' @import R.utils
#' @importFrom R6 R6Class
#' @include LibraryFactory.R
#' @include SummaryMeasureGenerator.R
#' @include Simulator.Simple.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(SL.Library)}}{starts a new OnlineSuperLearner. The provided /code{SL.Library} contains the machine learning models to use}
#'
#'   \item{\code{run(data, X, Y, initial.data.size = 10)}}{runs the actual OnlineSuperLearning calculation}
#'   \item{\code{getModel()}}{returns the final OnlineSuperLearner model}
#'   \item{\code{predict(data, X)}}{returns an actual prediction using the superlearning model}
#' }
#' @export
OnlineSuperLearner <-
  R6Class (
           "OnlineSuperLearner",
           #TODO: this representation should probably accept a datareader object from which
           # it retrieves lines of data. Also, we should add a parameter in which an already
           # fitted model can be inputted, so it gets updated.
           private =
             list(
                  SL.Library = NULL,
                  SL.Library.Fabricated = NULL,
                  summaryMeasurementGenerator = NULL,

                  trainLibrary = function(X, Y, iterations, data.initial){
                    data <- data.initial
                    while(iterations > 0 && nrow(data) >= 1 && !is.null(data)) {
                      lapply(private$SL.Library.Fabricated, function(model) { model$fit(data = data, X = X, Y = Y) })
                      data <- private$summaryMeasurementGenerator$getNext()
                      iterations <- iterations - 1
                    }
                    data
                  }
                  ),
           public =
             list(
                  initialize = function(SL.Library = c('ML.Local.lm', 'ML.H2O.glm')) {
                    private$SL.Library <- SL.Library

                    # Initialization, Fabricate the various models
                    LibraryFactory <- LibraryFactory$new()
                    private$SL.Library.Fabricated <- LibraryFactory$fabricate(private$SL.Library)
                  },

                  # Data = the data object from which the data can be retrieved
                  # X = the names of the confounders
                  # Y = the name of the outcome
                  # initial.data.size = the number of observations needed to fit the initial model
                  run = function(data, X, Y, initial.data.size = 10) {
                    # Steps in superlearning:
                    # Wrap the data into a summary measurement generator.
                    private$summaryMeasurementGenerator <- SummaryMeasureGenerator$new(Y = Y,
                                                                                       X = X,
                                                                                       data = data,
                                                                                       lags = 2)

                    # Get the initial data for fitting the first model
                    data <- private$summaryMeasurementGenerator$getNextN(initial.data.size)

                    # Fit the library of models using a given number of iterations
                    iterations.max <- 500
                    X <- private$summaryMeasurementGenerator$X
                    Y <- private$summaryMeasurementGenerator$Y
                    data <- private$trainLibrary(X = X, Y = Y, iterations = iterations.max, data = data)

                    # Calculate the accuracy of the prediction based on the remaining data
                    true.predicted <- 0
                    all.predicted <- 0
                    while(nrow(data) >= 1 && !is.null(data)) {
                      for (model in private$SL.Library.Fabricated) {
                        prediction <-  model$predict(data = data, X = X)
                        all.predicted <- all.predicted + 1
                        if((prediction>0.5) == (data[, Y, with = FALSE][[Y]] > 0.5))
                          true.predicted  <- true.predicted + 1
                      }

                      data <- private$summaryMeasurementGenerator$getNext()
                    }
                    print(paste('Accuracy:', true.predicted / all.predicted))


                    # build a matrix with predicted / actual
                    # Fit a final model which is a glm of the original models

                    return(private$SL.Library.Fabricated)
                  },

                  getModel = function() {
                    return(NULL)
                  },

                  predict = function(newdata, X = NULL, Y = NULL, onlySL = FALSE, ...) {
                    return(NULL)
                  }

                  )
           )

##################
# TEST FUNCTIONS #
##################
datatest <- function() {
  devtools::load_all()
  data <- Data.Static$new(url = 'https://raw.github.com/0xdata/h2o/master/smalldata/logreg/prostate.csv')

  smg <- SummaryMeasureGenerator$new(data = data,
                                     lags = 3)
  smg$getNext()

  print(smg$getNext())
  smg
}

main <- function() {
  devtools::document()
  sim  <- Simulator.Simple$new()
  data  <- sim$getObservation(1000)
  Y = "CAPSULE"
  Y = "y"
  X = c("AGE", "RACE", "PSA", "DCAPS")
  X = c("x1", "x2")
  SL.Library = c('ML.Local.lm', 'ML.XGBoost.glm')
  SL.Library = c('ML.XGBoost.glm')
  osl <- OnlineSuperLearner$new(SL.Library)
  #data <- Data.Static$new(url = 'https://raw.github.com/0xdata/h2o/master/smalldata/logreg/prostate.csv')
  data <- Data.Static$new(dataset = data)
  osl$run(data, X =  X, Y = Y)
}
