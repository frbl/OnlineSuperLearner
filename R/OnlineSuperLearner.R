#' General packages used by all of the other classes
#' @import R.oo
#' @import R.utils
#' @import magrittr
generalImports <- list()

#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#' @docType class
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

                  trainLibrary = function(Y, A, W, iterations, data.initial){
                    data <- data.initial
                    while(iterations > 0 && nrow(data) >= 1 && !is.null(data)) {
                      lapply(private$SL.Library.Fabricated, function(model) { model$fit(data = data, Y = Y, A = A, W = W) })
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
                  # W = the names of the confounders
                  # A = the names of the treatment
                  # Y = the name of the outcome
                  # initial.data.size = the number of observations needed to fit the initial model
                  run = function(data, Y, A, W, initial.data.size = 5, iterations.max = 20) {
                    # Steps in superlearning:
                    # Wrap the data into a summary measurement generator.
                    private$summaryMeasurementGenerator <- SummaryMeasureGenerator$new(Y = Y,
                                                                                       A = A,
                                                                                       W = W,
                                                                                       data = data,
                                                                                       lags = 2)

                    # Get the initial data for fitting the first model
                    data <- private$summaryMeasurementGenerator$getNextN(initial.data.size)

                    # Fit the library of models using a given number of iterations
                    Y <- private$summaryMeasurementGenerator$Y
                    A <- private$summaryMeasurementGenerator$A
                    W <- private$summaryMeasurementGenerator$W
                    data <- private$trainLibrary(Y = Y, A = A, W = W, iterations = iterations.max, data = data)


                    # build a matrix with predicted / actual

                    # Fit a final model which is a glm of the original models

                    return(private$SL.Library.Fabricated)

                  },

                  getModel = function() {
                    return(NULL)
                  },

                  predict = function(newdata, A = NULL, W = NULL, onlySL = FALSE, ...) {
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
  metrics <- data.table()
  sim  <- Simulator.Simple$new()
  dataset  <- sim$getObservation(1000)
  dataset.test  <- sim$getObservation(100)
  for(i in seq(2,20,1)) {
    set.seed(12345)
    data <- Data.Static$new(dataset = dataset)


    Y = "y"
    W = c("x1", "x2")
    A = c()
    SL.Library = c('ML.XGBoost.glm')
    #data <- Data.Static$new(url = 'https://raw.github.com/0xdata/h2o/master/smalldata/logreg/prostate.csv')

    data.copy <- copy(data)
    osl <- OnlineSuperLearner$new(SL.Library)
    models <- osl$run(data, Y = Y, A = A, W =  W, initial.data.size = 4, iterations.max = i)

    accuracy <- Evaluation.Accuracy(models[[1]], data = dataset.test, W= W, A = A, Y = Y)
    metrics <- rbindlist(list(metrics, list(i = i, acc = accuracy)))
  }
  print(metrics)
}
