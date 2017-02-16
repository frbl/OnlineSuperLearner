#' General packages used by all of the other classes
#' @import R.oo
#' @import R.utils
#' @import magrittr
generalImports <- list()

# General fixes, for usability
expit <- plogis
logit <- qlogis

#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#' @docType class
#' @importFrom R6 R6Class
#' @include LibraryFactory.R
#' @include SummaryMeasureGenerator.R
#' @include Simulator.Simple.R
#' @include Simulator.GAD.R
#' @include Simulator.Slow.R
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
                  SMG.list = NULL,
                  verbose = FALSE,

                  trainLibrary = function(Y, A, W, iterations, data.initial){
                    private$verbose && enter(private$verbose, 'Starting model training')

                    data.current <- data.initial
                    while(iterations > 0 && nrow(data.current) >= 1 && !is.null(data.current)) {

                      private$verbose && enter(private$verbose, paste('Iterations left', iterations ))

                      # Fit the models on the current data
                      lapply(private$SL.Library.Fabricated, function(model) { model$fit(data = data.current, Y = Y, A = A, W = W) })
                      data.current <- private$summaryMeasurementGenerator$getNext()
                      iterations <- iterations - 1

                      # Print some of the results
                      if(private$verbose) lapply(private$SL.Library.Fabricated, function(model) { private$verbose && cat(private$verbose, model$score) })
                      private$verbose && exit(private$verbose)
                    }
                    private$verbose && exit(private$verbose, 'Finished model training')
                    data.current
                  }
                  ),
           public =
             list(
                  initialize = function(SL.Library = c('ML.Local.lm', 'ML.H2O.glm'), SMG.list, verbose = -3) {
                    private$SL.Library <- SL.Library

                    # Initialization, Fabricate the various models
                    LibraryFactory <- LibraryFactory$new()
                    private$SL.Library.Fabricated <- LibraryFactory$fabricate(private$SL.Library)
                    private$SMG.list <- SMG.list
                    private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
                  },

                  # Data = the data object from which the data can be retrieved
                  # W = the names of the confounders
                  # A = the names of the treatment
                  # Y = the name of the outcome
                  # initial.data.size = the number of observations needed to fit the initial model
                  run = function(data, Y, A, W, initial.data.size = 5, iterations.max = 20) {
                    private$verbose && cat(private$verbose, paste('Starting super learner with a library:', private$SL.Library))

                    # Steps in superlearning:
                    # Wrap the data into a summary measurement generator.
                    private$summaryMeasurementGenerator <- SummaryMeasureGenerator$new(Y = Y,
                                                                                       A = A,
                                                                                       W = W,
                                                                                       data = data,
                                                                                       SMG.list = private$SMG.list)

                    # Get the initial data for fitting the first model
                    data <- private$summaryMeasurementGenerator$getNextN(initial.data.size)

                    # Fit the library of models using a given number of iterations
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
  set.seed(12345)
  sim  <- Simulator.Simple$new()
  dataset  <- sim$getObservation(1000)

  data <- Data.Static$new(dataset = dataset)
  Y = "y"
  W = c("x1", "x2")
  A = c()
  SMG.list <- list(
                   SMG.Lag$new(lags = 2, colnames.to.lag = (c(A, W, Y))),
                   SMG.Latest.Entry$new(colnames.to.use = (c(A, W, Y)))
                   )

  smg <- SummaryMeasureGenerator$new(Y = Y,
                                     A = A,
                                     W = W,
                                     data = data,
                                     SMG.list = SMG.list)
  smg$getNext()

  print(smg$getNext())
  smg
}

main <- function() {
  suppressWarnings(devtools::document())

  log <- Arguments$getVerbose(-8, timestamp=TRUE)
  metrics <- data.table()
  sim  <- Simulator.GAD$new()
  nobs <- 1e5

  ######################################
  # Generate observations for training #
  #####################################
  llW <- list(stochMech=rnorm,
                param=c(0, 0.5, -0.25, 0.1),
                rgen=identity)

  llA <- list (stochMech=function(ww) {
                 rbinom(length(ww), 1, expit(ww))
                },
                param=c(-0.1, 0.1, 0.25),
                rgen=function(xx, delta=0.05){
                  rbinom(length(xx), 1, delta+(1-2*delta)*expit(xx))
                })

  llY <- list(stochMech=function(aa, ww){
                  aa*ww+(1-aa)*(-ww)
                },
                param=c(0.1, 0.1, 0.1, 0.05, -0.01),
                rgen=identity)
  ##
  data.train <- sim$simulateWAY(nobs, qw=llW, ga=llA, Qy=llY, verbose=log)
  data.test <- sim$simulateWAY(1000, qw=llW, ga=llA, Qy=llY, verbose=log)
  browser()

  Y = "y"
  W = c("x1", "x2")
  A = c()

  SMG.list <- list(
                   SMG.Lag$new(lags = 2, colnames.to.lag = (c(A, W, Y))),
                   SMG.Latest.Entry$new(colnames.to.use = (c(A, W, Y)))
                   )

  set.seed(12345)
  data <- Data.Static$new(dataset = data.train)

  SL.Library = c('ML.Local.lm')
  #data <- Data.Static$new(url = 'https://raw.github.com/0xdata/h2o/master/smalldata/logreg/prostate.csv')

  osl <- OnlineSuperLearner$new(SL.Library, SMG.list)
  models <- osl$run(data, Y = Y, A = A, W =  W, initial.data.size = 2, iterations.max = 200)

  accuracy <- Evaluation.Accuracy(models[[1]], data = data.test, W = W, A = A, Y = Y)
  #metrics <- rbindlist(list(metrics, list(i = i, acc = accuracy)))
  #print(metrics)
}
