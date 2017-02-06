#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#' @field SL.Library the library of machinelearning algorithms to use
#' @importFrom R6 R6Class
#' @include LibraryFactory.R
#' @include H2O.Initializer.R
#' @import h2o
#'
#' @section Details:
#' \code{$new(SL.Library)} starts a new OnlineSuperLearner. The provided /code{SL.Library}
#' contains the machine learning models to use
#'
#' \code{$run()} runs the actual OnlineSuperLearning calculation
#'
#' \code{$getModel()} returns the final OnlineSuperLearner model
#'
#' \code{$getModel(data, X, Y, onlySL)} allows the user to make predictions on new data using
#' the fitted superlearner model. The data provided in this function is the new data object,
#' which can be either a stream of data or a dataframe. X is a vector of covariate names to use for the
#' prediction, Y is a vector containing the variables to predict
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
                  LibraryFactory = NULL
                  ),
           public =
             list(
                  initialize = function(SL.Library = c('ML.Local.lm','ML.H2O.glm')) {
                    private$SL.Library <- SL.Library
                    private$LibraryFactory <- LibraryFactory$new()
                    H2O.Initializer$new(host='imac.evionix.org', runlocal = FALSE)
                  },

                  run = function(data) {
                    # Steps in superlearning:

                    # 1. Initialization, Fabricate the various models
                    SL.fabricated.models <- private$LibraryFactory$fabricate(private$SL.Library, data = data)

                    # 2. Fit different models using cross validation
                    models <- lapply(SL.fabricated.models, function(model) {
                      model$fit(y = "CAPSULE", X = c("AGE","RACE","PSA","DCAPS"))
                    })

                    print(models)

                    # 3. build a matrix with predicted / actual

                    # 4. Fit a final model which is a glm of the original models

                    # 4. return the final model
                    return(models)
                  },

                  getModel = function() {
                    return(NULL)
                  },

                  predict = function(newdata, X = NULL, Y = NULL, onlySL = FALSE, ...) {
                    #TODO: Move X and Y to the data object?
                    return(NULL)
                  }

                  )
           )


main <- function() {
  SL.Library = c('ML.Local.lm','ML.H2O.glm')
  osl <- OnlineSuperLearner$new()
  data <- Data.Static$new(url='https://raw.github.com/0xdata/h2o/master/smalldata/logreg/prostate.csv')
  osl$run(data)
}
