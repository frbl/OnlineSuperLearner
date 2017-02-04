#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#' @field SL.Library the library of machinelearning algorithms to use
#' @importFrom R6 R6Class
#' @import h2o
#'
#' @section Details:
#' \code{$new(SL.Library)} starts a new OnlineSuperLearner. The provided /code{SL.Library}
#' contains the machine learning models to use
#'
#' \code{$run()} runs the actual OnlineSuperLearning calculation
#'
#' \code{$getModel()} returns the final OnlineSuperLearner model
#' @export
OnlineSuperLearner <-
  R6Class (
           "OnlineSuperLearner",
           #TODO: this representation should probably accept a datareader object from which
           # it retrieves lines of data. Also, we should add a parameter in which an already
           # fitted model can be inputted, so it gets updated.
           private =
             list(
                  SL.Library = NULL
                  ),
           public =
             list(
                  initialize = function(SL.Library = c('ML.Local.lm','ML.H2O.glm')) {
                    private$SL.Library <- SL.Library
                  },

                  run = function() {
                    # Steps in superlearning:

                    # 1. Initialization
                    # 1.1 Check parameters (dimensions of X / Y)

                    # 1.2 Setup a connection with H2O
                    localh2o <- H2O.initializer(host='imac.evionix.org', runlocal = FALSE)

                    # 1.3 Load the data
                    data <- Data.Static$new(url='https://raw.github.com/0xdata/h2o/master/smalldata/logreg/prostate.csv')

                    # 1.4 Fabricate the various models
                    libraryFactory <- LibraryFactory$new()
                    SL.fabricated.models <- libraryFactory$fabricate(private$SL.Library, data = data)


                    # 2. Fit different models using cross validation
                    for (model in SL.fabricated.models) {
                      fitted <- model$fit(y = "CAPSULE", X = c("AGE","RACE","PSA","DCAPS"))
                      print(fitted)
                    }

                    # 3. Fit a final model which is a glm of the original models

                    # 4. return the final model

                    return(NULL)
                  },

                  getModel = function() {
                    return(NULL)
                  }
                  )
           )


main <- function() {
  osl <- OnlineSuperLearner$new()
  osl$run()
}
