#' ML.H2O.randomForest
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
ML.H2O.randomForest <-
  R6Class (
           "ML.H2O.randomForest",
           inherit = ML.H2O,
           private =
            list(
                  nfolds = NULL,
                  ntrees = NULL
                ),
           public =
             list(
                  initialize = function(nfolds = 0, ntrees = 50) {
                    super$initialize()
                    private$nfolds = nfolds
                    private$ntrees = ntrees
                  },

                  fit = function(train, Y, A, W){

                    checkpoint <- private$getCheckpoint()

                    self$model <- h2o.randomForest(x = c(A,W), y = Y,
                                          training_frame = 'train.hex',
                                          ntrees = private$ntrees,
                                          nfolds = private$nfolds,
                                          checkpoint = checkpoint)

                    # Every update adds a new tree, so we have to increase the number of trees
                    private$ntrees <- private$ntrees + 1
                  }
                  )
           )
