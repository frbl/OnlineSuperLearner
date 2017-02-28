#' ML.H2O.gbm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @export
ML.H2O.gbm <-
  R6Class (
           "ML.H2O.gbm",
           inherit = ML.H2O,
           private =
             list(
                  prev = NULL,
                  ntrees = NULL
                  ),
           public =
             list(
                  nfolds = NULL,

                  initialize = function(nfolds = 0, ntrees=50) {
                    super$initialize()
                    private$ntrees = ntrees
                  },

                  fit = function(train, Y, A, W){
                    # TODO:! This is teribly inefficient and is merely for testing
                    train.hex <- as.h2o(train, key="train.hex")

                    checkpoint <- private$getCheckpoint()
                    if(!is.null(private$prev) &&
                       !all(private$prev == colnames(train))){
                      throw('All columns must be equal, every iteration!')
                    }
                    private$prev <- colnames(train)

                    # TODO: This currently fails because it probably
                    # has unseen strata in the new data? (it fails
                    # because of the checkpoint)
                    self$model <- h2o.gbm(x = c(A,W), y = Y,
                                          training_frame = train.hex,
                                          nfolds = self$nfolds,
                                          ntrees = private$ntrees,
                                          checkpoint = checkpoint)

                    # Every update adds a new tree, so we have to increase the number of trees
                    private$ntrees <- private$ntrees + 1

                  }
                  )
           )
