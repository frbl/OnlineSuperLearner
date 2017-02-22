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
                  prev = NULL
                  ),
           public =
             list(
                  nfolds = NULL,

                  initialize = function(nfolds = 0) {
                    super$initialize()
                  },


                  fit = function(train, test, Y, A, W){
                    # TODO:! This is teribly inefficient and is merely for testing
                    train.hex <- as.h2o(train, key="train.hex")
                    test.hex <- as.h2o(test, key="test.hex")

                    X <- c(A, W)
                    checkpoint <- private$getCheckpoint()
                    if(!is.null(private$prev) &&
                       !all(private$prev == colnames(train))){
                      throw('All columns must be equal, every iteration!')
                    }
                    private$prev <- colnames(train)

                    # TODO: This currently fails because it probably
                    # has unseen strata in the new data? (it fails
                    # because of the checkpoint)
                    self$model <- h2o.gbm(x = X, y = Y,
                                          training_frame = train.hex,
                                          validation_frame = test.hex,
                                          nfolds = self$nfolds,
                                          checkpoint = checkpoint)

                    h2o.cross_validation_predictions(self$model)
                  }
                  )
           )
