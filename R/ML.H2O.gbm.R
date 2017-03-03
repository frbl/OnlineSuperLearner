#' ML.H2O.gbm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
ML.H2O.gbm <-
  R6Class (
           "ML.H2O.gbm",
           inherit = ML.H2O,
           private =
             list(
                  prev = NULL,
                  min_rows = NULL,
                  ntrees = NULL
                  ),
           public =
             list(
                  nfolds = NULL,

                  initialize = function(nfolds = 0, ntrees=50, min_rows=9) {
                    super$initialize()
                    private$ntrees = ntrees
                    private$min_rows = min_rows
                  },

                  fit = function(train, Y, A, W){
                    checkpoint <- private$getCheckpoint()
                    if(!is.null(private$prev) &&
                       !all(private$prev == colnames(train))){
                      throw('All columns must be equal, every iteration!')
                    }
                    private$prev <- colnames(train)

                    self$model <- h2o.gbm(x = c(A,W), y = Y,
                                          training_frame = train,
                                          nfolds = self$nfolds,
                                          ntrees = private$ntrees,
                                          min_rows = private$min_rows,
                                          checkpoint = checkpoint)

                    # Every update adds a new tree, so we have to increase the number of trees
                    private$ntrees <- private$ntrees + 1
                    return(NULL)
                  }
                  )
           )
