#' ML.H2O.randomForest
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
ML.H2O.randomForest <- R6Class("ML.H2O.randomForest",
  inherit = ML.H2O,
  public =
    list(
      initialize = function(nfolds = 0, ntrees = 50) {
        super$initialize()
        private$nfolds = nfolds
        private$ntrees = ntrees
      }
    ),
  private =
    list(
      nfolds = NULL,
      ntrees = NULL,

      do.fit = function (X_mat, Y_vals, checkpoint = NULL) {

        # TODO: this is probably a bug
        unique_val <- unique(Y_vals)
        if(length(unique_val) == 1) {
          Y_vals[1] = ifelse(unique_val == 0, 1, 0)
        }

        pointer <- private$interactor$get_data_pointer(cbind(X_mat, Y_vals))

        private$catch_warning(h2o.randomForest, x = colnames(X_mat), y='Y_vals',
                         training_frame = pointer,
                         ntrees = private$ntrees,
                         nfolds = private$nfolds,
                         checkpoint = checkpoint) %>%
          return
      }
    )
)
