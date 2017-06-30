#' ML.H2O.randomForest
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
ML.H2O.randomForest <- R6Class("ML.H2O.randomForest",
  inherit = ML.H2O,
  public =
    list(
      initialize = function(nfolds = 0, ntrees = 50, min_rows = 1) {
        super$initialize()
        private$nfolds = nfolds
        private$ntrees = ntrees
        private$min_rows = min_rows
      }
    ),
  private =
    list(
      nfolds = NULL,
      ntrees = NULL,
      min_rows = NULL,

      do.fit = function (X_mat, Y_vals, checkpoint = NULL) {

        # TODO: this is probably a bug
        unique_val <- unique(Y_vals)
        # This would be a working (but bad) workaround 
        # (just incorrectly specifying one of the datapoints)
        #if(length(unique_val) == 1) {
          #Y_vals[1] = ifelse(unique_val == 0, 1, 0)
        #}

        if (nrow(X_mat) <= private$min_rows || length(unique_val) == 1) {
          # We are not able to deal with such small datasets for now.
          # Therefore we throw in order for the condensier package to
          # pick this up and start a GLM. This saves time, as we don't
          # have to call H2O and wait for it to fail.
          throw('Not enough measurements available for h2o forest')
        }

        pointer <- private$interactor$get_data_pointer(cbind(X_mat, Y_vals))

        private$catch_warning(h2o.randomForest, x = colnames(X_mat), y='Y_vals',
                         training_frame = pointer,
                         binomial_double_trees = TRUE,
                         ntrees = private$ntrees,
                         nfolds = private$nfolds,
                         min_rows = private$min_rows,
                         checkpoint = checkpoint) %>%
          return
      }
    )
)
