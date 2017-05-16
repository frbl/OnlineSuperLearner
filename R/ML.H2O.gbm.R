#' ML.H2O.gbm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
ML.H2O.gbm <- R6Class("ML.H2O.gbm",
  inherit = ML.H2O,
  public =
    list(
      nfolds = NULL,
      fitfunname='h2ogbm',
      lmclass='h2ogbmR6',
 
      initialize = function(nfolds = 0, ntrees=50, min_rows=9) {
        super$initialize()
        private$ntrees = ntrees
        private$min_rows = min_rows
      }
    ),
  private =
    list(
      prev = NULL,
      min_rows = NULL,
      ntrees = NULL,

      do.fit = function (X_mat, Y_vals, checkpoint = NULL) {
        # TODO: this is probably a bug
        unique_val <- unique(Y_vals)
        if(length(unique_val) == 1) {
          Y_vals[1] = ifelse(unique_val == 0, 1, 0)
        }

        print(head(Y_vals))
        print(head(X_mat))

        pointer <- private$interactor$get_data_pointer(cbind(X_mat, Y_vals))
        private$catch_warning(h2o.gbm, x = colnames(X_mat), y = 'Y_vals',
                training_frame = pointer,
                nfolds = self$nfolds,
                ntrees = private$ntrees,
                min_rows = private$min_rows,
                checkpoint = checkpoint) %>%
          return
      }
    ),
)
