#' ML.H2O.gbm
#'
#' Wrapper for an H2O gbm estimator. From their discription:
#' Gradient Boosting Machine (for Regression and Classification) is a forward
#' learning ensemble method. The guiding heuristic is that good predictive
#' results can be obtained through increasingly refined approximations. H2O's
#' GBM sequentially builds regression trees on all the features of the dataset
#' in a fully distributed way - each tree is built in parallel.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom h2o h2o.gbm
#' @include ML.H2O.R
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(ntrees=50, min_rows=9) }}{ 
#'     Creates a new gbm model
#'
#'     @param nfolds integer (default = 0) specify the number of folds for
#'      cross-validation.
#'
#'     @param ntrees integer (default = 50) specify the number of trees to build.
#'
#'     @param min_rows (default = 9) specify the minimum number of observations for a leaf (nodesize in R).
#'
#'     @param verbose (default = FALSE) the verbosity of the fitting procedure
#'   } 
#' }  
ML.H2O.gbm <- R6Class("ML.H2O.gbm",
  inherit = ML.H2O,
  public =
    list(
      fitfunname='h2ogbm',
      lmclass='h2ogbmR6',
 
      initialize = function(nfolds = 0, ntrees=50, min_rows=9, verbose=FALSE) {
        super$initialize()
        private$ntrees = ntrees
        private$min_rows = min_rows
        private$nfolds = nfolds
        private$verbose = verbose
      }
    ),
  private =
    list(
      prev = NULL,
      min_rows = NULL,
      ntrees = NULL,
      nfolds = NULL,
      verbose = NULL,

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
                ntrees = private$ntrees,
                nfolds = nfolds,
                min_rows = private$min_rows,
                checkpoint = checkpoint) %>%
          return
      }
    ),
)
