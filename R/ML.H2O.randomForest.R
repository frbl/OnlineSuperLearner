#' ML.H2O.randomForest
#'
#' Wrapper for an H2O randomForest estimator. From their discription:
#' Distributed Random Forest (DRF) is a powerful classification and regression
#' tool. When given a set of data, DRF generates a forest of classification (or
#' regression) trees, rather than a single classification (or regression) tree.
#' Each of these trees is a weak learner built on a subset of rows and columns.
#' More trees will reduce the variance. Both classification and regression take
#' the average prediction over all of their trees to make a final prediction,
#' whether predicting for a class or numeric value (note: for a categorical
#' response column, DRF maps factors (e.g. 'dog', 'cat', 'mouse) in
#' lexicographic order to a name lookup array with integer indices (e.g. 'cat
#' -> 0, 'dog' -> 1, 'mouse' -> 2).
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom h2o h2o.randomForest
#' @include ML.H2O.R
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(ntrees=50, min_rows=9) }}{ 
#'     Creates a new randomforest / drf model
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
ML.H2O.randomForest <- R6Class("ML.H2O.randomForest",
  inherit = ML.H2O,
  public =
    list(
      initialize = function(nfolds = 0, ntrees = 50, min_rows = 1, verbose = FALSE) {
        super$initialize()
        private$nfolds <- nfolds
        private$ntrees <- ntrees
        private$min_rows <- min_rows
        private$verbose <- verbose
      }
    ),
  active = 
    list(
      get_nfolds = function() {
        return(private$nfolds)
      },
      get_ntrees = function() {
        return(private$ntrees)
      },
      get_min_rows = function() {
        return(private$min_rows)
      },
      get_verbose = function() {
        return(private$verbose)
      }
    ),
  private =
    list(
      nfolds = NULL,
      ntrees = NULL,
      min_rows = NULL,
      verbose = NULL,

      do.predict = function(X_mat, m.fit) {
      },

      do.update = function(X_mat, Y_vals, m.fit) {
      },

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
