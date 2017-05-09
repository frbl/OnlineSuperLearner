#' ML.H2O
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @include ML.Base.R
#' @include H2O.Interactor.R
ML.H2O <- R6Class("ML.H2O",
  inherit = ML.Base,
  public =
    list(
      initialize = function(verbose = FALSE) {
      }
    ),
  active = 
    list(
    ),
  private =
    list(
      # This interactor is shared among all H2O classes! By design
      interactor = H2O.Interactor$new(),

      get_checkpoint = function(m.fit) {
        checkpoint <- NULL
        if(!is.null(m.fit) && 'coef' %in% m.fit){
          checkpoint <- m.fit$coef@model_id
        }
        checkpoint
      },

      do.update = function(X_mat, Y_vals, m.fit) {
        # Every update adds a new tree, so we have to increase the number of trees
        private$ntrees <- private$ntrees + 1
        checkpoint <- private$get_checkpoint(m.fit)
        private$do.fit(X_mat, Y_vals, checkpoint = checkpoint)
      },

      do.predict = function(X_mat, m.fit) {
        # Upload the data to h2o. 
        pointer <- private$interactor$get_data_pointer(X_mat)
        h2o.predict(object = m.fit$coef, newdata = pointer) %>% 
          as.vector %>%
          return
      },

      catch_warning = function(fn, ...) {
        withCallingHandlers(fn(...),
          warning = function(w){
            if(grepl("Dropping constant columns: \\[Intercept\\].", w$message)){
              invokeRestart("muffleWarning")
            } else {
              message(w$message)
            }
          }
        )
      }
    )
)
