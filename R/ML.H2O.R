#' ML.H2O
#'
#' Base class for any h2o machine learning model. Extend this class if you want
#' to create a new machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.Base.R
#' @include H2O.Interactor.R
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize() }}{ 
#'     Creates a new ML.H2o base model. Note that this also initializes a new
#'     H2O interactor for storing and retrieving data.
#'   } 
#' 
#'   \item{\code{get_checkpoint(m.fit) }}{ 
#'     In order to perform an update on an existing H2O model, H2O uses
#'     checkpoints. In order to use this system together with our other
#'     implementations, we have this function to retrieve a checkpoint if one
#'     is available. The function returns \code{NULL} if no checkpoint is
#'     available, and it returns the model ID if there is a checkpoint.
#'
#'     @param m.fit h2o model (not an h2o base model class, but a plain h2o
#'      model) a fitted h2o model from which one wants to retrieve a checkpoint.
#'   } 
#' }  
ML.H2O <- R6Class("ML.H2O",
  inherit = ML.Base,
  public =
    list(
      initialize = function() {
         private$interactor = H2O.Interactor$new()
      },

      get_checkpoint = function(m.fit) {
        checkpoint <- NULL
        print(m.fit)
        if(!is.null(m.fit) && 'coef' %in% m.fit){
          checkpoint <- m.fit$coef@model_id
        }
        checkpoint
      }
    ),
  active = 
    list(
    ),
  private =
    list(
      # TODO: This interactor should be shared among all H2O classes! By design, but not its not because it makes the application slower (we don't use H2O)
      #interactor = H2O.Interactor$new(),
      interactor = NULL,

      do.update = function(X_mat, Y_vals, m.fit) {
        # Every update adds a new tree, so we have to increase the number of trees
        private$ntrees <- private$ntrees + 1
        checkpoint <- self$get_checkpoint(m.fit)
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
