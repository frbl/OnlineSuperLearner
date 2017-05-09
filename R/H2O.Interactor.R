#' H2O.Interactor
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom digest digest
#' @include H2O.initializer.R
H2O.Interactor <- R6Class("H2O.Interactor",
  public =
    list(
      initialize = function(verbose = FALSE, digest_algorithm = 'sha1') {
        H2O.Initializer(host = "localhost",
                        port = 54321,
                        runlocal = TRUE,
                        verbose = verbose)
        h2o_objects <- list()
        private$digest_algorithm <- Arguments$getCharacter(digest_algorithm)
        private$verbose <- Arguments$getVerbose(verbose)
      },

      generate_hash = function(data) {
        digest(data, algo = private$digest_algorithm, serialize=TRUE)
      },

      get_data_pointer = function(data) {
        # TODO: This is a critical section and should be made threadsafe if we have more than 1 thread

        # We add the a here because h2o needs a name that starts with a char. We could replace the sha1 
        # with something that only produces characters
        hash <- paste('a',self$generate_hash(data),sep='_')
        if(hash %in% names(private$h2o_objects)) {
          private$verbose && cat(private$verbose, 'Not uploading data to h2o...')
          return(private$h2o_objects[[hash]])
        }
        private$verbose && cat(private$verbose, 'Uploading data to h2o...')
        private$h2o_objects[[hash]] <- as.h2o(data, destination = hash)
        private$h2o_objects[[hash]]
      }
    ),
  active =
    list(
    ),
  private =
    list(
      verbose = NULL,
      digest_algorithm = NULL,
      h2o_objects = NULL,

      do.predict = function(X_mat, m.fit) {
        # Upload the data to h2o. 
        pointer <- private$interactor$get_data_pointer(X_mat)
        as.data.table(h2o.predict(object = self$get_model, newdata = pointer))
      }
    )
)
