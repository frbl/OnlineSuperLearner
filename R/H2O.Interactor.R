#' H2O.Interactor
#'
#' Class used to interact with H2O, mainly for caching data. It keeps track off
#' all the data that has been uploaded to H2O, and returns the cached version
#' if it is already on H2O.
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom digest digest
#' @importFrom h2o as.h2o
#' @include H2O.initializer.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialized(verbose = FALSE, digest_algorithm = 'sha1')}}{
#'     Initializes a new h2o interactor. The provided digest algorithm is used
#'     to calculate unique hashes of data when storing it (and to looking it
#'     back up in the cache).
#'
#'     @param verbose the verbosity to use
#'     @param digest_algorithm (default = 'sha1') string, the algorithm to use for the hashing
#'     @return a new intereactor
#'   }
#'
#'   \item{\code{generate_hash(data)}}{
#'     Generates a checksum for a piece of data (e.g., datatable). This is used
#'     as a pointer to store the data under.
#'
#'     @param data the data table to hash
#'     @return string the hash of the datatable
#'   }
#'
#'   \item{\code{get_data_pointer(data)}}{
#'     Function that either uploads or reuses a dataset to H2O and returns the
#'     pointer to the data on H2O. It will always return a pointer to data on
#'     H2O.
#'
#'     @param data the data to send to H2O.
#'     @return string the identifier on H2O.
#'   }
#'
#'   \item{\code{add_and_return_data_hash(data)}}{
#'     Function that adds data to a specific hash location in the h2o cache. 
#'
#'     @param hash string hash to use for caching the data.
#'     @param data the data to send to H2O.
#'     @return string the identifier on H2O.
#'   }
#'
#'   \item{\code{get_h2o_objects()}}{
#'     Active method. Function that returns the raw h2o objects.
#'
#'     @param hash string hash to use for caching the data.
#'     @param data the data to send to H2O.
#'     @return string the identifier on H2O.
#'   }
#' }
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
        hash <- digest(data, algo = private$digest_algorithm, serialize=TRUE)
        ## We add the a here because h2o needs a name that starts with a char.
        ## We could replace the sha1 with something that only produces
        ## characters
        paste('a',hash,sep='_')
      },

      get_data_pointer = function(data) {
        ## TODO: This is a critical section and should be made threadsafe if we
        ## have more than 1 thread

        hash <- self$generate_hash(data = data)
        if(hash %in% names(private$h2o_objects)) {
          private$verbose && cat(private$verbose, 'Not uploading data to h2o...')
          return(private$h2o_objects[[hash]])
        }
        private$verbose && cat(private$verbose, 'Uploading data to h2o...')
        self$add_and_return_data_hash(hash = hash, data = data)
      },

      add_and_return_data_hash = function(hash, data){
        private$h2o_objects[[hash]] <- private$as_h2o(data = data, destination = hash)
        private$h2o_objects[[hash]]
      }
    ),
  active =
    list(
      get_h2o_objects = function(){
        private$h2o_objects
      }
    ),
  private =
    list(
      verbose = NULL,
      digest_algorithm = NULL,
      h2o_objects = NULL,

      as_h2o = function(data, destination) {
        as.h2o(data, destination = destination)
      },

      do.predict = function(X_mat, m.fit) {
        # Upload the data to h2o. 
        pointer <- private$interactor$get_data_pointer(X_mat)
        as.data.table(h2o.predict(object = self$get_model, newdata = pointer))
      }
    )
)
