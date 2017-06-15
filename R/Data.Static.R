#' Class to provide the functionality of treating a static, finite datatable as a stream of incomming
#' data.
#'
#' @docType class
#' @include Data.Base.R
#' @importFrom R6 R6Class
#' @import data.table
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(dataset, url)}}{
#'     This method is used to create object of this class. One can provide either a \code{datatable} or a \code{url} pointing to a dataframe.
#'   }
#'
#'   \item{\code{getAll()}}{
#'     Method to retrieve the whole dataset at once.
#'   }
#'
#'   \item{\code{getNext()}}{
#'     Method to retrieve the next observation from the data.
#'   }
#'
#'   \item{\code{getNextN(n)}}{
#'     Method that returns the next \code{n} observations. This function can be used to bootstrap
#'     an initial model or to get a minibatch of observations. Note that the function will always
#'     try to return data. If one asks for n observations, it will check if there are still n new
#'     observations. If not, it will return all observations still available. If there are no
#'     observations available, it will return null.
#'     @param n = the number of measurements requested
#'   }
#' }
#' @export
Data.Static <-
  R6Class (
  "Data.Static",
  inherit = Data.Base,
  private =
    list(
        dataset = NULL,
        currentrow = 1,

        readDataFromUrl = function(url) {
          # TODO: Test the file, which format it should be
          data.table(read.csv(url))
        }
        ),
  active =
    list(
      is_data_set = function() {
        !is.null(self$getAll())
      },

      get_length = function() {
        if(self$is_data_set) {
          nrow(self$getAll())
        } else {
          warning('Data not yet set, returning 0')
          0
        }
      },

      get_remaining_length = function() {
        max(self$get_length - self$get_currentrow, 0)
      },

      get_currentrow = function() {
        return(private$currentrow)
      }
      ),
  public =
    list(
        initialize = function(dataset = NULL, url = NULL, verbose = FALSE) {
          if (!is.null(dataset)) {
            if (!is.data.table(dataset)) {
              dataset <- data.table(dataset)
            }
            private$dataset <- dataset
          } else if (!is.null(url)) {
            private$dataset <- private$readDataFromUrl(url)
          } else {
            throw('You need to provide at least a datatable or url')
          }
          self$reset()
          #TODO: Do proper verbosity check here
          if(verbose){
            print('Static set initialized with:')
            print(head(dataset))
          }
        },

        getAll = function() {
          return(private$dataset)
        },

        reset = function() {
          private$currentrow <- 1
        },

        getNext = function() {
          temp <- private$dataset[private$currentrow, ]
          private$currentrow <- private$currentrow + 1
          return(temp)
        },

        getNextN = function(n = 1) {
          max <- nrow(private$dataset) - private$currentrow + 1 %>%
           Arguments$getInteger(., c(1, Inf))

          # Check if the max value > 0
          n <- min(n, max)
          if(n <= 0) return(NULL)

          temp <- private$dataset[private$currentrow:((private$currentrow + n)-1), ]
          private$currentrow <- private$currentrow + n
          return(temp)
        }
    )
)
