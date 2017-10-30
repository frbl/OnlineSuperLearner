#' Class to provide the functionality of treating a static, finite datatable as a stream of incomming data.
#'
#' @docType class
#' @include Data.Base.R
#' @importFrom R6 R6Class
#' @import data.table
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(dataset = NULL, url = NULL, verbose = FALSE)}}{
#'     This method is used to create object of this class. One can provide either a \code{datatable} or a \code{url} pointing to a dataframe.
#'     @param dataset is a datatable to read the data from
#'     @param url is a datatable to read the data from if no dataset is available
#'     @param verbose the verbosity
#'   }
#'
#'   \item{\code{initialize(dataset = NULL, url = NULL, verbose = FALSE)}}{
#'     This method is used to create object of this class. One can provide either a \code{datatable} or a \code{url} pointing to a dataframe.
#'     @param dataset is a datatable to read the data from
#'     @param url is a datatable to read the data from if no dataset is available
#'     @param verbose the verbosity
#'   }
#'
#'   \item{\code{getNext()}}{
#'     Method to retrieve the next observation from the data.
#'     @return datatable a row from a datatable
#'   }
#'
#'   \item{\code{getNextN(n = 1)}}{
#'     Method that returns the next \code{n} observations. This function can be used to bootstrap an initial model or to
#'     get a minibatch of observations. Note that the function will always try to return data. If one asks for n
#'     observations, it will check if there are still n new observations. If not, it will return all observations still
#'     available. If there are no observations available, it will return null.
#'     @param n = the number of measurements requested
#'     @return datatable a row or number of rows from a datatable, NULL if \code{n} <= 0
#'   }
#'
#'   \item{\code{reset}}{
#'     Method to reset the pointer to the beginning of the datatable.
#'   }
#'
#'   \item{\code{get_all}}{
#'     Method to retrieve the whole dataset at once.
#'     @return a datatable as set when initializing the object
#'   }
#'
#'   \item{\code{get_length}}{
#'     Method to retrieve the number of rows in the dataframe
#'     @return integer the number of rows in the dataframe. 0 and a warning if no data is set.
#'   }
#'
#'   \item{\code{get_remaining_length}}{
#'     Method to retrieve the number of rows still remaining in the dataframe
#'     @return integer the remaining number of rows in the dataframe.
#'   }
#'
#'   \item{\code{get_currentrow}}{
#'     Method to retrieve pointer to the current row in the data frame.
#'     @return integer the current row in the dataframe. 1 if no data is set.
#'   }
#' }
#' @export
Data.Static <-
  R6Class (
  "Data.Static",
  inherit = Data.Base,
  public =
    list(
        initialize = function(dataset = NULL, url = NULL, verbose = FALSE) {
          if (!is.null(dataset)) {
            ## Make sure the dataset is a data table
            if (!is.data.table(dataset)) dataset <- data.table(dataset)
            private$dataset <- dataset
          } else if (!is.null(url)) {
            private$dataset <- private$read_data_from_url(url = url)
          } else {
            throw('You need to provide at least a datatable or url')
          }

          ## Initialize the pointer to the data
          self$reset

          private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)
          private$verbose && cat(private$verbose, 'Static set initialized with:', head(dataset))
        },

        getNext = function() {
          temp <- private$dataset[private$currentrow, ]
          private$increase_pointer(1)
          return(temp)
        },

        getNextN = function(n = 1) {
          #if(round(private$currentrow) != private$currentrow) browser()
          max <- self$get_remaining_length

          ## Check if the max value > 0
          ## And return less than n if it is smaller than max
          n <- min(n, max)
          if(n <= 0) return(NULL)

          temp <- private$dataset[private$currentrow:((private$currentrow + n)-1), ]
          private$increase_pointer(n)
          return(temp)
        }
    ),
  active =
    list(
      reset = function() {
        private$currentrow <- 1
      },

      get_all = function() {
        return(private$dataset)
      },

      get_length = function() {
        nrow(self$get_all)
      },

      get_remaining_length = function() {
        max(self$get_length - self$get_currentrow + 1, 0)
      },

      get_currentrow = function() {
        return(private$currentrow)
      }
    ),
  private =
    list(
      dataset = NULL,
      currentrow = NULL,
      verbose = NULL,

      increase_pointer = function(n = 1) {
        private$currentrow <- private$currentrow + n
      },

      read_data_from_url = function(url) {
        ## TODO: Test the file, which format it should be
        data.table(read.csv(url))
      }
    )
)
