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
#'   \item{\code{new(dataset, url)}}{This method is used to create object of this class. One can provide either a \code{datatable} or a \code{url} pointing to a dataframe.}
#'
#'   \item{\code{getAll()}}{Method to retrieve the whole dataset at once.}
#'   \item{\code{getNext()}}{Method to retrieve the next observation from the data.}
#'   \item{\code{getNextN(number.of.observations)}}{Method that returns the next \code{number.of.observations} observations. This function can be used to bootstrap an initial model.}
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

           public =
             list(
                  initialize = function(dataset = NULL, url = NULL) {
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
                  },

                  getAll = function() {
                    return(private$dataset)
                  },

                  getNext = function() {
                    temp <- private$dataset[private$currentrow, ]
                    private$currentrow <- private$currentrow + 1
                    return(temp)
                  },

                  getNextN = function(number.of.observations = 1) {
                    max <- nrow(private$dataset) - private$currentrow
                    number.of.observations <- Arguments$getInteger(number.of.observations, c(1, max))

                    temp <- private$dataset[private$currentrow:number.of.observations, ]
                    private$currentrow <- private$currentrow + number.of.observations
                    return(temp)
                  }
                  )
           )
