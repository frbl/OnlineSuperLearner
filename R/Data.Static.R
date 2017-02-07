#' Data.Static
#' @importFrom R6 R6Class
#' @include Data.Base.R
Data.Static <-
  R6Class (
           "Data.Static",
           inherit = Data.Base,
           private =
             list(
                  dataset = NULL,
                  url = NULL,
                  lazyLoad = NULL,
                  currentrow = 1,

                  readDataFromUrl = function() {
                    # TODO: Test the file, which format it should be
                    read.csv(private$url)
                  }
                  ),
           public =
             list(
                  initialize = function(dataset = NULL, url = NULL, lazyload=TRUE) {
                    super$initialize()
                    private$dataset <- dataset
                    private$url <- url
                    private$lazyLoad <- lazyload
                  },

                  getAll = function() {
                    if (!is.null(private$dataset)) {
                      return(private$dataset)
                    }

                    if (!is.null(private$url)) {
                      if (private$lazyLoad) {
                        return(private$url)
                      }else {
                        private$dataset <- private$readDataFromUrl()
                        return(private$dataset)
                      }
                    }
                    # If all fails, load the data locally in a dataframe and return that
                  },


                  # Treat the dataframe as a stream as well.
                  getNext = function() {
                    if (is.null(private$dataset)) {
                        private$dataset <- private$readDataFromUrl()
                    }
                    temp <- private$dataset[private$currentrow,]
                    private$currentrow <- private$currentrow + 1
                    return(temp)
                  }
                  )
           )
