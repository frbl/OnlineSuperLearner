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
                  lazyLoad = NULL
                  ),
           public =
             list(
                  initialize = function(dataset = NULL, url = NULL, lazyLoad=TRUE) {
                    super$initialize()
                    private$dataset <- dataset
                    private$url <- url
                    private$lazyLoad <- lazyLoad
                  },

                  getAll = function() {
                    if (!is.null(private$url)) {
                      if (private$lazyLoad) {
                        return(private$url)
                      }else {
                        return(self.readDataFromUrl())
                      }
                    }

                    if (!is.null(private$dataset)) {
                      return(private$dataset)
                    }

                    # If all fails, load the data locally in a dataframe and return that
                  },

                  readDataFromUrl = function() {
                    # TODO: Test the file, which format it should be
                    data = read.csv(self$url)
                  },

                  getNext = function() {

                  }
                  )
           )
