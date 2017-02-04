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
                    if (private$lazyLoad && !is.null(private$url)) {
                      return(private$url)
                    }

                    if (!is.null(private$dataset)) {
                      return(private$dataset)
                    }

                    # If all fails, load the data locally in a dataframe and return that
                  },

                  getNext = function() {

                  }
                  )
           )
