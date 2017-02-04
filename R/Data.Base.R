#' Data.Base
#' @importFrom R6 R6Class
Data.Base <-
  R6Class (
           "Data.Base",
           public =
             list(
                  initialize = function() {
                  },

                  getNext = function(){
                    throw('This method needs to be inherited')
                  }
                  )
           )
