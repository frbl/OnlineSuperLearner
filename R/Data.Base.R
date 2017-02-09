#' Data.Base
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
Data.Base <-
  R6Class (
           "Data.Base",
           public =
             list(
                  initialize = function() {
                  },

                  getNext = function(){
                    throw('This method needs to be inherited')
                  },

                  getNextN = function(n=1){
                    dt <- data.table()
                    i <- 0
                    while(i < n) {
                      dt <- rbindlist(list(dt, self$getNext()))
                      i <- i +1
                    }
                    dt
                  }
                  )

           )
