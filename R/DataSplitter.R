#' DataSplitter
#'
#' Splits data into train and test set.
#' @docType class
#' @importFrom R6 R6Class
DataSplitter <-
  R6Class (
           "DataSplitter",
           private =
             list(
                  data.previous = NULL
                  ),
           public =
             list(
                  initialize = function() {
                  },

                  split = function(data){
                    # Use the last observation as test set
                    test <- tail(data, 1)

                    if(is.null(self$model)){
                      if(nrow(data) < 2){
                        throw('At least 2 rows of data are needed, 1 train and 1 test')
                      }
                      # use the rest of the observations as trainingset
                      train <- head(data, nrow(data)-1)
                    } else {
                      # Use the rest of the observations as trainingset, including the previous testset
                      train <- rbind(private$data.previous, head(data, nrow(data) - 1))
                    }

                    private$data.previous <- test

                    
                    return(list(train = train, test = test))
                  }
                  )
           )
