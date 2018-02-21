#' DataSplitter
#'
#' Splits a dataset into a train and test set.
#'
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(test_set_size = 1) }}{ 
#'     Creates a new datasplitter
#'
#'     @param test_set_size (default = 1) an integer to specify the size of the
#'      test set to use.
#'   } 
#' 
#'   \item{\code{split(data)}}{ 
#'     Splits the data in a train and test set. A block that was used as a test
#'     set previously is automatically appended to the traininset (so all data
#'     gets used for training that has been used for testing before).
#'
#'     In the current implementation it will always use the last observation as
#'     the test set observation.  Eventually this should be configurable, and
#'     \code{n} number of observations should be includable.
#'
#'     @param data data.table the data to spit into a train and test set.
#'
#'     @param test_set_size integer (default = NULL) overrides the initialized
#'      test_set_size. If \code{NULL}, we use the initialized one (default).
#'
#'     @return list with two entries: \code{train} and \code{test}. Each
#'      containing the respective dataframe.
#'   } 
#'
#'   \item{\code{get_test_set_size}}{ 
#'     Active method. The size of the testset used by this splitter instance.
#'   } 
#' }  
#' @docType class
#' @importFrom R6 R6Class
DataSplitter <- R6Class("DataSplitter",
  public =
    list(
      initialize = function(test_set_size = 1) {
        ## Initialize the number of blocks that need to be used as test set
        private$test_set_size <- Arguments$getInteger(test_set_size, c(1,Inf))
      },

      split = function(data, test_set_size = NULL){
        ## We offer the capability to override the initial testsize.
        if(is.null(test_set_size)) test_set_size <- self$get_test_set_size

        if (is.null(private$data.previous) && nrow(data) < (test_set_size + 1)) {
          throw('At least ', test_set_size + 1, ' rows of data are needed, ',
                '1 train and ', test_set_size,' test')
        }

        ## Use the last tau observation as test set
        test <- tail(data, test_set_size)

        ## use the rest of the observations as trainingset
        train <- head(data, nrow(data) - test_set_size)

        if(!is.null(private$data.previous)){
          ## Use the rest of the observations as trainingset, including the previous testset
          ## Note, rbind or rbindlist doesnt matter here in timings,
          ## https://gist.github.com/frbl/ef64fe8d5c935ddf9af7fddecf01a600
          train <- rbind(private$data.previous, train)
        }
        private$data.previous <- test
        return(list(train = train, test = test))
      }
    ),
  active =
    list(
      get_test_set_size = function() {
        return(private$test_set_size)
      }
    ),
  private =
    list(
      data.previous = NULL,
      test_set_size = NULL
    )
)
