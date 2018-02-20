#' SMG.Transformation
#'
#' The OnlineSuperLearner uses various \code{SummaryMeasureGenerator} instances
#' to generate new variables that can be used to represent the relevant history
#' of a variable. The \code{SMG.Transformation} class allows a user to
#' transform the current value of a variable using a given function (like a
#' \code{sin} or \code{cos}).
#' 
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(function_to_use = sin, suffix = "sin", colnames_to_use) }}{ 
#'     Initializes a new \code{SMG.Transformation} object.
#'
#'     @param function_to_use function (default = sin) the function to use for
#'      transforming a value.
#'
#'     @param suffix string (default = "sin") the string to use as suffix for
#'      the generated column names.
#'
#'     @param colnames_to_use vector a vector of string containing all
#'      variables for which a transformation needs to be created.
#'   } 
#' 
#'   \item{\code{update(data.current) }}{ 
#'     For online learning we need to be able to create new data blocks on the
#'     fly (as not all data is available beforehand. This function updates a
#'     set of variables with the function specified on initialization.
#'     
#'     @param data.current data.table the current data / the last data used for
#'      training, from which a new summary measure needs to be generated.
#'
#'     @return data.table a new block for the next training iteration.
#'   } 
#' 
#'   \item{\code{process(data.current) }}{ 
#'     Adds the transformed columns for each of the specified colnames.  It
#'     goes through the data and selects the variables specified on
#'     initialization.
#'
#'     @param data.current data.table the currently available data.
#'
#'     @return data.table with the new summary measure columns.
#'   } 
#'
#'   \item{\code{exposedVariables}}{ 
#'     Active method. Returns a list of variables returned by this SMG.
#'
#'     @return vector a vector of strings conaining all specified (used)
#'      contemporaneous colnames.
#'   } 
#' 
#'   \item{\code{minimalObservations}}{ 
#'     Active method. The minimal number of measurements needed for this SMG to
#'     be able to generate a new block. This is 1.
#'
#'     @return integer the minimal number of measurements needed.
#'   } 
#' }  
SMG.Transformation <- R6Class("SMG.Transformation",
  inherit = SMG.Base,
  public =
    list(
      initialize = function(function_to_use = sin, suffix = 'sin', colnames_to_use) {
        private$function_to_use <- function_to_use
        private$suffix <- suffix
        private$exposed_columnames <- paste(colnames_to_use, suffix , sep = '_')
        private$colnames_to_use <- colnames_to_use
      },

      update = function(data.current) {
        result <- self$process(data.current) %>% unlist
        return(result)
      },

      process = function(data.current) {
        self$check_enough_available(data.current)
        data <- private$function_to_use(data.current[,private$colnames_to_use, with=FALSE])
        colnames(data) <- private$exposed_columnames
        return(data)
      }
    ),
  active =
    list(
      minimalObservations = function() {
        1
      },

      exposedVariables = function() {
        private$exposed_columnames
      }
    ),
  private =
    list(
      exposed_columnames = NULL,
      suffix = NULL,
      function_to_use = NULL,
      colnames_to_use = NULL
    )
)
