#' SMG.Transformation
#' Transforms the current value of a variable using the given function
#' 
#' @docType class
#' @importFrom R6 R6Class
#'
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(function_to_use = sin, suffix = "sin", colnames_to_use) }}{ 
#'     <<description>> 
#'     @param function_to_use = sin 
#'     @param suffix = "sin" 
#'     @param colnames_to_use  
#'   } 
#' 
#'   \item{\code{update(data.current) }}{ 
#'     <<description>> 
#'     @param data.current  
#'   } 
#' 
#'   \item{\code{process(data.current) }}{ 
#'     <<description>> 
#'     @param data.current  
#'   } 
#' 
#'   } 
#' 
#' }  
SMG.Transformation <- R6Class("SMG.Transformation",
  public =
    list(
        initialize = function(function_to_use = sin, suffix = 'sin', colnames_to_use) {
          private$function_to_use <- function_to_use
          private$suffix <- suffix
          private$exposed_columnames <- paste(colnames_to_use, suffix , sep='_')
          private$colnames_to_use <- colnames_to_use
        },

        update = function(data.current) {
          data <- private$function_to_use(data.current[,private$colnames_to_use, with=FALSE])
          colnames(data) <- private$exposed_columnames
          data
        },

        process = function(data.current) {
          if(nrow(data.current) < self$minimalObservations){
            stop(paste('At least', self$minimalObservations, 'observations required'))
          } 
          data <- self$update(data.current)
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
