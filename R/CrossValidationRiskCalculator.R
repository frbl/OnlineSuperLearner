#' CrossValidationRiskCalculator
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize() }}{
#'     <<description>>
#'   }
#'
#'   \item{\code{calculate_risk(predicted.outcome, observed.outcome, randomVariables}}{
#'     <<description>>
#'     @param predicted.outcome
#'     @param observed.outcome
#'     @param randomVariables
#'   }
#'
#'   \item{\code{update_risk(predicted.outcome = predicted.outcome, observed.outcome = observed.outcome}}{
#'     <<description>>
#'     @param predicted.outcome
#'     @param observed.outcome
#'   }
#'
#' }
CrossValidationRiskCalculator <- R6Class("CrossValidationRiskCalculator",
  public =
    list(
        initialize = function() {
        },

        # Calculate the CV risk for each of the random variables provided
        calculate_risk = function(predicted.outcome, observed.outcome, randomVariables, useAsLoss=TRUE){
          cv_risk <- list()
          if (is.a(predicted.outcome, 'list')) {
            # MOVE TO SEPARTE FUNCTION
            for (rv in randomVariables) {
              current.outcome <- 
              lossFn <- Evaluation.get_evaluation_function(rv$getFamily, useAsLoss = useAsLoss)
              cv_risk[[rv$getY]] <- lossFn(data.observed = observed.outcome[[rv$getY]],
                                            data.predicted = predicted.outcome[[rv$getY]]) 
            }
          } else {
            algorithms <- rownames(predicted.outcome)
            for (rv in randomVariables) {
              current.outcome <- predicted.outcome[[rv$getY]]
              names(current.outcome) <- algorithms
              lossFn <- Evaluation.get_evaluation_function(rv$getFamily, useAsLoss = useAsLoss)
              for (algorithm in algorithms) {
                cv_risk[[rv$getY]][[algorithm]] <- lossFn(data.observed = observed.outcome[[rv$getY]],
                                              data.predicted = current.outcome[[algorithm]]) 
              }
            }
          }
          return(cv_risk)
        },

        update_risk = function(predicted.outcome, observed.outcome, randomVariables,
                               current_count, current_risk) {

          updated_risk <- self$calculate_risk(predicted.outcome = predicted.outcome,
                                                  observed.outcome = observed.outcome, 
                                                  randomVariables= randomVariables)

          for(rv in randomVariables) {
            #TODO: Is this calulation correct now?
            current <- rv$getY 

            # The score up to now needs to be calculated current_count times, the other score 1 time.
            new_risk <- (1 / (current_count + 1)) * updated_risk[[current]] 
            if(!is.null(current_risk[[current]])){
              new_risk <- new_risk +(current_count / (current_count + 1)) * current_risk[[current]] 
            }

            current_risk[[current]] <- new_risk
          }
          
          return(current_risk)
        }
        ),
  active =
    list(
        ),
  private =
    list(
    )
)
