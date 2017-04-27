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

        calculate_evaluation = function(predicted.outcome, observed.outcome, randomVariables, add_evaluation_measure_name=TRUE) {
          evaluate <- function(current.predicted.outcome) {
            sapply(randomVariables, function(rv) {
              current_outcome <- rv$getY
              lossFn <- Evaluation.get_evaluation_function(rv$getFamily, useAsLoss = FALSE)
              result <- lossFn(data.observed  = observed.outcome[,current_outcome, with=FALSE],
                              data.predicted = current.predicted.outcome[,current_outcome, with=FALSE]) 
              if (add_evaluation_measure_name){
                names(result) <- paste(names(result), current_outcome, sep='.')
              } else {
                names(result) <- current_outcome
              }
              result
            }) %>% as.list
          }

          if (is.a(predicted.outcome, 'data.table')) {
            return(evaluate(predicted.outcome))
          } else if (is.a(predicted.outcome, 'list')) {
            return(lapply(predicted.outcome, evaluate))
          }

          throw('Input predicted.outcome should be a data.table or list of data.tables')
        },

        # Calculate the CV risk for each of the random variables provided
        # Output is a list of lists
        calculate_risk = function(predicted.outcome, observed.outcome, randomVariables){
          if (!is.a(predicted.outcome, 'list')) {
            throw('Input predicted.outcome should be a list, where each entry is the outcome of an estimator')
          }

          cv_risk <- lapply(predicted.outcome, function(algorithm_outcome) {
            # The as.list unlist is a hack to flatten the result, but to keep the correct names
            # that the entries got in the loop.
            as.list(unlist(lapply(randomVariables, function(rv) {
              current_outcome <- rv$getY
              lossFn <- Evaluation.get_evaluation_function(rv$getFamily, useAsLoss = TRUE)
              risk <- lossFn(data.observed = observed.outcome[[current_outcome]],
                                     data.predicted = algorithm_outcome[[current_outcome]])
              names(risk) <- current_outcome
              risk
            })))
          })
          
          # cv_risk <- list()
          #if (is.a(predicted.outcome, 'list')) {
            ## MOVE TO SEPARTE FUNCTION
            #for (rv in randomVariables) {
              #current.outcome <- 
              #lossFn <- Evaluation.get_evaluation_function(rv$getFamily, useAsLoss = useAsLoss)
              #cv_risk[[rv$getY]] <- lossFn(data.observed = observed.outcome[[rv$getY]],
                                            #data.predicted = predicted.outcome[[rv$getY]]) 
            #}
          #} else {
            #algorithms <- rownames(predicted.outcome)
            #for (rv in randomVariables) {
              #current.outcome <- predicted.outcome[[rv$getY]]
              #names(current.outcome) <- algorithms
              #lossFn <- Evaluation.get_evaluation_function(rv$getFamily, useAsLoss = useAsLoss)
              #for (algorithm in algorithms) {
                #cv_risk[[rv$getY]][[algorithm]] <- lossFn(data.observed = observed.outcome[[rv$getY]],
                                              #data.predicted = current.outcome[[algorithm]]) 
              #}
            #}
          #}
          return(cv_risk)
        },

        # Outcome is a datatable
        update_risk = function(predicted.outcome, observed.outcome, randomVariables,
                               current_count, current_risk) {

          if(is.null(predicted.outcome) | length(predicted.outcome) == 0) throw('Predicted outcome is empty!')
          if(is.null(observed.outcome) | all(dim(observed.outcome) == c(0,0))) throw('Observed outcome is empty!')

          updated_risk <- self$calculate_risk(predicted.outcome = predicted.outcome,
                                              observed.outcome = observed.outcome, 
                                              randomVariables = randomVariables)


          # Note that we need to update the risk for every algorithm and every RV
          # TODO: Is this calulation correct now?
          current_risk <- lapply(updated_risk, function(algorithm_risks) {
            as.list(unlist(lapply(randomVariables, function(rv) {
              current <- rv$getY 

              # The score up to now needs to be calculated current_count times, the other score 1 time.
              new_risk <- (1 / (current_count + 1)) * algorithm_risks[[current]] 
              if(!is.null(current_risk[[current]])){
                new_risk <- new_risk +(current_count / (current_count + 1)) * algorithm_risks[[current]] 
              }
              names(new_risk) <- current
              new_risk
            })))
          })
          
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
