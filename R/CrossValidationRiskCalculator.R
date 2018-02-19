#' CrossValidationRiskCalculator
#'
#' Class that contains various methods for calculating the crossvalidated risk
#' of an estimator.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include Evaluation.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize() }}{
#'     Creates a new cross validated risk calculator.
#'   }
#'
#'   \item{\code{calculate_evaluation(predicted.outcome, observed.outcome, randomVariables, add_evaluation_measure_name=TRUE)}}{
#'     Calculates an evaluation using the provided predicted and observed
#'     outcomes. It uses a list of \code{RandomVariable} objects to loop
#'     through all data provided to it. If the \code{predicted.outcome} list is
#'     provided with both a normalized and denormalized entry, it will use the
#'     normalized entry as the default. One can choose to add the evaluation
#'     metric that was used to the names of the output. This is done by setting
#'     \code{add_evaluation_measure_name} to true.
#'
#'     The input data should look looks as followos:
#'       list
#'         normalized 
#'           AlgorithmName1
#'            data.table with a W, A, Y entry.
#'           AlgorithmName2
#'            data.table with a W, A, Y entry.
#'         denormalized
#'           AlgorithmName1
#'            data.table with a W, A, Y entry.
#'           AlgorithmName2
#'            data.table with a W, A, Y entry.
#'
#'     The output data then looks as follows:
#'       list
#'         AlgorithmName1
#'          data.table with a W, A, Y entry.
#'         AlgorithmName2
#'          data.table with a W, A, Y entry.
#'
#'     @param predicted.outcome the outcome predicted by the various algorithms
#'      in the super learner. This is a list which either has two entries
#'      (normalized and denormalized), and in which both those entries have a
#'      list of ML outputs, or it is a list of the outputs of each of the
#'      algorithms (e.g., the normalized output directly).
#'
#'     @param observed.outcome the actual data that was observed in the study.
#'
#'     @param randomVariables the randomvariables that are included in the
#'      prediction
#'
#'     @param add_evaluation_measure_name (default TRUE) should we add the name
#'      of the evaluation metric to the output?
#'
#'     @return a list with the evalutation of each of the algorithms.
#'   }
#'
#'   \item{\code{evaluate_single_outcome(observed.outcome, predicted.outcome, ra ndomVariables}}{
#'     Perform the evaluation of a single estimator. In this case the data of
#'     just one estimator can be provided, such as:
#'       AlgorithmName1 >
#'        data.table with a W, A, Y entry.
#'     the function will then use the default evaluation metric to determine
#'     the performance of the estimator.
#'
#'     @param predicted.outcome the outcome predicted by a single algorithms in
#'      the super learner. 
#'
#'     @param observed.outcome the actual data that was observed in the study.
#'
#'     @param randomVariables the randomvariables that are included in the
#'      prediction.
#'
#'     @return a list with the evalutation of the algorithm.
#'   }
#'
#'   \item{\code{calculate_risk(predicted.outcome, observed.outcome, randomVariables}}{
#'     Calculate the CV risk for each of the random variables provided based on
#'     the predicted and observed outcomes. This function also expects a list
#'     of predictions in a similar way as \code{calculate_evaluation} does. 
#'
#'     @param predicted.outcome the outcome predicted by the various algorithms
#'      in the super learner. This is a list which either has two entries
#'      (normalized and denormalized), and in which both those entries have a
#'      list of ML outputs, or it is a list of the outputs of each of the
#'      algorithms (e.g., the normalized output directly).
#'
#'     @param observed.outcome the actual data that was observed in the study
#'      (emperically, or from a simulation).
#'
#'     @param randomVariables the randomvariables that are included in the
#'      prediction
#'
#'     @param add_evaluation_measure_name (default TRUE) should we add the name
#'      of the evaluation metric to the output?
#'
#'     @return a list of lists, in which each element is the risk for one
#'      estimator. In each list per estimator, each element corresponds to one
#'      of the random variables.
#'   }
#'
#'   \item{\code{update_risk(predicted.outcome, observed.outcome, randomVariables, current_count, current_risk) }}{
#'     Function used by the OSL to update a previous risk. This function uses
#'     the equation by Benkeser et al. (2017) to update a previous risk. What
#'     it does is multiply a previous risk (\code{current_risk}) by the
#'     \code{current_count} and add the new risk to this multiplied risk. Then
#'     it divides this risk by \code{current_count + 1} to come to the current
#'     risk estimate. This way we don't have to recalculate the whole risk when
#'     only one update is required.
#'     
#'     @param predicted.outcome the outcome predicted by the various algorithms
#'      in the super learner. This is a list which either has two entries
#'      (normalized and denormalized), and in which both those entries have a
#'      list of ML outputs, or it is a list of the outputs of each of the
#'      algorithms (e.g., the normalized output directly).
#'
#'     @param observed.outcome the actual data that was observed in the study
#'      (emperically, or from a simulation).
#'
#'     @param randomVariables the randomvariables for which the distributions
#'      have been calculated
#'
#'     @param current_count the current number of evaluations performed for
#'      calculating the \code{current_risk}.
#'
#'     @param current_risk the previously calculated risk of each of the
#'      estimators (calculated over \code{current_count} number of
#'      evaluations).
#'
#'     @return a list of lists with the updated risk for each estimator, and
#'      for each estimator an estimate of the risk for each random variable.
#'   }
#'
#'   \item{\code{update_single_risk(old_risk, new_risks, current_count, randomVariables) }}{
#'     Instaed of updating the risk for each of estimators, one can also update
#'     a single risk. In this case the risks are updated using the
#'     \code{old_risk} and \code{new_risks} variable. Essentially, this
#'     function performs the internals of the \code{update_risk} function,
#'     however, here it expects risks to be calculated beforehand instead of
#'     mere predictions and observed outcomes. This function uses
#'     the equation by Benkeser et al. (2017) to update a previous risk. What
#'     it does is multiply a previous risk (\code{current_risk}) by the
#'     \code{current_count} and add the new risk to this multiplied risk. Then
#'     it divides this risk by \code{current_count + 1} to come to the current
#'     risk estimate. This way we don't have to recalculate the whole risk when
#'     only one update is required.
#'
#'     @param old_risk the old risks, calculated in a previous iteration.
#'     @param new_risks the new risks, calculated using the current machine learning estimators.
#'     @param current_count the number of iterations used to calculate the old risk.
#'     @param randomVariables the random variables for which the predictions have been created.
#'     @return the updated risk as a data.table. 
#'     @examples
#'     \dontrun{ 
#'       update_single_risk 
#'     }
#'   }
#' }
#' @export
CrossValidationRiskCalculator <- R6Class("CrossValidationRiskCalculator",
  public =
    list(
      initialize = function() {
      },

      ## Output is a list of data.tables
      calculate_evaluation = function(predicted.outcome, observed.outcome, randomVariables, add_evaluation_measure_name=TRUE) {
        # predicted.outcome <- Arguments$getInstanceOf(predicted.outcome, 'list')

        ## If there is a normalized field, prefer the normalized outcomes
        if ('normalized' %in% names(predicted.outcome)) predicted.outcome = predicted.outcome$normalized 

        ## Calculate the evaluations
        lapply(predicted.outcome, function(one.outcome) {
          self$evaluate_single_outcome( 
            observed.outcome = observed.outcome,
            predicted.outcome = one.outcome,
            randomVariables = randomVariables,
            add_evaluation_measure_name = add_evaluation_measure_name
          )
        })
      },

      ## Evaluate should receive the outcome of 1 estimator
      evaluate_single_outcome = function(observed.outcome, predicted.outcome, randomVariables, add_evaluation_measure_name) {
        sapply(randomVariables, function(rv) {
          current_outcome <- rv$getY
          lossFn <- Evaluation.get_evaluation_function(family = rv$getFamily, useAsLoss = FALSE)
          result <- lossFn(data.observed = observed.outcome[[current_outcome]],
                            data.predicted = predicted.outcome[[current_outcome]])

          ## Add the name of the evaluation used
          if (add_evaluation_measure_name){
            names(result) <- paste(names(result), current_outcome, sep='.')
          } else {
            names(result) <- current_outcome
          }
          result
        }) %>% t %>% as.data.table
      },

      ## Calculate the CV risk for each of the random variables provided
      ## Output is a list of data.tables
      calculate_risk = function(predicted.outcome, observed.outcome, randomVariables, check=FALSE){
        if ('normalized' %in% names(predicted.outcome)) predicted.outcome = predicted.outcome$normalized
          
        if (check) {
          predicted.outcome <- Arguments$getInstanceOf(predicted.outcome, 'list')
          observed.outcome <- Arguments$getInstanceOf(observed.outcome, 'data.table')
        }

        cv_risk <- lapply(predicted.outcome, function(algorithm_outcome) {
          self$risk_single_outcome(
            observed.outcome = observed.outcome,
            predicted.outcome = algorithm_outcome,
            randomVariables = randomVariables
          )
        })

        return(cv_risk)
      },

      risk_single_outcome = function(observed.outcome, predicted.outcome, randomVariables) {
        result <- lapply(randomVariables, function(rv) {
          current_outcome <- rv$getY
          lossFn <- Evaluation.get_evaluation_function(family = rv$getFamily, useAsLoss = TRUE)
          risk <- lossFn(data.observed = observed.outcome[[current_outcome]],
                          data.predicted = predicted.outcome[[current_outcome]])
          current_outcome
          names(risk) <- current_outcome
          risk
        }) 

        ## Unname the results, just so we can be sure the lapply did not add
        ## any names. If it does add names, the datatable function will
        ## concatenate the names.
        result %<>% unname

        ## The unlist t is a hack to flatten the result, but to keep the correct names
        ## that the entries got in the loop.
        as.data.table(t(unlist(result)))
      },

      ## Outcome is a list of datatables
      update_risk = function(predicted.outcome, observed.outcome, randomVariables, current_count, current_risk, check = FALSE) {
        ## Note that we don't need to check whether data is normalized, as we
        ## are passing it to the calculate_risk function, which will pick the
        ## correct one.
        if(check) {
          current_count <- Arguments$getInteger(current_count, c(0, Inf))
          predicted.outcome <- Arguments$getInstanceOf(predicted.outcome, 'list')
          observed.outcome <- Arguments$getInstanceOf(observed.outcome, 'data.table')
          if(is.null(predicted.outcome) || length(predicted.outcome) == 0) throw('Predicted outcome is empty!')
          if(is.null(observed.outcome) || all(dim(observed.outcome) == c(0,0))) throw('Observed outcome is empty!')
        }

        ## Calculate the risk for the current observations / predictions
        updated_risk <- self$calculate_risk(
          predicted.outcome = predicted.outcome,
          observed.outcome = observed.outcome,
          randomVariables = randomVariables
        )

        ## Note that we need to update the risk for every algorithm and every RV
        algorithm_names <- names(updated_risk)

        current_risk <- lapply(algorithm_names, function(algorithm_name) {
          new_risks <- updated_risk[[algorithm_name]]
          old_risk <- current_risk[[algorithm_name]]

          self$update_single_risk(
            old_risk = old_risk, 
            new_risks = new_risks, 
            current_count = current_count, 
            randomVariables = randomVariables
          )
        })

        names(current_risk) <- algorithm_names
        return(current_risk)
      },

      update_single_risk = function(old_risk, new_risks, current_count, randomVariables) {
        result <- lapply(randomVariables, function(rv) {
          current <- rv$getY

          ## The score up to now needs to be calculated current_count times, the other score 1 time.
          ## new_risk <- (1 / (current_count + 1)) * new_risks[[current]]
          new_risk <- new_risks[[current]]

          if(!is.null(old_risk) && is.na(old_risk[[current]])) {
            ## If our previous risk is NA, that means that we had a previous iteration in which we could
            ## not calculate the risk. Most likely because of the initialization of the DOSL. In the next
            ## iteration we can make a prediction, but it is out of sync with the provided count. Therefore,
            ## set the old risk to the new risk and resync with the count.
            old_risk[[current]] <- new_risk
          }

          if(!is.null(old_risk)) {
            new_risk <- (new_risk + (current_count * old_risk[[current]])) / (current_count + 1)
          }
          names(new_risk) <- current
          new_risk
        })
        
        ## Unname the results, just so we can be sure the lapply did not add
        ## any names. If it does add names, the datatable function will
        ## concatenate the names.
        result %<>% unname

        result %>% unname %>% unlist %>% t %>% as.data.table
      }
    ),
  active =
    list(
        ),
  private =
    list(
    )
)
