#' OnlineSuperLearner.Predict
#'
#' @docType class
#' @importFrom R6 R6Class
OnlineSuperLearner.Predict <- R6Class("OnlineSuperLearner.Predict",
  public =
    list(
      initialize = function(pre_processor = NULL, verbose = FALSE) {
          private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)

          if (!is.null(pre_processor)) {
            private$verbose && cat(private$verbose,'Adding a preprocessor for changing the data.')
            private$pre_processor <- Arguments$getInstanceOf(pre_processor, 'PreProcessor')
          }
      },

      predict = function(osl, data, randomVariables, all_estimators = TRUE, discrete = TRUE, continuous = TRUE, 
                         sample = FALSE, plot = FALSE) {
        if (!osl$is_fitted){
          return(NA)
        }

        all_estimators <- Arguments$getLogical(all_estimators)
        discrete <- Arguments$getLogical(discrete) && osl$fits_dosl
        continuous <- Arguments$getLogical(continuous) && osl$fits_osl

        if (!any(c(discrete, all_estimators, continuous))) {
          throw('At least one option should be selected: discrete, all_estimators, or continuous')
        }

        private$verbose && enter(private$verbose, 'Predicting for',
                                ifelse(all_estimators, ', all estimators',''),
                                ifelse(discrete, ', discrete superlearner',''),
                                ifelse(continuous, ', continuous superlearner','')
                                )
        result <- list()

        if (all_estimators || continuous) {
          predictions <- self$predict_using_all_estimators(data = data, sl_library = osl$get_estimators,
                                                           sample = sample, 
                                                           plot = plot)

          if (all_estimators) {
            private$verbose && cat(private$verbose, 'All Estimators')
            result <- predictions
          }

          if(continuous) {
            predictions <- self$predict_osl(osl$get_osl_weights, predictions, randomVariables)

            result$normalized$osl.estimator <-  predictions$normalized
            result$denormalized$osl.estimator <- predictions$denormalized
          }
        }

        if (discrete) {
          predictions <- self$predict_dosl(osl$get_dosl, data, randomVariables,
                                                     sample = sample, plot = plot)

          result$normalized$dosl.estimator <-  predictions$normalized
          result$denormalized$dosl.estimator <- predictions$denormalized

        }
        private$verbose && exit(private$verbose)

        # Result is a list of lists. first list has  two entries: Normalized and denormalized. Each of these lists hase
        # the outcomes for each of the estimators. In each of those entries a data.table is contained.
        result
      },

      predict_osl = function(osl_weights, predictions, randomVariables) {
        private$verbose && cat(private$verbose, 'continuous SL')
        if ('normalized' %in% names(predictions)) {
          predictions <- predictions$normalized 
        }

        # TODO: What if A ends up not being binary?
        # TODO: More important, what if a variable is discrete?
        normalized_result <- lapply(randomVariables, function(rv) {
          current_rv_name <- rv$getY
          result <- do.call(cbind, predictions) %>%
            subset(., select = grep(paste(current_rv_name,"$",sep=""), names(.))) %>%
            as.matrix(.) %*% osl_weights[,current_rv_name]

          colnames(result) <- current_rv_name
          result
        }) %>%
          do.call(cbind, .) %>%
          as.data.table 
        
        denormalized_result <- private$denormalize(copy(normalized_result))

        list(normalized = normalized_result,
             denormalized = denormalized_result)
      },

      predict_dosl = function(dosl, data, randomVariables, sample = FALSE, plot = FALSE) {
        private$verbose && cat(private$verbose, 'discrete SL')
        normalized_result <- lapply(randomVariables, function(rv) {
            outcome_name <- rv$getY

            # This is for the first iteration, we don't have a dosl yet as it get's selected based
            # on the other estimator's CV score
            if (outcome_name %in% names(dosl)) {
              prediction <- dosl[[outcome_name]]$predict(data = data,
                                                         sample = sample,
                                                         subset = outcome_name,
                                                         plot = plot)[[outcome_name]]
            } else {
              prediction <- c(outcome = NA)
            }
            prediction %<>% as.matrix(prediction)
            colnames(prediction) <- outcome_name
            prediction
          }) %>%
          do.call(cbind, .) %>%
          as.data.table

        denormalized_result <- private$denormalize(copy(normalized_result))

        list(normalized = normalized_result,
             denormalized = denormalized_result)

      },

      # Predict using all estimators separately.
      # Params:
      # @param data_current: the initial dataset to train the estimators on
      # @param Y: the column names used for the outcome
      # @param A: the column names used for the treatment
      # @param W: the column names used for the covariates
      # @return a list of outcomes, each entry being a data.table with the outcomes of an estimator
      predict_using_all_estimators = function(data, sl_library, sample = FALSE, plot = FALSE) {
        private$verbose && enter(private$verbose, 'Predicting with all estimators')

        normalized_results <- list()
        denormalized_results <- list()
        for (estimator in sl_library) {
          private$verbose && cat(private$verbose, 'Predicting with ', estimator$get_name)

          result <- private$predict_with_one_estimator(estimator = estimator,
                                             data = data,
                                             sample = sample,
                                             plot = plot)
          normalized_results <- append(normalized_results, list(result$normalized))
          denormalized_results <- append(denormalized_results, list(result$denormalized))
        }
        names(normalized_results) <- names(sl_library)
        names(denormalized_results) <- names(sl_library)

        private$verbose && exit(private$verbose)

        list(normalized = normalized_results,
             denormalized = denormalized_results)
      }
    ),
  active =
    list(
    ),
  private =
    list(
      verbose = NULL,

      ## The data processor to convert the results back to their original format
      pre_processor = NULL,

      predict_with_one_estimator = function(estimator, data, sample, plot) {
        # TODO: Unity in export formats. Probably the best is to enforce a data.table output
        normalized_result <- estimator$predict(data, 
                          sample = sample, 
                          plot = plot) %>%
          as.data.table
        denormalized_result <- private$denormalize(copy(normalized_result))

        list(normalized = normalized_result,
             denormalized = denormalized_result)
      },

      # denormalize the data
      denormalize = function(data) {
        if (is.null(private$pre_processor)) {
          return(data)
        }
        return(private$pre_processor$denormalize(data))
      }

    )
)
