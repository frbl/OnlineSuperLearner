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

        if (!osl$is_fitted) return(NA)

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
        result <- list(denormalized = list(), normalized = list())

        if (all_estimators) {
          private$verbose && cat(private$verbose, 'All Estimators')
          result <- self$predict_using_all_estimators(
            data = data, 
            sl_library = osl$get_estimators,
            sample = sample, 
            plot = plot
          )
        }

        if(continuous) {
          result <- self$predict_osl(
            data = data,
            osl_weights = osl$get_osl_weights,
            sl_library = osl$get_estimators,
            current_result = result,
            sample = sample, 
            plot = plot,
            randomVariables = randomVariables
          )
        }

        if (discrete) {
          result <- self$predict_dosl(
            dosl = osl$get_dosl,
            data = data, 
            randomVariables = randomVariables,
            sample = sample, 
            plot = plot,
            current_result = result
          )
        }
        private$verbose && exit(private$verbose)

        # Result is a list of lists. first list has  two entries: Normalized and denormalized. Each of these lists hase
        # the outcomes for each of the estimators. In each of those entries a data.table is contained.
        result
      },

      predict_osl = function(data, osl_weights, current_result, sl_library, randomVariables, weight_threshold = 0.00, sample = FALSE, plot = FALSE) {
        private$verbose && cat(private$verbose, 'continuous SL')
        # TODO: What if A ends up not being binary?
        # TODO: More important, what if a variable is discrete?

        ## Using a for loop so we can add data to the predictions
        result <- list()
        for (rv in randomVariables) {
          current_rv_name <- rv$getY
          filtered_weights <- subset(osl_weights, osl_weights[,current_rv_name] >= weight_threshold, select = current_rv_name)
          
          algorithm_names <- rownames(filtered_weights)

          ## Create new predictions that are not yet in the data set, but are needed
          current_result <- private$predict_missing_predictions(
            sl_library = sl_library,
            data = data,
            sample = sample, 
            plot = plot,
            algorithm_names = algorithm_names,
            predictions = current_result
          )
          current_result

          outcomes <- lapply(current_result$normalized[algorithm_names], function(x) x[[current_rv_name]])

          ## Convert the outcomes from a list to  a matrix
          outcomes <- matrix(unlist(outcomes, use.names = FALSE), ncol = length(outcomes), byrow = FALSE)

          ## Store the names
          colnames(outcomes) <- algorithm_names
          
          result <- append(result, list(outcomes %*% filtered_weights))
        }
        normalized_result <- as.data.table(do.call(cbind, result))
        denormalized_result <- private$denormalize(copy(normalized_result))

        current_result[['normalized']]$osl.estimator <-  normalized_result
        current_result[['denormalized']]$osl.estimator <- denormalized_result
        current_result
      },

      predict_dosl = function(dosl, data, randomVariables, current_result, sample = FALSE, plot = FALSE) {
        private$verbose && cat(private$verbose, 'discrete SL')

        result <- list()
        for (rv in randomVariables) {
          outcome_name <- rv$getY

          prediction <- c(outcome = NA)
          # This is for the first iteration, we don't have a dosl yet as it get's selected based
          # on the other estimator's CV score
          if (outcome_name %in% names(dosl)) {
            dosl_for_current_rv <- dosl[[outcome_name]]
            current_algo_name <- dosl_for_current_rv$get_name
            ## If we already had the outcome, don't predict it again, use the previous prediction (it is the same)
            if(!is.null(current_result) && current_algo_name %in% names(current_result$normalized)) {
              prediction <- current_result$normalized[[current_algo_name]]
            } else {
              prediction <- dosl_for_current_rv$predict(
                data = data,
                sample = sample,
                subset = outcome_name,
                plot = plot
              ) 
            }

            ## Only store the present outcome
            prediction <- prediction[[outcome_name]]
          }
          prediction %<>% as.matrix(prediction)
          colnames(prediction) <- outcome_name
        
          result <- append(result, list(prediction))
        }
        normalized_result <- as.data.table(do.call(cbind, result))
        denormalized_result <- private$denormalize(copy(normalized_result))

        current_result[['normalized']]$dosl.estimator <-  normalized_result
        current_result[['denormalized']]$dosl.estimator <- denormalized_result
        current_result
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

          result <- private$predict_with_one_estimator(
            estimator = estimator,
            data = data,
            sample = sample,
            plot = plot
          )
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

      predict_missing_predictions = function(sl_library, data, sample, plot, algorithm_names, predictions) {
        for (name in algorithm_names) {
          ## If the data has not yet been predicted, predict it here
          if(!(name %in% names(predictions$normalized))) {
            current_prediction <- private$predict_with_one_estimator(
              estimator = sl_library[[name]],
              data = data,
              sample = sample,
              plot = plot
            )
            predictions$normalized[[name]] <- current_prediction$normalized
            predictions$denormalized[[name]] <- current_prediction$denormalized
          }
        }
        predictions
      },

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
