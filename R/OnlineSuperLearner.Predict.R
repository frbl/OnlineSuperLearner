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
                         sample = FALSE, plot = FALSE, denormalize = TRUE) {
        if (!osl$is_fitted){
          return(NA)
        }

        all_estimators <- Arguments$getLogical(all_estimators)
        discrete <- Arguments$getLogical(discrete) & osl$fits_dosl
        continuous <- Arguments$getLogical(continuous) & osl$fits_osl

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
                                                           plot = plot, denormalize = FALSE)

          if (all_estimators) {
            private$verbose && cat(private$verbose, 'All Estimators')
            if (denormalize) {
              processed_predictions <- lapply(copy(predictions), private$denormalize)
              result <- append(result, processed_predictions)
            } else {
              result <- append(result, predictions)
            }
          }

          if(continuous) {
            result$osl.estimator <- self$predict_osl(osl$get_osl_weights, predictions, randomVariables, denormalize = denormalize)
          }
        }

        if (discrete) {
          result$dosl.estimator <- self$predict_dosl(osl$get_dosl, data, randomVariables,
                                                     sample = sample, plot = plot, denormalize = denormalize)
        }
        private$verbose && exit(private$verbose)
          result

      },

      predict_osl = function(osl_weights, predictions, randomVariables, denormalize = TRUE) {
        private$verbose && cat(private$verbose, 'continuous SL')

        # TODO: What if A ends up not being binary?
        # TODO: More important, what if a variable is discrete?
        result <- lapply(randomVariables, function(rv) {
          current_rv_name <- rv$getY
          result <- do.call(cbind, predictions) %>%
            subset(., select = grep(paste(current_rv_name,"$",sep=""), names(.))) %>%
            as.matrix(.) %*% osl_weights[,current_rv_name]

          colnames(result) <- current_rv_name
          result
        }) %>%
          do.call(cbind, .) %>%
          as.data.table 
        
        if(denormalize) result <- private$denormalize(result)
        return(result)
      },

      predict_dosl = function(dosl, data, randomVariables, sample = FALSE, plot = FALSE, denormalize = TRUE) {
        private$verbose && cat(private$verbose, 'discrete SL')
        result <- lapply(randomVariables, function(rv) {
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

        if(denormalize) result <- private$denormalize(result)
        return(result)
      },


      # Predict using all estimators separately.
      # Params:
      # @param data_current: the initial dataset to train the estimators on
      # @param Y: the column names used for the outcome
      # @param A: the column names used for the treatment
      # @param W: the column names used for the covariates
      # @return a list of outcomes, each entry being a data.table with the outcomes of an estimator
      predict_using_all_estimators = function(data, sl_library, sample = FALSE, plot = FALSE, denormalize = TRUE) {
        private$verbose && enter(private$verbose, 'Predicting with all estimators')
        #dataH2o <- as.h2o(data)
        #private$verbose && cat(private$verbose, 'Uploaded data to h2o')

        result <- lapply(sl_library,
          function(estimator) {
            #if(is.a(estimator, 'ML.H2O')){
              #current <- dataH2o
            #} else {
              current <- data
            #}
            # TODO: Unity in export formats. Probably the best is to enforce a data.table output
            estimator$predict(current, sample = sample, plot = plot)
          })

        # convert the list of results into a data.table
        result <- lapply(result, function(res) as.data.table(do.call(cbind, res)))
        private$verbose && exit(private$verbose)

        if(denormalize) result <- lapply(result, private$denormalize)
        return(result)
      }
    ),
  active =
    list(
    ),
  private =
    list(
      verbose = NULL,
      pre_processor = NULL,

      # denormalize the data
      denormalize = function(data) {
        if (is.null(private$pre_processor)) {
          return(data)
        }
        return(private$pre_processor$denormalize(data))
      }

    )
)
