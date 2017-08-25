#devtools::load_all('~/Workspace/osofr/condensier')

#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import future
#' @include zzz.R
#' @include LibraryFactory.R
#' @include DataSplitter.R
#' @include DensityEstimation.R
#' @include SummaryMeasureGenerator.R
#' @include WeightedCombinationComputer.R
#' @include WCC.NMBFGS.R
#' @include WCC.SGD.Simplex.R
#' @include CrossValidationRiskCalculator.R
#' @include InterventionParser.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(SL.library.definition = c("ML.Local.lm", "ML.H2O.glm", summaryMeasureGenerator, verbose = FALSE)}}{
#'     starts a new OnlineSuperLearner. The provided \code{SL.library.definition} contains the machine learning models to use
#'     @param SL.library.definition = a list of machine learning algorithms. This could be either a vector with with the
#'     name of each estimator or a list according to the libraryFactroy.  Look in the LibraryFactory class for the
#'     specification of this list.
#'     @param summaryMeasureGenerator = an object of the type SummaryMeasureGenerator. This generator is used to get new
#'     observations with the correct aggregated columns.
#'     @param verbose = the verbosity (how much logging). Note that this might be propagated to other classes.
#'   }
#'
#'   \item{\code{evaluateModels(data, randomVariables) }}{
#'     Performs a basic evaluation on the data, given a list of random variables
#'     @param data = the data to use for performing the evaluation
#'     @param randomVariables = the randomVariables for which one wants to see the evaluation. Note that this needs to
#'     be equal to, or a subset of, the random variables used to train the estimators.
#'   }
#'
#'   \item{\code{sample_iteratively(data, randomVariables, tau = 10, intervention = NULL)}}{
#'     Method to sample iteratively from the densities. It works by providing an initial observation (\code{data}), from which
#'     iteretitatively the next measurement is estimated. This is done until \code{tau} steps in the future. Furthermore,
#'     this sampling step can be augmented with an intervention. That is, we could set a given time step (or all)
#'     to a certain value. The \code{intervention} provided should be a list containing a \code{when} and \code{what} entry.
#'     the \code{when} entry should show when the intervention is performed, the \code{what} entry shows what should be done.
#'     @param data = the initial data to start the sampling from. At most 1 row of data.
#'     @param randomVariables = the ordered set of randomvariables used when fitting the data
#'     @param tau = the timestep at which you want to evaluate the output
#'     @param intervention = the intervention, e.g.: \code{list(when = c(1,2), what = c(1,0))}
#'   }
#'
#'   \item{\code{fit(data, randomVariables, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20)}}{
#'     The actual method to fit the OnlineSuperLearner. This will fit the provided \code{SL.library.definition}
#'     estimators as well as the OnlineSuperLearner and the DiscreteOnlineSuperLearner.
#'     @param data = the data to fit the estimator on. Should be a \code{Data.Base} subclass.
#'     @param randomVariables = the random variables to fit the densities / estmators for (W,A,Y)
#'     @param initial_data_size = the size of the dataset to use for the initial fit (pre-update)
#'     @param max_iterations = the number of iterations to run for updating the data
#'     @param mini_batch_size = the size of the mini batch to use for each update.
#'   }
#'
#'   \item{\code{predict(data, randomVariables, all_estimators = TRUE, discrete = TRUE, continuous = TRUE)}}{
#'     Method to perform a prediction on the estimators. It can run in different configurations. It can be configured
#'     to predict the outcome using all estimators (the \code{all_estimators} flag), using the discrete superlearner
#'     (the \code{discrete} flag), or using the continuous online superlearner (the \code{continous} flag). At least
#'     one of these three flags must be true.
#'     @param data = the data to use for doing the predictions
#'     @param randomVariables = the random variables used for doing the predictions (these should be the same as the
#'     ones used for fitting).
#'     @param all_estimators = whether or not to include the output of all candidate estimators in the output
#'     @param discrete = whether or not to include the output of the discrete super learner in the output
#'     @param continuous = whether or not to include the output of the continuous super learner in the output
#'   }
#'
#'   \item{\code{is_fitted}}{
#'     Active method to return whether the OSL has been fitted or not
#'     @return boolean true if it has been fitted, false if not
#'   }
#'
#'   \item{\code{is_online}}{
#'     Active method to deterimine whether the actual algorithm is fitted in an online way. That is to say, that all of
#'     the estimators are in fact online.
#'     @return boolean true if it all algorithms are online, false if not
#'   }
#'
#'   \item{\code{fits_osl}}{
#'     Active method to know whether the current OSL fits an online super learner (that is, the weighted combination).
#'     This setting comes from the initialization step of OSL.
#'     @return boolean true if it fits an osl (false if not)
#'   }
#'
#'   \item{\code{fits_dosl}}{
#'     Active method to know whether the current OSL fits a discrete online super learner.  This setting comes from the
#'     initialization step of OSL.
#'     @return boolean true if it fits a discrete osl (false if not)
#'   }
#'
#'   \item{\code{info}}{
#'     Active method to print some general info related to the current OSL
#'   }
#'
#'   \item{\code{get_estimators}}{
#'     Active method to retrieve a list of estimators. These can be the fitted versions (if the osl is fitted), or the
#'     plain unfitted versions. Check the is_online version for that.
#'     @return list a list object containing all estimators.
#'   }
#'
#'   \item{\code{get_osl_weights}}{
#'     Active method to retrieve a vector of weights that the OSL has found for its continuous online super learner fit.
#'     @return vector a vector containing the estimates of the OSL weights
#'   }
#'
#'   \item{\code{get_dosl}}{
#'     Active method to retrieve the actual DOSL fit. this could be nil if no dosl has been fit yet.
#'     @return list a list containing the best estimator for each of the random variables.
#'   }
#'
#'   \item{\code{get_cv_risk}}{
#'     Active method to retrieve the crossvalidated risk of each of the estimators
#'     @return list a list containing the risk estimates for each of hte estimators.
#'   }
#'
#'   \item{\code{get_valididy}}{
#'     Active method that throws an error if the current state of the OSL is not valid (i.e., that it has invalid
#'     parameters in it).
#'   }
#' }
#' @export
OnlineSuperLearner <- R6Class ("OnlineSuperLearner",
  private =
    list(
        ## Variables
        ## =========
        ## The R.cv score of the current fit
        ## default_wcc = WCC.SGD.Simplex,
        default_wcc = WCC.NMBFGS,
        cv_risk = NULL,
        historical_cv_risk = NULL,
        cv_risk_count = NULL,
        cv_risk_calculator = NULL,

        ## The online discrete super learners. One for each outcome variable.
        dosl.estimators = NULL,

        ## ML Library
        SL.library.names = NULL,
        SL.library.descriptions = NULL,
        SL.library.fabricated = NULL,
        fitted = NULL,

        ## Options for fitting
        should_fit_osl = NULL,
        should_fit_dosl = NULL,

        ## Splitter for the data
        data_splitter = NULL,

        ## A cache for the data (used whenever the algorithm is not online)
        data_cache = NULL,
        all_estimators_online = NULL,

        ## Summary measures and a generator
        summaryMeasureGenerator = NULL,

        ## Verbosity of the logs
        verbose = FALSE,

        ## The computer for the SuperLearner combination
        weightedCombinationComputers = NULL,

        ## The class to make predictions on the data
        online_super_learner_predict = NULL,

        ## Functions
        ## =========

        ## Update the cross validation risk
        update_risk = function(predicted.outcome, observed.outcome, randomVariables, update_counter = TRUE) {
          private$cv_risk <- private$cv_risk_calculator$update_risk(predicted.outcome = predicted.outcome,
                                                                    observed.outcome = observed.outcome,
                                                                    randomVariables = randomVariables,
                                                                    current_count = private$cv_risk_count,
                                                                    current_risk = self$get_cv_risk)
          if (update_counter) {
            private$cv_risk_count <- private$cv_risk_count + 1
          }
          private$cv_risk_count
        },

        update_historical_cv_risk = function() {
          private$historical_cv_risk <- append(self$get_historical_cv_risk, list(private$cv_risk))
        },

        ## Initializes the weighted combination calculators. One for each randomvariable.
        initialize_weighted_combination_calculators = function(randomVariables) {
          lapply(randomVariables, function(rv) {
            weights.initial <- rep(1 / length(private$SL.library.descriptions), length(private$SL.library.descriptions))

            ## TODO: DIP the WCC
            private$weightedCombinationComputers[[rv$getY]] <- private$default_wcc$new(weights.initial = weights.initial)
          })
        },

        ## Updates the data cache with the new observations. This function only
        ## updates the cache if not all estimators are online. It tells the user
        ## if it has updated by the boolean it returns.
        ## @param newdata the new data to add to the cache
        ## @return boolean whether or not it actually needed to update the cache.
        update_cache = function(newdata) {
          if (self$is_online) {
            ## If we are truly online, just cache the last entry
            private$data_cache <- newdata
            return(FALSE)
          }
          private$data_cache <- rbindlist(list(private$data, newdata))
          return(TRUE)
        },

        ## Train using all estimators separately.
        ## Postcondition: each of our density estimators will have a fitted conditional
        ## density in them for each of our random vars WAY *AND IT SHOULD DO THIS FOR ALL
        ## $w \in W$*
        ## Params:
        ## @param data_current: the initial dataset to train the estimators on
        ## @param Y: the column names used for the outcome
        ## @param A: the column names used for the treatment
        ## @param W: the column names used for the covariates
        ## @return a vector of outcomes, each entry being the predicted outcome of an estimator on the test set
        train_all_estimators = function(data, randomVariables) {
          private$verbose && enter(private$verbose, 'Training all estimators')
          
          # If not all estimators are online, we have to keep track of a cache of data.
          private$update_cache(newdata = data)

          #private$SL.library.fabricated <- mclapply(private$SL.library.fabricated, function(estimator) {
          for(estimator in private$SL.library.fabricated) {
          #estimators <- foreach(estimator=private$SL.library.fabricated) %dopar% {
            if(self$is_fitted && estimator$is_online) {
              # Note that we use the data here, and not the cache, as
              # essentially this cache will be  empty if none of the algorithms
              # is online, and we only want to use the new observations to
              # update the estimator.
              estimator$update(data)
            } else {
              estimator$fit(private$data_cache, randomVariables = randomVariables)
            }
            estimator
          }
          #names(estimators) <- names(private$SL.library.fabricated)
          #private$SL.library.fabricated <- estimators
          #}, mc.cores = 23)
          private$verbose && exit(private$verbose)
        },

        ## Functions
        ## Function to train the initial set of models
        ## Params:
        ## @param data_current: the initial dataset to train the estimators on
        ## @param Y: the column names used for the outcome
        ## @param A: the column names used for the treatment
        ## @param W: the column names used for the covariates
        train_library = function(data_current, randomVariables) {
          ## Fit or update the  estimators
          data.splitted <- private$data_splitter$split(data_current)
          outcome.variables <- sapply(randomVariables, function(rv) rv$getY)

          private$train_all_estimators(data = data.splitted$train, randomVariables = randomVariables)

          ## Extract the level 1 data and use it to fit the osl
          predicted.outcome <- private$online_super_learner_predict$predict_using_all_estimators(
            data = data.splitted$train,
            sl_library = private$SL.library.fabricated
          )

          observed.outcome <- data.splitted$train[,outcome.variables, with=FALSE]
          private$fit_osl(predicted.outcome = predicted.outcome,
                          observed.outcome = observed.outcome)
          private$fitted <- TRUE

          ## Make a prediction using the learners on the test data
          observed.outcome <- data.splitted$test[,outcome.variables, with=FALSE]
          predicted.outcome <- self$predict(data = data.splitted$test,
                                          randomVariables = randomVariables,
                                          discrete = TRUE, continuous = TRUE, all_estimators = TRUE)

          ## We need to store the dosl risk, as we will update it later.
          pre_dosl_risk <- private$cv_risk$dosl.estimator

          ## Calculate the error using the normalized predictions
          private$update_risk(predicted.outcome = predicted.outcome,
                              observed.outcome = observed.outcome,
                              randomVariables = randomVariables)

          ## Update the discrete superlearner (take the first if there are multiple candidates)
          if (self$fits_dosl) {
            private$fit_dosl()

            ## Put the CV risk back to what it was before the update. We can now actually fit it correctly.
            private$cv_risk$dosl.estimator <- pre_dosl_risk

            ## In order to get the initial estimate of the CV error of the DOSL, we first need to fit the other 
            ## estimators, and after that calculate the dosl error separately. 
            predicted.outcome <- self$predict(data = data.splitted$test,
                                            randomVariables = randomVariables,
                                            discrete = TRUE, continuous = FALSE, all_estimators = FALSE)

            private$cv_risk$dosl.estimator <- 
              private$cv_risk_calculator$update_risk(predicted.outcome = predicted.outcome,
                                                                      observed.outcome = observed.outcome,
                                                                      randomVariables = randomVariables,
                                                                      current_count = private$cv_risk_count-1,
                                                                      current_risk = self$get_cv_risk)$dosl.estimator
              #private$cv_risk_calculator$calculate_risk(predicted.outcome = predicted.outcome,
                                                        #observed.outcome = observed.outcome,
                                                        #randomVariables = randomVariables)$dosl.estimator
          }

          private$update_historical_cv_risk()

        },

        ## Function to update the models with the available data
        ## Params:
        ## @param Y: the column names used for the outcome
        ## @param A: the column names used for the treatment
        ## @param W: the column names used for the covariates
        ## @param max_iterations: the number of iterations we can maximaly run for training
        ## @param mini_batch_size: size of the batch we use
        update_library = function(randomVariables, max_iterations, mini_batch_size){
          private$verbose && enter(private$verbose, 'Starting estimator updating')
          if(!self$is_fitted){
            throw('Fit the inital D-OSL and OSL first')
          }

          ## Set the current timestep to 1
          t <- 0
          data_current <- private$summaryMeasureGenerator$getNext(mini_batch_size)

          ## TODO: Check wether the stopping criteria are met (e.g., improvement < theta)
          while(t < max_iterations && nrow(data_current) >= 1 && !is.null(data_current)) {

            ## Only show this log every 5 times
            if(t %% 5 == 0 && private$verbose) {
              lapply(names(self$get_cv_risk), function(cv_name) {
                cat(private$verbose, paste('Updating OSL at iteration', t,
                                          'error for', cv_name,
                                          'is', self$get_cv_risk[cv_name]))
              })
            }

            private$train_library(data_current = data_current, randomVariables = randomVariables)
            output = paste('performance_iteration',t,sep='_')
            OutputPlotGenerator.create_risk_plot(self$get_cv_risk, output, '/tmp/osl/')
 

            ## Get the new row of data
            data_current <- private$summaryMeasureGenerator$getNext(mini_batch_size)
            t <- t + 1
          }
          private$verbose && exit(private$verbose)
        },

        fit_osl = function(predicted.outcome, observed.outcome){
          if(!self$fits_osl) return(NULL)
          ## If we have 1 estimator, the weighted combination of that estimator
          ## is just the estimator itself.
          ## Actually fit a estimator here
          ## Tratidional way:
          ## We run each of the models on the (full?) dataset
          ## to determine their performance, and we build a design matrix from
          ## their predictions and the true observed outcome. This design matrix
          ## is then used to fit the new 'SuperLearner' estimator on.
          ##
          ## Online way:
          ## We fit our initial superlearner estimator in a similar way as described
          ## above, and we update this initial estimator using the new observations
          ## as they come in.

          ## If there is no estimator, we need to fit a estimator based on Nl observations.
          ## If we already have a estimator, we update the old one, given the new measurement
          random_variable_names <- Arguments$getCharacters(colnames(observed.outcome))
          if(is.null(random_variable_names)) throw('Something went wrong, the random_variable_names are not defined')

          lapply (random_variable_names, function(random_variable_name) {
            observed_outcome <- observed.outcome[, random_variable_name, with=FALSE]

            ## Convert the predictions to wide format so we can use them per column
            predicted_outcomes <- do.call(cbind, predicted.outcome$normalized) %>%
              subset(., select = grep(paste(random_variable_name,"$",sep=""), names(.)))

            if(is.null(colnames(predicted_outcomes))) throw('Something went wrong, the predicted_outcome colnames are not defined')

            private$weightedCombinationComputers[[random_variable_name]]$process(Z = as.matrix(predicted_outcomes),
                                                        Y = as.matrix(observed_outcome),
                                                        private$SL.library.descriptions)
          })
        },

        ## Find the best estimator among the current set, for each of the densities (WAY)
        fit_dosl = function() {
          if(!self$fits_dosl) return(FALSE)

          private$verbose && enter(private$verbose, 'Finding best estimators from the candidates')
          current_risk <- self$get_cv_risk
          private$dosl.estimators <- rbindlist(current_risk) %>%
            ## Get the first if there are multiple best ones
            sapply(., function(algorithm_scores) {
              ids <- sort(algorithm_scores, index.return=TRUE)$ix
              ## We do it this way as the OSL might also get selected. This
              ## might be something we want, but for now the discrete SL can
              ## only be one of the candidates, and not the OSL.
              for(name in names(current_risk)[ids]) {
                if(name %in% names(private$SL.library.fabricated)) {
                  #private$verbose && cat(private$verbose, 'Selecting ', name)
                  cat(private$verbose, 'Selecting ', name)
                  return(private$SL.library.fabricated[[name]])
                }
              }
            })

          ## DEBUGGING ONLY
          ## This function checks the fitted DOSL and checks if each of the
          ## selected estimators indeed has the lowest risk. 
          if(FALSE) {
            for (i in seq_along(private$dosl.estimators)) {
              estimator <- private$dosl.estimators[[i]]
              scores <- current_risk[[i]]
              best_risk <- current_risk[[estimator$get_name]][[i]] 
              all_risks <- lapply(current_risk, function(x) x[[i]])
              idx <- names(all_risks) == 'osl.estimator' | names(all_risks)== 'dosl.estimator'
              min_risks <- all_risks[!(idx)] %>% unlist %>% min
              assert_that(min_risks == best_risk)
            }
          }

          private$verbose && exit(private$verbose)
          return(TRUE)
        }


        ## Fit the actual super learner on all models in the current set of models
        ##
        ## Params:
        ## @param predicted.outcome: the predicted.outcome by the learning algorithms
        ## @param observed.outcome: the outcome we observed in the actual dataset
        #fit = function(predicted.outcome, observed.outcome) {

          #fit_dosl(predicted.outcome, observed.outcome)
          #private$dosl.estimators <- private$fit_dosl()

          ## Do gradient descent update
          ## Something like:
          ##Xmat <- model.matrix(formula, train)
          ##suppressWarnings(prediction <- self$predict(train, A, W))
          ##gradient <- (t(Xmat) %*% (prediction - Ymat))
          ##self$estimator <- self$estimator - private$learning.rate * gradient

          ##prediction <- self$predict(train, A, W)
        #}

        ),
  active =
    list(
        is_online = function() {
          private$all_estimators_online
        },

        is_fitted = function() {
          private$fitted
        },

        fits_osl = function() {
          private$should_fit_osl
        },

        fits_dosl = function() {
          private$should_fit_dosl
        },

        get_historical_cv_risk = function() {
          private$historical_cv_risk
        },

        info = function() {
          if (!self$is_fitted) {
            print('Fit the algorithm first')
            return(-1)
          }
          print('Information about the current OSL fit')
          print('=====================================')
          print('The online super learner (CTS) was fit using the following weights:')
          print(self$get_osl_weights)
          print('\n-------------------------------------')
          print('The online super learner (DSC) was fit using the following estimators:')
          print('-------------------------------------')
          lapply(self$get_dosl, function(estimator) {list(estimator$get_estimator_type, bins=estimator$get_nbins)}) %>%
            print
          print('\n-------------------------------------')
          print('The cross validated risk of each estimator is')
          print('-------------------------------------')
          print(self$get_cv_risk)
          print('=====================================')
        },

        get_estimators = function() {
          return(private$SL.library.fabricated)
        },

        get_osl_weights = function() {
          sapply(private$weightedCombinationComputers, function(wcc) wcc$get_weights) %>%
            unlist
        },

        get_dosl = function() {
          return(private$dosl.estimators)
        },

        get_cv_risk = function() {
          return(private$cv_risk)
        },

        get_validity = function() {
          if (length(private$SL.library.descriptions) == 0 || length(private$SL.library.fabricated) == 0 ) {
            throw("There should be at least one estimator in the library")
          }
          if (is.null(private$summaryMeasureGenerator) || class(private$summaryMeasureGenerator) != 'SummaryMeasureGenerator') {
            throw("You need to provide a summary measure generator of class SummaryMeasureGenerator")
          }
          if (!is.a(private$weightedCombinationComputers, 'list')) {
            throw("The WCC's should be in a list, one for each RV")
          }
          private$historical_cv_risk <- Arguments$getInstanceOf(private$historical_cv_risk, 'list')
        }
        ),
  public =
    list(
        ## Functions
        ## =========
        initialize = function(SL.library.definition = c('ML.Local.lm', 'ML.H2O.glm'),
                              summaryMeasureGenerator, should_fit_osl = TRUE, should_fit_dosl = TRUE, pre_processor = NULL,
                              verbose = FALSE ) {
          self$set_verbosity(Arguments$getVerbose(verbose, timestamp = TRUE))
          private$fitted = FALSE
          private$summaryMeasureGenerator <- Arguments$getInstanceOf(summaryMeasureGenerator, 'SummaryMeasureGenerator')
          private$should_fit_dosl <- Arguments$getLogical(should_fit_dosl)
          private$should_fit_osl <- Arguments$getLogical(should_fit_osl)

          ## Cross validation initialization
          private$cv_risk = list()
          private$cv_risk_count = 0
          private$cv_risk_calculator = CrossValidationRiskCalculator$new()
          private$data_splitter <- DataSplitter$new()

          ## Initialization, Fabricate the various models
          libraryFactory <- LibraryFactory$new(verbose = verbose)
          private$SL.library.fabricated <- libraryFactory$fabricate(SL.library.definition)
          private$SL.library.descriptions <- names(private$SL.library.fabricated)
          private$all_estimators_online <- DensityEstimation.are_all_estimators_online(private$SL.library.fabricated)

          ## We need a weighted combination computer for each of the randomvariables.
          ## We could reuse the WCC, and just save the weights here. However, this way we do allow
          ## to use a different wcc for each of the random variables.
          private$weightedCombinationComputers <- list()
          private$online_super_learner_predict <- OnlineSuperLearner.Predict$new(pre_processor = pre_processor,
                                                                                 verbose = verbose)
          private$historical_cv_risk <- list()

          self$get_validity
        },

        set_verbosity = function(verbosity) {
          private$verbose = verbosity
        },

        ## Samples the data iteratively from the fitted distribution, and applies an intervention if necessary
        ## @param tau is the time at which we want to measure the outcome
        sample_iteratively = function(data, randomVariables, tau = 10, intervention = NULL, discrete = TRUE, 
                                      return_type = 'observations', 
                                      start_from_variable = NULL,
                                      start_from_time = 1) {
          randomVariables <- Arguments$getInstanceOf(randomVariables, 'list')
          randomVariables <- RandomVariable.find_ordering(randomVariables)

          tau <- Arguments$getNumerics(tau, c(1,Inf))
          start_from_time <- Arguments$getNumerics(start_from_time, c(1,tau))

          return_type <- Arguments$getCharacters(return_type)
          valid_return_types <- c('observations', 'full', 'summary_measures')
          if (!return_type %in% valid_return_types) {
            throw('Return type should be in', valid_return_types)
          }

          if(!is.null(intervention) && !InterventionParser.valid_intervention(intervention)) {
            throw('The intervention specified is not correct! it should have a when (specifying t), a what (specifying the intervention) and a variable (specifying the name of the variable to intervene on).')
          }

          ## If no random variable to start from is provided, just start from the first one
          if (is.null(start_from_variable)) {
            start_from_variable <- randomVariables[[1]]
          }

          start_from_variable <- Arguments$getInstanceOf(start_from_variable, 'RandomVariable')

          result <- data.table()
          result_denormalized_observations <- data.table(matrix(nrow=0, ncol=length(randomVariables)))
          colnames(result_denormalized_observations) <- names(randomVariables)

          ## Just to be certain we don't use future data, we remove a subset:
          remove_vars <- names(randomVariables)
          started = FALSE

          ## We need to sample sequentially here, just so we can plugin the value everytime in the next evaluation
          private$verbose && enter(private$verbose, 'Sampling from PN*')
          for (t in seq(start_from_time, tau)) {
            current_denormalized_observation <- list()
            private$verbose && enter(private$verbose,'Sampling at ', t)

            for (rv in randomVariables) {
              current_outcome <- rv$getY
              private$verbose && enter(private$verbose, 'Working on randomvariable ', current_outcome)

              if (!started) {
                if (!equals(current_outcome, start_from_variable$getY)) {
                  ## The current outcome lies in the past, so it might be that we
                  ## need it later, don't remove it
                  remove_vars <- remove_vars[remove_vars != current_outcome]
                  next
                }
                ## set the variables we don't need to NA
                data[,remove_vars] <- NA
                started = TRUE
              }

              ## Here we select whether the current node is an intervention
              ## node. This can be moved to a separate function.
              parsed_intervention <- InterventionParser.parse_intervention(
                intervention = intervention,
                current_time = t,
                current_outcome = current_outcome
              )

              ## If the current t is an intervention t, apply the proposed intervention.
              if (parsed_intervention$should_intervene) {
                outcome <- list(normalized = parsed_intervention$what, denormalized = parsed_intervention$what)
                private$verbose && cat(private$verbose, 'Setting intervention on ', current_outcome,' with ', outcome, ' on time ', t)
              } else {
                outcome <- self$predict(data = data, randomVariables = c(rv),
                                        discrete = discrete,
                                        continuous = !discrete,
                                        all_estimators = FALSE,
                                        sample = TRUE)

                private$verbose && cat(private$verbose,'Predicted ', current_outcome, 
                                       ' using ', paste(rv$getX, collapse=', '), 
                                       ' and the prediction was ', outcome$denormalized[[1]])
              }
              ## We need to add the [[1]] because the result is a list of lists (1st norm/denorm, then estimator, then
              ## values)
              data[,  (current_outcome) := as.numeric(outcome$normalized[[1]]) ]
              private$verbose && exit(private$verbose)
              current_denormalized_observation[[current_outcome]] <- outcome$denormalized[[1]] %>%
                as.numeric
            }
            private$verbose && exit(private$verbose)
            result_denormalized_observations <- rbindlist(
              list(
                result_denormalized_observations, 
                current_denormalized_observation), 
              fill=TRUE
            )

            result <- rbind(result, data)
            if(t < tau)  data <- private$summaryMeasureGenerator$getLatestCovariates(data)
          }
          private$verbose && exit(private$verbose)
          if (return_type == 'observations') {
            #if(any(is.na(result_denormalized_observations[,names(randomVariables), with = FALSE]))){
              #print(result_denormalized_observations)
            #}

            ## Return the denormalized observations?
            return(result_denormalized_observations[,names(randomVariables), with = FALSE])
            #return(result[,names(randomVariables), with = FALSE])
          } else if (return_type == 'summary_measures') {
            return(result[, !names(randomVariables), with = FALSE])
          }
          return(result)
        },

        ## Data = the data object from which the data can be retrieved
        ## initial_data_size = the number of observations needed to fit the initial estimator
        fit = function(data, randomVariables, initial_data_size = 5, max_iterations = 20, mini_batch_size = 20) {
          tic <- Sys.time()
          initial_data_size <- Arguments$getInteger(initial_data_size, c(1,Inf))
          max_iterations <- Arguments$getInteger(max_iterations, c(0,Inf))
          data <- Arguments$getInstanceOf(data, 'Data.Base')

          private$summaryMeasureGenerator$setData(data = data)

          ## TODO: Move to check validity? Needs moving of the equations as well.
          private$summaryMeasureGenerator$checkEnoughDataAvailable(randomVariables = randomVariables)

          ## We initialize the WCC's here because we need to have the randomVariables
          private$initialize_weighted_combination_calculators(randomVariables)

          print(paste('Fitting OnlineSuperLearner with a library:', paste(private$SL.library.descriptions, collapse = ', '),
                      'and we use an initial data size of', initial_data_size,
                      'with',max_iterations,'iterations,',
                      'and a minibatch of',mini_batch_size))

          ## Get the initial data for fitting the first estimator and train the initial models
          private$verbose && enter(private$verbose, 'Fitting initial estimators')
          private$summaryMeasureGenerator$getNext(initial_data_size) %>%
            private$train_library(data_current = ., randomVariables = randomVariables)
          private$verbose && exit(private$verbose)

          private$verbose && enter(private$verbose, 'Updating estimators')
          ## Update the library of models using a given number of max_iterations
          private$update_library(randomVariables = randomVariables, max_iterations = max_iterations,
                                mini_batch_size = mini_batch_size)

          ## Return the cross validated risk
          private$verbose && exit(private$verbose)

          toc <- Sys.time()
          private$verbose && cat(private$verbose, 'The whole procedure took ', (toc - tic), ' seconds.')
          return(self$get_cv_risk)
        },

        ## Predict should return a nrow(data) * 1 matrix, where the predictions are multiplied by
        ## the weights of each estimator.
        predict = function(data, randomVariables, all_estimators = TRUE, discrete = TRUE, continuous = TRUE, sample = FALSE, plot = FALSE) {

          ## Pass the function to the separate class so it won't fill up this class
          private$online_super_learner_predict$predict(osl = self, data = data, randomVariables = randomVariables,
                                               all_estimators = all_estimators, discrete = discrete, continuous = continuous,
                                               sample = sample, plot = plot)

        }
  )
)
