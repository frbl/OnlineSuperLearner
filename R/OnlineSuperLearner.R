#' OnlineSuperLearner
#'
#' This is the main super learner class. This class contains everything related
#' to the super learner machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include Global.R
#' @include LibraryFactory.R
#' @include DataSplitter.R
#' @include SummaryMeasureGenerator.R
#' @include WeightedCombinationComputer.R
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(SL.library.definition)}}{
#'     starts a new OnlineSuperLearner. The provided /code{SL.library.definition} contains the machine learning models to use
#'   }
#'
#'   \item{\code{run(data, Y, A, W,  initial.data.size = 10)}}{
#'     Runs the actual OnlineSuperLearning calculation
#'   }
#'   \item{\code{getModel()}}{
#'     Returns the final OnlineSuperLearner model
#'   }
#'   \item{\code{predict(data, X)}}{
#'     returns an actual prediction using the superlearning model
#'   }
#' }
#' @export
OnlineSuperLearner <-
  R6Class (
           "OnlineSuperLearner",
           private =
             list(
                  # The online discrete super learner
                  odsl.estimator = NULL,

                  # ML Library
                  SL.library.names = NULL,
                  SL.library.descriptions = NULL,
                  SL.library.fabricated = NULL,
                  osl.fitted = NULL,

                  # Splitter for the data
                  dataSplitter = NULL,

                  # The family of data we are working with
                  family = NULL,

                  # Summary measures and a generator
                  summaryMeasureGenerator = NULL,

                  # Verbosity of the logs
                  verbose = FALSE,

                  # The computer for the SuperLearner combination
                  weightedCombinationComputer = NULL,

                  # Predict using all estimators separately.
                  # Params:
                  # @param data.initial: the initial dataset to train the estimators on
                  # @param Y: the column names used for the outcome
                  # @param A: the column names used for the treatment
                  # @param W: the column names used for the covariates
                  # @return a vector of outcomes, each entry being the predicted outcome of an estimator
                  predictUsingAllEstimators = function(data) {
                    private$verbose && enter(private$verbose, 'Predicting with all models')
                    #dataH2o <- as.h2o(data)
                    #private$verbose && cat(private$verbose, 'Uploaded data to h2o')

                    result <- lapply(private$SL.library.fabricated,
                      function(model) {
                        #if(is.a(model, 'ML.H2O')){
                          #current <- dataH2o
                        #} else {
                        current <- data
                        #}
                        model$sample(current)
                      })

                    # convert the list of results into a data.table
                    result <- rbindlist(result)
                    rownames(result) <- private$SL.library.descriptions
                    private$verbose && exit(private$verbose)
                    result
                  },


                  # Train using all estimators separately.
                  # Postcondition: each of our density estimators will have a fitted conditional
                  # density in them for each of our random vars WAY *AND IT SHOULD DO THIS FOR ALL
                  # $w \in W$*
                  # Params:
                  # @param data.initial: the initial dataset to train the estimators on
                  # @param Y: the column names used for the outcome
                  # @param A: the column names used for the treatment
                  # @param W: the column names used for the covariates
                  # @return a vector of outcomes, each entry being the predicted outcome of an estimator on the test set
                  trainAllEstimators = function(data, Y.eq, A.eq, W.eq) {
                    private$verbose && enter(private$verbose, 'Training all models')
                    #dataH2o <- as.h2o(data)
                    #private$verbose && cat(private$verbose, 'Uploaded data to h2o')

                    lapply(private$SL.library.fabricated,
                           function(model) {
                             #if(is.a(model, 'ML.H2O')){
                               #current <- dataH2o
                             #} else {
                              current <- data
                             #}
                             model$process(current, formulae = c(Y.eq, A.eq, W.eq))
                           }
                           )
                    private$verbose && exit(private$verbose)
                  },

                  # Functions
                  # Function to train the initial set of models
                  # Params:
                  # @param data.initial: the initial dataset to train the estimators on
                  # @param Y: the column names used for the outcome
                  # @param A: the column names used for the treatment
                  # @param W: the column names used for the covariates
                  trainLibrary = function(data.initial, Y.eq, A.eq, W.eq, Y.fam, A.fam, W.fam) {
                    if(self$fitted){
                      warning('DOSL and OSL already fitted (initially), skipping initialization')
                      return(FALSE)
                    }

                    private$verbose && enter(private$verbose, 'Creating initial fits')

                    data.splitted <- private$dataSplitter$split(data.initial)

                    # Fit the initial models
                    private$trainAllEstimators(data = data.splitted$train, Y.eq = Y.eq, A.eq = A.eq, W.eq = W.eq)

                    # TODO: Make sure that all predictions are returned in the same format
                    predicted.outcome <- private$predictUsingAllEstimators(data = data.splitted$test)
                    
                    # Calculate the error
                    observed.outcome <- data.splitted$test[,colnames(predicted.outcome), with=FALSE]
                    browser()

                    ###############################################################
                    ###############################################################
                    ###############################################################
                    ###############################################################
                    ###############################################################
                    ###############################################################
                    b <- as.matrix(predicted.outcome)
                    x <- as.matrix(observed.outcome)[1,]
                    b<- sweep(b,2,x)
                    sqrt(b^2)

                    risk.cv.update <- self$lossFunction(family)(data.observed = observed.outcome,
                                                        predicted.outcome) 

                    # Fit the super learner on the current data

                    Ymat <- as.matrix(data.initial[, Y, with = FALSE])
                    private$fit(predicted.outcome = predictions, observed.outcome = Ymat )

                    # Update the discrete superlearner (take the first if there are multiple candidates)
                    private$odsl.estimator <-
                      private$SL.library.fabricated[[which(CVpredictions == min(CVpredictions))[1]]]

                    self$setFitted(TRUE)
                    private$verbose && exit(private$verbose)

                    TRUE
                  },

                  # Function to update the models with the available data
                  # Params:
                  # @param Y: the column names used for the outcome
                  # @param A: the column names used for the treatment
                  # @param W: the column names used for the covariates
                  # @param max.iterations: the number of iterations we can maximaly run for training
                  # @param mini.batch.size: size of the batch we use
                  updateLibrary = function(Y, A, W, max.iterations, mini.batch.size){
                    private$verbose && enter(private$verbose, 'Starting model updating')
                    if(!self$fitted){
                      throw('Fit the inital D-OSL and OSL first')
                    }

                    # Set the current timestep to 1
                    t <- mini.batch.size

                    data.current <- private$summaryMeasureGenerator$getNextN(mini.batch.size)

                    # TODO: Check wether the stopping criteria are met (e.g., improvement < theta)
                    while(t < max.iterations && nrow(data.current) >= 1 && !is.null(data.current)) {
                      if(t %% (1 * mini.batch.size) == 0 && private$verbose) {
                        lapply(names(self$risk.cv), function(cv.name) {
                                 cat(private$verbose, paste('Updating OSL at iteration', t,
                                                            'error for', cv.name,
                                                            'is', self$risk.cv[cv.name]))
                           })
                      }

                      # Update all models on the previous data set, and get their prediction on the current data.

                      data.splitted <- private$dataSplitter$split(data.current)
                      private$trainAllEstimators(data = data.splitted$train, Y = Y, A = A, W = W)

                      # Make a prediction using our (super)learners
                      CVpredictions <- private$predictUsingAllEstimators(data = data.splitted$test)
                      osl.prediction <- self$predict(data = data.splitted$test, A = A, W = W)
                      dosl.prediction <- self$predict(data = data.splitted$test, A = A, W = W, discrete = TRUE)

                      # Calculate the updated cross validated risk using the new measurement
                      # Predicted.outcome is now the level 1 'matrix' of each estimator. It contains
                      # their predicted outcomes on the test set
                      predicted.outcome <- as.data.frame(c(CVpredictions, dosl = dosl.prediction, osl = osl.prediction))
                      observed.outcome <- as.matrix(tail(data.current[, Y, with = FALSE], 1))

                      # Update the cross-validated risk
                      risk.cv.update <- self$lossFunction(family)(data.observed = observed.outcome, predicted.outcome) / t
                      self$risk.cv <- (t / (t + mini.batch.size)) * self$risk.cv + risk.cv.update

                      # ReFit the super learner on the current data
                      private$fit(predicted.outcome = CVpredictions, observed.outcome = observed.outcome)

                      # Update the discrete superlearner (take the first if there are multiple candidates)
                      private$odsl.estimator <-
                        which(self$risk.cv[private$SL.library.descriptions] == min(self$risk.cv[private$SL.library.descriptions]))[1] %>%
                        private$SL.library.fabricated[[.]]

                      # Get the new row of data
                      data.current <- private$summaryMeasureGenerator$getNextN(mini.batch.size)

                      t <- t + mini.batch.size
                    }
                    private$verbose && exit(private$verbose)
                  },

                  # Fit the actual super learner on all models in the current set of models
                  #
                  # Params:
                  # @param predicted.outcome: the predicted.outcome by the learning algorithms
                  # @param observed.outcome: the outcome we observed in the actual dataset
                  fit = function(predicted.outcome, observed.outcome) {

                    # If we have 1 estimator, the weighted combination of that estimator
                    # is just the estimator itself.
                    # Actually fit a model here
                    # Tratidional way:
                    # We run each of the models on the (full?) dataset
                    # to determine their performance, and we build a design matrix from
                    # their predictions and the true observed outcome. This design matrix
                    # is then used to fit the new 'SuperLearner' model on.
                    #
                    # Online way:
                    # We fit our initial superlearner model in a similar way as described
                    # above, and we update this initial model using the new observations
                    # as they come in.

                    # If there is no model, we need to fit a model based on Nl observations.
                    # If we already have a model, we update the old one, given the new measurement
                    private$weightedCombinationComputer$process(Z = predicted.outcome,
                                                                Y = observed.outcome,
                                                                private$SL.library.descriptions)

                    # Do gradient descent update
                    # Something like:
                    #Xmat <- model.matrix(formula, train)
                    #suppressWarnings(prediction <- self$predict(train, A, W))
                    #gradient <- (t(Xmat) %*% (prediction - Ymat))
                    #self$model <- self$model - private$learning.rate * gradient

                    #prediction <- self$predict(train, A, W)
                  }

                  ),
           active =
             list(
                  fitted = function(){
                    private$osl.fitted
                  },

                  ),
           public =
             list(
                  # Variables
                  # The R.cv score of the current fit
                  risk.cv = NULL,

                  initialize = function(SL.library.definition = c('ML.Local.lm', 'ML.H2O.glm'), summaryMeasureGenerator, verbose = FALSE, family='gaussian') {
                    private$verbose <- Arguments$getVerbose(verbose, timestamp = TRUE)

                    private$family <- family
                    private$summaryMeasureGenerator <- summaryMeasureGenerator

                    # Initialization, Fabricate the various models
                    libraryFactory <- LibraryFactory$new()
                    private$SL.library.fabricated <- libraryFactory$fabricate(SL.library.definition,
                                                                              private$summaryMeasureGenerator)

                    # If the list of libraries is a list instead of a vector, convert it to a vector
                    if(is.a(SL.library.definition, 'list')){
                      SL.library.definition <- sapply(SL.library.definition, function(entry) entry$description )
                    } 
                    private$SL.library.descriptions <- SL.library.definition

                    private$verbose && cat(private$verbose, paste('Creating super learner with a library:', private$SL.library.descriptions))


                    # TODO: DIP the WCC
                    weights.initial <- rep(1 / length(private$SL.library.descriptions), length(private$SL.library.descriptions))
                    private$weightedCombinationComputer <- WCC.NLopt$new(weights.initial = weights.initial)

                    # TODO: The initial risk is 0. Should probably be Inf? 
                    self$risk.cv <- rep(0, length(private$SL.library.descriptions) + 2) # +2 for the OSL and DOSL
                    names(self$risk.cv) <- c(private$SL.library.descriptions, 'DOSL', 'OSL')

                    # TODO: Check for H2O, if we need it
                    private$dataSplitter <- DataSplitter$new()

                    self$setFitted(FALSE)

                    self$getValidity()
                  },

                  getValidity = function() {
                    if (length(private$SL.library.descriptions) == 0 || length(private$SL.library.fabricated) == 0 ) {
                      throw("There should be at least one estimator in the library")
                    }
                    if (is.null(private$summaryMeasureGenerator) || class(private$summaryMeasureGenerator) != 'SummaryMeasureGenerator') {
                      throw("You need to provide a summary measure generator of class SummaryMeasureGenerator")
                    }
                    if (is.null(private$family) || private$family == "") {
                      throw("The provided family is not valid")
                    }
                    if (!is.a(private$weightedCombinationComputer, 'WeightedCombinationComputer')) {
                      throw("The provided WCC is not a WCC")
                    }
                  },

                  setFitted = function(value) {
                    private$osl.fitted = value
                  },

                  evaluateModels = function(data, W, A, Y) {
                    lapply(private$SL.library.fabricated,
                           function(model) {
                             data.predicted <-  model$predict(data = data, A = A,  W = W)
                             data.observed <- data[, Y, with = FALSE][[Y]]
                             self$lossFunction(family)(data.observed = data.observed,
                                               data.predicted = data.predicted)
                           })
                  },

                  # Data = the data object from which the data can be retrieved
                  # initial.data.size = the number of observations needed to fit the initial model
                  run = function(data, W.eq, A.eq, Y.eq, W.fam = 'gaussian', A.fam='binomial', Y.fam='gaussian',
                                 initial.data.size = 5, max.iterations = 20, mini.batch.size = 20) {

                    private$summaryMeasureGenerator$setData(data = data)

                    # TODO: Move to check validity? Needs moving of the equations as well.
                    if(!private$summaryMeasureGenerator$checkEnoughDataAvailable(formulae = c(W.eq, A.eq, Y.eq))) {
                      throw('Not all provided variables are included in the SMGs, include the correct SMGs')
                    }

                    # Get the initial data for fitting the first model and train the initial models
                    private$summaryMeasureGenerator$getNextN(initial.data.size) %>%
                      private$trainLibrary(data.initial = ., 
                                           Y.eq = Y.eq, A.eq = A.eq, W.eq = W.eq,
                                           Y.fam = Y.fam, A.fam = A.fam, W.fam = W.fam
                                           )

                    # Update the library of models using a given number of max.iterations
                    #private$updateLibrary(Y = Y, A = A, W = W, max.iterations = max.iterations,
                                          #mini.batch.size = mini.batch.size)

                    # Return the cross validated risk
                    return(self$risk.cv)
                  },

                  # Predict should return a nrow(data) * 1 matrix, where the predictions are multiplied by
                  # the weights of each estimator.
                  predict = function(data, A = NULL, W = NULL, discrete = FALSE) {
                    if(!self$fitted){
                      return(NA)
                    }

                    # TODO: This can be very inefficient, especially when we are updating both of them.
                    # We could perform the prediction only once, which saves time.
                    if(discrete){
                      return(private$odsl.estimator$predict(data = data, A = A, W = W))
                    }

                    # This will be the convex combination of the data and the models (and weights)
                    # something like
                    # private$summaryMeasureGenerator$setData(data = data)
                    # data <- private$summaryMeasureGenerator$getNext()
                    predictions <- as.matrix(private$predictUsingAllEstimators(data=data, A=A, W=W))
                    weights <- as.matrix(private$weightedCombinationComputer$getWeights)

                    return(predictions %*% weights)
                  },

                  getModels = function() {
                    return(private$SL.library.fabricated)
                  }

                  )
           )

