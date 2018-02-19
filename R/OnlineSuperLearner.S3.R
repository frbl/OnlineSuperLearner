#' fit.OnlineSuperLearner
#'
#' Fits an online superlearner using a similar notation as a GLM.
#' @param formulae list a list of all randomVariable objects that need to be fitted
#'
#' @param data data.frame the data set to use for fitting the OSL
#'
#' @param algorithms list of algorithms to use in the online superlearner 
#'
#' @param normalize boolean (default = FALSE) we provide the option to
#'  normalize the data in the OSL procedure. This entails that the package will
#'  automatically select a set of bounds (min and max) based on the data set
#'  provided. After that it will only use the normalized features (all scaled
#'  between 0-1).
#'
#' @param measurements_per_obs integer (default = Inf) the number of
#'  measurments in a single observation. 
#'
#' @param ... other parameters directly passed to the OSL and fit function.
#'  There are several named variables to provide here:
#'  - initial_data_size 
#'  - max_iterations
#'  - mini_batch_size
#'  See for a full list the documentation of the \code{OnlineSuperLearner}
#'  \code{fit} and \code{initialize} functions.
#' @return a fitted version of an \code{OnlineSuperLearner} class
#' @export
fit.OnlineSuperLearner <- function(formulae, data, algorithms = NULL, normalize = FALSE, measurements_per_obs = Inf, ...) {
  ## Convert the data.frame to a data.static object
  if(!is(data, 'Data.Base')) data <- Data.Static$new(dataset = data)

  ## Build an SMG Factory from the provided formulae
  smg_factory <- OnlineSuperLearner::SMGFactory$new()

  ## Check if the provided formulae are indeed 
  formulae <- Arguments$getInstanceOf(formulae, 'list')
  formulae <- lapply(formulae, function(rv) Arguments$getInstanceOf(rv, 'RandomVariable'))

  pre_processor <- NULL
  if (normalize) {
    bounds <- PreProcessor.generate_bounds(data.train)
    pre_processor <- PreProcessor$new(bounds = bounds)
  }

  smg <- smg_factory$fabricate(formulae,
    pre_processor = pre_processor,
    number_of_observations_per_timeseries = measurements_per_obs
  )

  osl  <- OnlineSuperLearner$new(SL.library.definition = algorithms,
                                 random_variables = formulae,
                                 summaryMeasureGenerator = smg,
                                 pre_processor = pre_processor,
                                 ...)
  osl$fit(data, ...)
  return(osl)
}

#' sample.OnlineSuperLearner
#' 
#' S3 prediction function for the online superlearner package. Can be used to
#' perform a sampling procedure on the fitted OSL method.
#'
#' @param object OnlineSuperLearner trained instance of an online superlearner class.
#'
#' @param newdata the new data to perform the prediction with. Note that this
#'  can be a data.frame, after which we will generate blocks based on the
#'  measurements in the data, or a \code{Data.Base}, which _should_ already
#'  include all necessary variables.
#'
#' @param Y the dependent variables for which we want to predict the outcome.
#'  The parameter is allowed to take several forms:
#' 
#'   - List of \code{RandomVariable} objects to predict
#'   - Single \code{RandomVariable} object to predict
#'   - List of strings with the names of the outputs (\code{list('X','Y')})
#'   - Single string with the name of the output (\code{'Y'})
#'
#' @param ... other parameters directly passed to the predict function
#'
#' @return \code{data.table} a \code{data.table} containing the sampled outcomes
#' @export
sample.OnlineSuperLearner <- function(object, newdata, Y = NULL, ...) {
  ## Test if the provided object is actually a OnlineSuperlearner
  object <- Arguments$getInstanceOf(object, 'OnlineSuperLearner')

  ## Convert newdata to data.static
  if(!is(newdata, 'Data.Base')) {
    Data.Static$new(dataset = newdata) %>%
      object$get_summary_measure_generator$setData(.)
    newdata <- object$get_summary_measure_generator$getNext(nrow(newdata))
  }

  if (!is.null(Y)) Y <- object$retrieve_list_of_random_variables(random_variables = Y)
  object$predict(data = newdata, randomVariables = Y, sample = TRUE, ...)
}

#' @export
sample <- function(object, newdata, Y = NULL, ...) UseMethod("sample")

#' predict.OnlineSuperLearner
#' 
#' S3 prediction function for the online superlearner package. Can be used to
#' perform a prediction on the trained online superlearner object.
#'
#' @param object OnlineSuperLearner trained instance of an online superlearner class.
#' @param newdata the new data to perform the prediction with. Note that this
#'  can be a data.frame, after which we will generate blocks based on the
#'  measurements in the data, or a \code{Data.Base}, which _should_ already
#'  include all necessary variables.
#' @param Y the dependent variables for which we want to predict the outcome.
#'  The parameter is allowed to take several forms:
#'   - List of \code{RandomVariable} objects to predict
#'   - Single \code{RandomVariable} object to predict
#'   - List of strings with the names of the outputs (\code{list('X','Y')})
#'   - Single string with the name of the output (\code{'Y'})
#' @param ... other parameters directly passed to the predict function
#' @return \code{data.table} a \code{data.table} containing the predicted probabilities
#' @export
predict.OnlineSuperLearner <- function(object, newdata, Y = NULL, ...) {
  ## Test if the provided object is actually a OnlineSuperlearner
  object <- Arguments$getInstanceOf(object, 'OnlineSuperLearner')

  ## Convert newdata to data.static
  if(!is(newdata, 'Data.Base')) {
    Data.Static$new(dataset = newdata) %>%
      object$get_summary_measure_generator$setData(.)

    newdata <- object$get_summary_measure_generator$getNext(nrow(newdata))
  }

  if (!is.null(Y)) Y <- object$retrieve_list_of_random_variables(random_variables = Y)
  object$predict(data = newdata, randomVariables = Y, sample = FALSE, ...)
}

#' summary.OnlineSuperLearner
#'
#' S3 method to provide a summary about the online superlearner object. Prints
#' a description about the current fit of the OSL.
#' @param object onlinesuperlearner the trained OSL instance
#' @export
summary.OnlineSuperLearner <- function(object, ...) {
  if (!is(object, 'OnlineSuperLearner')) {
    throw('The provided object is not an online superlearner instance') 
  }
  object$info
}

