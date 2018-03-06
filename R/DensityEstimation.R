#' DensityEstimation
#'
#' This class performs the actual density estimation for each of the relevant
#' variables provided to it. 
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom condensier DataStore fit_density predict_probability sample_value speedglmR6
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(nbins = 30, bin_estimator = NULL, online = FALSE, name = 'default', verbose = FALSE) }}{ 
#'     Creates a new density estimator. One can provide various hyper
#'     parameters. First of all the number of bins the estimator uses can be
#'     configured, by default this is 30. Then one can define the actual
#'     estimator the density estimator uses for estimating the conditional
#'     densities. By default it uses speedglm. Finally, one can select whether
#'     to treat the algorithm as an online or batch algorithm. If online is set
#'     to false, we will keep a record of the data that has been used to train
#'     an estimator. Note that this is currently very inefficient.
#'
#'     @param nbins (default = 30) integer the number of bins to use for the
#'      estimator.
#'
#'     @param bin_estimator (default = NULL) ML.Base the actual estimator used
#'      for fitting the conditional density
#'
#'     @param online (default = FALSE) boolean does the algorithm have an
#'      updating possibility? And if so, should we treat it as an online
#'      algorithm?
#'
#'     @param name (default = 'default') the name to use for the density
#'     estimator.
#'
#'     @param verbose (default = FALSE) the verbosity (log level) to use while running.
#'   } 
#' 
#'   \item{\code{predict(data, sample = FALSE, subset = NULL, plot = FALSE, check = FALSE) }}{ 
#'     Method to perform the prediction on all (or a subset of) the relevant
#'     variables / conditional densities. One can provide the option to
#'     \code{sample}, which means that if this is set, the predict function
#'     will sample new values from the underlying distributions. If this
#'     argument is set to false, this function will return predicted
#'     probabilities.
#'
#'     @param data the data from which to predict the outcome. It depends on
#'      the goal of the prediction what this needs to be. If the goal is to
#'      sample a new relevant variable, the value for the relevant variables to
#'      sample does not need to be set in the data table (they can be NA).
#'      However, if one wants to know the probability of an instance of a relevant
#'      variable given the other variables, ($P(Y | X_1, X2_)$), then the relevant
#'      variable cannot be empty. 
#'
#'     @param sample (default = FALSE) boolean would we like to sample a value
#'      (true) or a probability (false) from the conditional density
#'
#'     @param subset (default = NULL) stringarray do we want to perform predictions for all
#'      variables? (NULL), or just a subset thereof?
#'
#'     @param plot (default = FALSE) boolean plot the predicted outcomes to a
#'      file in /tmp/osl. This can be used for debugging (i.e., it shows the
#'      sampled distribution over the actual distribution.
#'
#'     @return list a list containing the predictions, where each entry is one
#'      of the relevantvariables for which a conditional distribution was fit.
#'   } 
#'
#'   \item{\code{predict_probability(datO, X, Y, plot = FALSE, check = FALSE) }}{ 
#'     Internal method used by the \code{predict} function. This function
#'     predicts a $P(Y | X)$. These arguments are therefore instances of the
#'     RelevantVariable class. The data of these relevant variables needs to be
#'     included in the \code{datO} argument.
#'
#'     @param datO the data from which to predict the probability.
#'      As we want to predict the probability of Y given a set of X, ($P(Y |
#'      X_1, X2_)$), the relevant variable column in \code{datO} cannot be empty. 
#'
#'     @param sample (default = FALSE) boolean would we like to sample a value
#'      (true) or a probability (false) from the conditional density
#'
#'     @param subset (default = NULL) stringarray do we want to perform predictions for all
#'      variables? (NULL), or just a subset thereof?
#'
#'     @param plot (default = FALSE) boolean plot the predicted outcomes to a
#'      file in /tmp/osl. This can be used for debugging (i.e., it shows the
#'      sampled distribution over the actual distribution.
#'
#'     @return list a list containing the predictions, where each entry is one
#'      of the relevantvariables for which a conditional distribution was fit.
#'   } 
#' 
#'   \item{\code{getConditionalDensities(outcome = NULL) }}{ 
#'     Function to get all fitted conditional densities. By default it will
#'     return the full list of conditional densities (when \code{outcome =
#'     NULL}).  One could also provide a subset of relevant variables to the
#'     function.  Note that if only a single variable is returned (e.g.
#'     \code{outcome = 'Y'}), it will return a single conditional density
#'     (i.e., this outcome is not encapsulated in a list). If a vector of
#'     outcomes is provided it will return a list.
#'
#'     @param outcome (default = NULL) the subset of outcomes for which a
#'      conditional density needs to be returned. When \code{NULL} it will
#'      return all outcomes.
#'
#'     @return either all conditional densities in a list, a subset (in a
#'      list), or a single density.
#'   } 
#'
#'   \item{\code{is_online()}}{
#'     Active method. returns whether the estimator is initialized to be online
#'     or not.
#'
#'     @return boolean true if the estimator is fitted as an online estimator.
#'   }
#' 
#'   \item{\code{get_bin_estimator()}}{
#'     Active method. Returns the algorithm that is used for fitting the bins
#'     (i.e., the machine learning algorithm). 
#'
#'     @return ML.base the actual algorithm used to fit the density.
#'   } 
#'
#'   \item{\code{get_nbins()}}{
#'     Active method. the number of bins used to split the continuous density
#'     distribution.
#'
#'     @return integer the number of bins.
#'   }
#'
#'   \item{\code{get_name()}}{
#'     Active method. Returns the set name for the current estimator.
#'
#'     @return string the name of the estimator.
#'   }
#'
#'   \item{\code{get_raw_conditional_densities()}}{
#'     Active method. Returns the conditional densities. Note that this method
#'     should preferably not be used. Using the
#'     \code{getConditionalDensities()} method is prefered. 
#'
#'     @return list the conditional densities
#'   }
#'
#'   \item{\code{get_estimator_type()}}{
#'     Active method. returns a list with two elements. First \code{fitfunname}
#'     the name of the function used to fit the density, and \code{lmclass} the
#'     lmclass for each of the algorithms.
#' 
#'     @return list with the \code{fitfunname} and the \code{lmclass}
#'   }
#' }  
DensityEstimation <- R6Class ("DensityEstimation",
  #class = FALSE,
  cloneable = FALSE,
  portable = FALSE,
  public =
    list(
        initialize = function(nbins = 30, bin_estimator = NULL, online = FALSE, name = 'default', verbose = FALSE) {
          private$verbose <- Arguments$getVerbose(verbose)
          private$is_online_estimator <- Arguments$getLogical(online)
          private$nbins <- Arguments$getIntegers(as.numeric(nbins), c(1, Inf))

          if (is.null(bin_estimator)) { bin_estimator <- condensier::glmR6$new() }
          private$bin_estimator <- Arguments$getInstanceOf(bin_estimator, 'logisfitR6')
          private$conditional_densities <- list()
          self$set_name(name = name)
        },

        ##TODO: Implement a way to run the prediction on a subset of outcomes
        predict = function(data, sample = FALSE, subset = NULL, plot = FALSE, check = FALSE) {
          if (check) {
            data <- Arguments$getInstanceOf(data, 'data.table')
            plot <- Arguments$getLogical(plot)
            if (is.null(self$get_relevant_variables) || length(self$get_raw_conditional_densities) == 0) {
              throw('The conditional_densities need to be fit first!')
            }
          }

          results <- lapply(self$get_relevant_variables, function(rv) {
            current_outcome <- rv$getY

            ## Return NA if we want to skip this iteration (next is not available in lapply)
            if (!is.null(subset) && !(current_outcome %in% subset)) return(NA)

            if(sample) {
              self$sample(datO=data, Y = current_outcome, X = rv$getX, plot = plot)
            } else {
              self$predict_probability(datO = data, Y = current_outcome, X = rv$getX, plot = plot, check = check)
            }
          }) 
          results[!is.na(results)]
        },

        predict_probability = function(datO, X, Y, plot = FALSE, check = FALSE) {
          if(check && !(Y %in% names(datO))) throw('In order to predict the probability of an outcome, we also need the outcome')
          yValues <- datO[[Y]]
          conditionalDensity <- self$getConditionalDensities(Y)

          ## TODO: Why would we want to use predict over Aeqa?
          ## This fix might not be necessary. It seems that whenever we have 1 row of data and 1 covariate, everythin
          ## crashes and burns. Using this simple fix actually fixes that problem. Unfortunately, when using this fix,
          ## it doesnt work when there are more than 1 covariate.
          ##
          ## THIS WAS A TOUGH ONE, BUT NOW APPEARS FIXED. ALSO ADDED TEST FOR PREDICTIONS WITH ONLY ONE ROW OF DATA.
          ##
          ## fixed <- FALSE
          ## if(nrow(datO) == 1) {
          ##    datO <- rbind(datO, datO)
          ##    fixed <- TRUE
          ## }

          ## Predict the instances where A=A (i.e., the outcome is the outcome)
          ## NOTE! These estimated probabilities contain NAs whenever an estimator was fitted without any oata.

          estimated_probabilities <- condensier::predict_probability(
            model_fit = conditionalDensity, 
            newdata = datO
          )

          ## We undo our fix here:
          ## if(fixed) { estimated_probabilities <- estimated_probabilities[[1]] }

          if (plot && length(yValues) > 1) {
            OutputPlotGenerator.create_density_plot(
              yValues = yValues, 
              estimated_probabilities = estimated_probabilities, 
              output = paste(self$get_name, Y)
            )
          }

          estimated_probabilities
        },

        ## Generates a sample given the provided data.
        sample = function(datO, X, Y, plot = FALSE, check = FALSE) {
          ## TODO: Implement sampling from a non conditional distribution
          if(length(X) == 0) throw('Sampling from non conditional distribution is not yet supported!')

          #print(paste('Sampling from:',Y, 'conditional on',paste(X, collapse='+')))

          yValues <- datO[[Y]]
          conditionalDensity <- self$getConditionalDensities(Y)

          ## TODO: BUG! For some weird reason, the code doesn't work with NA
          ## FIXED
          ## if(anyNA(datO)) {
          ##   # TODO: warning('Some NA values were replaced with zeros!')
          ##   # datO[is.na(datO)] <- 0
          ## }

          # Outcome (sampled_data) is a vector of samples
          sampled_data <- condensier::sample_value(
            model_fit = conditionalDensity,
            newdata = datO
          )

          if (plot && length(yValues) > 1) {
            density_for_plot <- density(x = sampled_data)
            OutputPlotGenerator.create_density_plot(
              yValues = yValues, 
              estimated_probabilities = density_for_plot$y,
              estimated_y_values = density_for_plot$x,
              output = paste('sampled', self$get_name, Y, sep = '-')
            )
          }

          sampled_data
        },

        fit = function(datO, relevantVariables){
          if(is.null(self$get_relevant_variables)) {
            self$set_relevant_variables(relevantVariables = relevantVariables)
          }

          ## Fit conditional density for all of the relevantVariables
          for(rv in private$relevantVariables) {
            ## TODO: Currently it is is not yet possible to sample from an non-conditional distribution!
            ## OS: Maybe the following hack (its probably not a very good one):
            ## 1) Fit unconditional density using the same method (histogram) with intercept only GLMs
            ## 2) Sample from that fit just like conditional density
            X <- rv$getX
            Y <- rv$getY
            family <- rv$getFamily

            if(length(X) > 0) {
              dens_fit <- self$fit_single_rv(
                datO = datO,
                X = X,
                Y = Y,
                family = family
              )
              private$store_conditional_density(Y = Y, density = dens_fit)
            }
          }
          TRUE
        },

        fit_single_rv = function(datO, X, Y, family) {
            if (family == 'binomial') {
              bins <- 2
            } else {
              bins <- private$nbins
            }
            private$verbose && cat(private$verbose, 'Fitting density: ', Y, ' on ', self$get_name)
            dens_fit <- condensier::fit_density(
              X = X,
              Y = Y,
              input_data = datO,
              nbins = bins,
              bin_estimator = private$bin_estimator
            )
            return(dens_fit)
        },

        set_relevant_variables = function(relevantVariables) {
          # Convert the formula in vectors (can probably be done easier)
          for (rv in relevantVariables) {
            private$relevantVariables[[rv$getY]] <- Arguments$getInstanceOf(rv, 'RelevantVariable')
          }
        },

        # Updates the used condistional density
        update = function(newdata) {
          for (rv in private$relevantVariables) {
            ## TODO: Currently it is is not yet possible to sample from an non-conditional distribution!
            ## OS: Maybe the following hack (its probably not a very good one):
            ## 1) Fit unconditional density using the same method (histogram) with intercept only GLMs
            ## 2) Sample from that fit just like conditional density
            X <- rv$getX
            Y <- rv$getY
            if(length(X) > 0) {
              private$verbose && cat(private$verbose, 'Updating density: ', Y)
              data_obj <- private$create_data_store(newdata = newdata, Y = Y, X = X)
              dens_fit <- self$getConditionalDensities(Y)

              ## Update the fit with the new data
              updated_dens_fit <- dens_fit$update(newdata = data_obj)
              private$store_conditional_density(Y = Y, density = updated_dens_fit)
            }
          }
          TRUE
        },

        getConditionalDensities = function(outcome = NULL) {
          if(length(private$conditional_densities) == 0) throw('Densities not yet fitted')

          ## If no outcome type is provided, just return all of them
          if(is.null(outcome)) return(private$conditional_densities)
          outcome <- Arguments$getCharacters(outcome)

          if(!(all(outcome %in% names(private$conditional_densities)))) throw(paste(outcome, 'is not a fitted outcome'))

          if (length(outcome) == 1) return(private$conditional_densities[[outcome]])
          return(private$conditional_densities[outcome])
        },

        set_name = function(name) {
          private$name <- Arguments$getCharacters(name)
        }
    ),
  active =
    list(
        is_online = function() {
          return(private$is_online_estimator)
        },

        get_relevant_variables = function() {
          private$relevantVariables
        },

        get_bin_estimator = function() {
          return(private$bin_estimator)
        },

        get_nbins = function() {
          return(private$nbins)
        },

        get_name = function() {
          return(private$name)
        },

        get_raw_conditional_densities = function() {
          return(private$conditional_densities)
        },

        get_estimator_type = function() {
          list(
            fitfunname = private$bin_estimator$fitfunname,
            lmclass = private$bin_estimator$lmclass
          )
        }
      ),
  private =
    list(
        ## Variables
        # =========
        conditional_densities = NULL,
        data = NULL,
        nbins = NULL,
        verbose = NULL,
        relevantVariables = NULL,
        bin_estimator = NULL,
        is_online_estimator = NULL,
        name = NULL,

        # Functions
        # =========
        store_conditional_density = function(Y, density) {
          private$conditional_densities[Y] <- list(density)
        },

        create_data_store = function(newdata, Y, X) {
          condensier::DataStore$new(input_data = newdata, Y = Y, X = X, auto_typing = FALSE)
        }

  )
)

#' Static method to check whether all included estimators are in fact specified
#' as being online estimators. If this is not the case, we should keep a cache
#' of all data somewhere. 
#' @param estimators list of all estimator objects (should be density estimator objects)
#' @return boolean TRUE if all estimators are online, FALSE if not
#' @export
DensityEstimation.are_all_estimators_online = function(estimators) {
  for (estimator in estimators) {
    if(!estimator$is_online) {
      return(FALSE)
    }
  }
  return(TRUE)
}
