#' Density.Estimation
#' This class performs the actual density estimation for each of the random variables provided to it. 
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom condensier DataStore fit_density predict_probability sample_value speedglmR6
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(nbins = 30, bin_estimator = NULL, online = FALSE, verbose = FALSE) }}{ 
#'     Creates a new density estimator. One can provide various hyper parameters. First of all the number of bins the
#'     estimator uses can be configured, by default this is 30. Then one can define the actual estimator the density
#'     estimator uses for estimating the conditional densities. By default it uses speedglm. Finally, one can select
#'     whether to treat the algorithm as an online or batch algorithm. If online is set to false, we will keep a record
#'     of the data that has been used to train an estimator. Note that this is currently very inefficient.
#'     @param nbins integer the number of bins to use for the estimator
#'     @param bin_estimator ML.Base the actual estimator used for fitting the conditional density
#'     @param online boolean does the algorithm have an updating possibility? And should we treat it as online?
#'     @param verbose the verbosity
#'   } 
#' 
#'   \item{\code{predict(data, sample = FALSE, subset = NULL, plot = FALSE) }}{ 
#'     Method to perform the prediction on all (or a subset of) the random variables / conditional densities.
#'     @param data the data from which to predict the outcome.
#'     @param sample boolean would we like to sample a value (true) or a probability (false) from the conditional
#'     density
#'     @param subset stringarray do we want to perform predictions for all variables? (NULL), or just a subset thereof?
#'     @param plot boolean plot the predicted outcomes to a file in /tmp/osl
#'   } 
#' 
#'   \item{\code{process(data, randomVariables, update = FALSE) }}{ 
#'     Either fits or updates the conditional densities for each of the variables. 
#'     @param data the data to fit or update the bin estimation algorithm on.
#'     @param randomVariables list of RandomVariable objects. The randomvariables for which a density should be fit.
#'     @param update boolean is the current call an update or a fit? False = fit.
#'   } 
#' 
#'   \item{\code{getConditionalDensities(outcome = NULL) }}{ 
#'     Method to get all fitted conditional densities. By default it will return the full list of conditional densities
#'     (\code{outcome = NULL}), but if an outcome is provided a subset is returned a subset is returned. Note that if
#'     only a single variable is returned (e.g. \code{outcome = 'Y'}), it will return a single conditional density. If
#'     a vector of outcomes is provided it will return a list.
#'     @param outcome the outcome for which a conditional density needs to be returned.
#'     @return either all conditional densities, or a subset, or a single density.
#'   } 
#'
#'   \item{\code{is_online()}}{
#'     Active method, returns whether the estimator is online or not
#'     @return boolean true if the estimator is fitted as an online estimator
#'   }
#' 
#'   \item{\code{get_bin_estimator()}}{
#'     Active method, Returns the algorithm that is used fot the bins. 
#'     @return ML.base the actual algorithm used to fit the density
#'   } 
#'
#'   \item{\code{get_nbins()}}{
#'     Active method, the number of bins used to split the continuous density distribution 
#'     @return integer the number of bins
#'   }
#'
#'   \item{\code{get_estimator_type()}}{
#'     Active method, returns a list with two elements. First \code{fitfunname} the name of the function used to fit the
#'     density, and \code{lmclass} the lmclass for each of the algorithms.
#'     @return the list with the \code{fitfunname} and the \code{lmclass}
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
            if (is.null(self$get_random_variables) || length(self$get_raw_conditional_densities) == 0) {
              throw('The conditional_densities need to be fit first!')
            }
          }

          results <- lapply(self$get_random_variables, function(rv) {
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

        fit = function(datO, randomVariables){
          if(is.null(self$get_random_variables)) {
            self$set_random_variables(randomVariables = randomVariables)
          }

          ## Fit conditional density for all of the randomVariables
          for(rv in private$randomVariables) {
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

        set_random_variables = function(randomVariables) {
          # Convert the formula in vectors (can probably be done easier)
          for (rv in randomVariables) {
            private$randomVariables[[rv$getY]] <- Arguments$getInstanceOf(rv, 'RandomVariable')
          }
        },

        # Updates the used condistional density
        update = function(newdata) {
          for (rv in private$randomVariables) {
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

        get_random_variables = function() {
          private$randomVariables
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
        randomVariables = NULL,
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
