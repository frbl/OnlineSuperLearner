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

        ## Functions
        ## =========
        fake_update = function(newdata, X, Y) {
          ##warning('This is not an online update! We fake  the online part!')
          private$data <- rbindlist(list(private$data, newdata))
          private$fit(private$data, X=X,Y=Y)
        },

        ## Updates the used condistional density
        update = function(newdata, X, Y) {
          if (self$is_online) {
            data_obj <- condensier::DataStore$new(input_data = newdata, Y = Y, X = X, auto_typing = FALSE)
            self$getConditionalDensities(Y)$update(newdata = data_obj)
          } else {
            private$fake_update(newdata = newdata, X = X, Y = Y)
          }
        },

        fit = function(datO, X, Y){
          if (!self$is_online) {
            ## If the actual algorithm is not online, we fake the online part
            private$data <- datO
          }

          dens_fit <- condensier::fit_density(X = X,
                                  Y = Y,
                                  input_data = datO,
                                  nbins = private$nbins,
                                  bin_estimator = private$bin_estimator)
          private$conditional_densities[Y] <- list(dens_fit)

        },

        predict_probability = function(datO, X, Y, plot = FALSE) {
          if(!(Y %in% names(datO))) throw('In order to predict the probability of an outcome, we also need the outcome')
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
          estimated_probabilities <- condensier::predict_probability(conditionalDensity, datO)

          ## We undo our fix here:
          ## if(fixed) { estimated_probabilities <- estimated_probabilities[[1]] }

          if (plot && length(yValues) > 1) {
            private$create_output_plots(yValues = yValues, 
                                        estimated_probabilities = estimated_probabilities, 
                                        output = Y)
          }

          estimated_probabilities
        },

        ## Generates a sample given the provided data.
        sample = function(datO, X, Y, plot = FALSE) {
          ## TODO: Implement sampling from a non conditional distribution
          if(length(X) == 0) throw('Sampling from non conditional distribution is not yet supported!')
          yValues <- datO[[Y]]
          conditionalDensity <- self$getConditionalDensities(Y)

          ## TODO: BUG! For some weird reason, the code doesn't work with NA
          ## FIXED
          ## if(anyNA(datO)) {
          ##   # TODO: warning('Some NA values were replaced with zeros!')
          ##   # datO[is.na(datO)] <- 0
          ## }

          sampled_data <- condensier::sample_value(conditionalDensity, datO)

          if (plot && length(yValues) > 1) {
            private$create_output_plots(yValues = yValues, 
                                        estimated_probabilities = density(sampled_data)$y,
                                        estimated_y_values = density(sampled_data)$x,
                                        output = Y)
          }

          sampled_data
        },

        ## Function to output the density estimations on top of the actual density to a series of pdfs
        create_output_plots = function(yValues, estimated_probabilities, estimated_y_values = NULL, output, dir = '/tmp/osl/') {
          ## plot densitity first:
          vals <- unique(yValues)
          if(length(vals) == 2 ) {
            ## If the outcome is binary, we can see how well it managed to predict the whole distribution
            ## This error term should be small (~ 0.001)
            abs(mean(estimated_probabilities[yValues == vals[1] ]) - mean(yValues == vals[1]))
            abs(mean(estimated_probabilities[yValues == vals[2] ]) - mean(yValues == vals[2]))
          }
          true_density <- density(yValues)

          ## Normalize the density to 1
          ##true_density$y <- diff(true_density$x) * true_density$y

          ## Draw a line by default, instead of dots
          type = "l"

          if (is.null(estimated_y_values)) {
            estimated_y_values <- yValues 
            type = "p"
          }

          ## Normalize to sum to one, to make it an actual density

          ## Save the output in a dir so we can access it later
          date <- format(Sys.time(), "%y%m%d%H%M")
          full_dir <- paste(dir, date, '/', sep ='')
          dir.create(full_dir, showWarnings = FALSE, recursive = TRUE)

          pdf(paste(full_dir,output,'.pdf',sep = ''))
          plot(true_density, ylim=c(0,max(estimated_probabilities)+.5))
          lines(estimated_y_values, estimated_probabilities, type = type, cex = .3, col = "red",
                ylim=c(0,max(estimated_probabilities)+.5))
          dev.off()
        }
      ),
  active =
    list(
        is_online = function() {
          return(private$is_online_estimator)
        },

        get_bin_estimator = function() {
          return(private$bin_estimator)
        },

        get_nbins = function() {
          return(private$nbins)
        },

        get_estimator_type = function() {
          list(
            fitfunname = private$bin_estimator$fitfunname,
            lmclass = private$bin_estimator$lmclass
          )
        }
      ),
  public =
    list(
        initialize = function(nbins = 30, bin_estimator = NULL, online = FALSE, verbose = FALSE) {
          private$verbose <- Arguments$getVerbose(verbose)
          private$is_online_estimator <- Arguments$getLogical(online)
          private$nbins <- Arguments$getIntegers(as.numeric(nbins), c(1, Inf))

          if (is.null(bin_estimator)) { bin_estimator <- condensier::speedglmR6$new() }
          private$bin_estimator <- Arguments$getInstanceOf(bin_estimator, 'logisfitR6')
          private$conditional_densities <- list()
        },

        ##TODO: Implement a way to run the prediction on a subset of outcomes
        predict = function(data, sample = FALSE, subset = NULL, plot = FALSE) {
          data <- Arguments$getInstanceOf(data, 'data.table')
          plot <- Arguments$getLogical(plot)
          if (is.null(private$randomVariables) | length(private$conditional_densities) == 0) {
            throw('The conditional_densities need to be fit first!')
          }

          results <- lapply(private$randomVariables, function(rv) {
            current_outcome <- rv$getY

            ## Return NA if we want to skip this iteration (next is not available in lapply)
            if (!is.null(subset) && !(current_outcome %in% subset)) return(NA)
            if(sample) {
              private$sample(datO=data, Y = current_outcome, X = rv$getX, plot = plot)
            } else {
              private$predict_probability(datO = data, Y = current_outcome, X = rv$getX, plot = plot)
            }
          }) 
          results[!is.na(results)]
        },

        ## Fits the densities according to the provided randomVariables
        process = function(data, randomVariables, update=FALSE) {
          private$verbose && cat(private$verbose, 'Fitting ', length(randomVariables), ' densities')

          ## Convert the formula in vectors (can probably be done easier)
          for (rv in randomVariables) {
            private$randomVariables[[rv$getY]] <- Arguments$getInstanceOf(rv, 'RandomVariable')
          }

          ## Fit conditional density for all of the randomVariables
          lapply(private$randomVariables, function(rv) {
            ## TODO: Currently it is is not yet possible to sample from an non-conditional distribution!
            ## OS: Maybe the following hack (its probably not a very good one):
            ## 1) Fit unconditional density using the same method (histogram) with intercept only GLMs
            ## 2) Sample from that fit just like conditional density
            if(length(rv$getX) > 0) {
              if (update) {
                private$verbose && cat(private$verbose, 'Updating density: ', rv$getY)
                private$update(newdata = data, Y = rv$getY, X = rv$getX)
              } else {
                private$verbose && cat(private$verbose, 'Fitting density: ', rv$getY)
                private$fit(datO = data, Y = rv$getY, X = rv$getX)
              }
            }
          })
          TRUE
        },

        getConditionalDensities = function(outcome = NULL) {
          if(length(private$conditional_densities) == 0) throw('Densities not yet fitted')

          ## If no outcome type is provided, just return all of them
          if(is.null(outcome)) return(private$conditional_densities)
          outcome <- Arguments$getCharacters(outcome)

          if(!(all(outcome %in% names(private$conditional_densities)))) throw(paste(outcome, 'is not a fitted outcome'))

          if (length(outcome) == 1) {
            return(private$conditional_densities[[outcome]])
          }
          return(private$conditional_densities[outcome])
        }
  )
)
