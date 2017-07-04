#' Density.Estimation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom condensier DataStore fit_density predict_probability sample_value speedglmR6
DensityEstimation <- R6Class ("DensityEstimation",
  private =
    list(
        # Variables
        # =========
        conditional_densities = NULL,
        data = NULL,
        nbins = NULL,
        verbose = NULL,
        randomVariables = NULL,
        bin_estimator = NULL,
        is_online_estimator = NULL,

        # Functions
        # =========
        predict_probability = function(datO, X, Y, plot = FALSE) {
          if(!(Y %in% names(datO))) throw('In order to predict the probability of an outcome, we also need the outcome')
          yValues <- datO[[Y]]
          conditionalDensity <- self$getConditionalDensities(Y)

          #TODO: Why would we want to use predict over Aeqa?
          # This fix might not be necessary. It seems that whenever we have 1 row of data and 1 covariate, everythin
          # crashes and burns. Using this simple fix actually fixes that problem. Unfortunately, when using this fix,
          # it doesnt work when there are more than 1 covariate.
          ##
          ## THIS WAS A TOUGH ONE, BUT NOW APPEARS FIXED. ALSO ADDED TEST FOR PREDICTIONS WITH ONLY ONE ROW OF DATA.
          ##
          # fixed <- FALSE
          # if(nrow(datO) == 1) {
          #   # datO <- rbind(datO, datO)
          #   # fixed <- TRUE
          # }

          # Predict the instances where A=A (i.e., the outcome is the outcome)
          estimated_probabilities <- condensier::predict_probability(conditionalDensity, datO)

          # We undo our fix here:
          # if(fixed) { estimated_probabilities <- estimated_probabilities[[1]] }

          if (plot && length(yValues) > 1) {
            private$create_output_plots(yValues = yValues, 
                                        estimated_probabilities = estimated_probabilities, 
                                        output = Y)
          }

          estimated_probabilities
        },

        # Generates a sample given the provided data.
        sample = function(datO, X, Y, plot = FALSE) {
          # TODO: Implement sampling from a non conditional distribution
          if(length(X) == 0) throw('Sampling from non conditional distribution is not yet supported!')
          yValues <- datO[[Y]]
          conditionalDensity <- self$getConditionalDensities(Y)

          # TODO: BUG! For some weird reason, the code doesn't work with NA
          ## FIXED
          # if(anyNA(datO)) {
          #   # TODO: warning('Some NA values were replaced with zeros!')
          #   # datO[is.na(datO)] <- 0
          # }

          sampled_data <- condensier::sample_value(conditionalDensity, datO)

          if (plot && length(yValues) > 1) {
            private$create_output_plots(yValues = yValues, 
                                        estimated_probabilities = density(sampled_data)$y,
                                        estimated_y_values = density(sampled_data)$x,
                                        output = Y)
          }

          sampled_data
        },

        # Function to output the density estimations on top of the actual density to a series of pdfs
        create_output_plots = function(yValues, estimated_probabilities, estimated_y_values = NULL, output, dir = '/tmp/osl/') {
          # plot densitity first:
          vals <- unique(yValues)
          if(length(vals) == 2 ) {
            # If the outcome is binary, we can see how well it managed to predict the whole distribution
            # This error term should be small (~ 0.001)
            abs(mean(estimated_probabilities[yValues == vals[1] ]) - mean(yValues == vals[1]))
            abs(mean(estimated_probabilities[yValues == vals[2] ]) - mean(yValues == vals[2]))
          }
          true_density <- density(yValues)

          # Normalize the density to 1
          #true_density$y <- diff(true_density$x) * true_density$y

          # Draw a line by default, instead of dots
          type = "l"

          if (is.null(estimated_y_values)) {
            estimated_y_values <- yValues 
            type = "p"
          }

          # Normalize to sum to one, to make it an actual density

          # Save the output in a dir so we can access it later
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

        #TODO: Implement a way to run the prediction on a subset of outcomes
        predict = function(data, sample = FALSE, subset = NULL, plot = FALSE) {
          data <- Arguments$getInstanceOf(data, 'data.table')
          plot <- Arguments$getLogical(plot)
          if (is.null(private$randomVariables) | length(private$conditional_densities) == 0) {
            throw('The conditional_densities need to be fit first!')
          }

          results <- lapply(private$randomVariables, function(rv) {
            current_outcome <- rv$getY

            # Return NA if we want to skip this iteration (next is not available in lapply)
            if (!is.null(subset) && !(current_outcome %in% subset)) return(NA)
            if(sample) {
              private$sample(datO=data, Y = current_outcome, X = rv$getX, plot = plot)
            } else {
              private$predict_probability(datO = data, Y = current_outcome, X = rv$getX, plot = plot)
            }
          }) 
          results[!is.na(results)]
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
            if(length(rv$getX) > 0) {
              private$verbose && cat(private$verbose, 'Updating density: ', Y)
              data_obj <- condensier::DataStore$new(input_data = newdata, Y = Y, X = X, auto_typing = FALSE)
              dens_fit <- self$getConditionalDensities(Y)
              dens_fit$update(newdata = data_obj)

              private$conditional_densities[Y] <- list(dens_fit)
            }
          }
          TRUE
        },

        fit = function(datO, randomVariables){
          if(is.null(private$randomVariables)) {
            self$set_random_variables(randomVariables = randomVariables)
          }

          lapply(private$randomVariables, function(rv) {
            # TODO: Currently it is is not yet possible to sample from an non-conditional distribution!
            ## OS: Maybe the following hack (its probably not a very good one):
            ## 1) Fit unconditional density using the same method (histogram) with intercept only GLMs
            ## 2) Sample from that fit just like conditional density
            X = rv$getX
            Y = rv$getY
            if(length(X) > 0) {
              private$verbose && cat(private$verbose, 'Fitting density: ', Y)
              dens_fit <- condensier::fit_density(X = X,
                                      Y = Y,
                                      input_data = datO,
                                      nbins = private$nbins,
                                      bin_estimator = private$bin_estimator)
              private$conditional_densities[Y] <- list(dens_fit)
            }
          })
          TRUE
        },

        getConditionalDensities = function(outcome = NULL) {
          if(length(private$conditional_densities) == 0) throw('Densities not yet fitted')

          # If no outcome type is provided, just return all of them
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
