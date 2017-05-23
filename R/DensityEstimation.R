#' Density.Estimation
#'
#' @docType class
#' @importFrom R6 R6Class
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
        fake_update = function(newdata, X, Y) {
          ##warning('This is not an online update! We fake  the online part!')
          private$data <- rbindlist(list(private$data, newdata))
          private$fit(private$data, X=X,Y=Y)
        },

        # Updates the used condistional density
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
            # If the actual algorithm is not online, we fake the online part
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

          if (plot & length(yValues) > 1) {
            private$create_output_plots(yValues = yValues, estimated_probabilities = estimated_probabilities)
          }

          estimated_probabilities
        },

        # Generates a sample given the provided data.
        sample = function(datO, X, Y) {
          # TODO: Implement sampling from a non conditional distribution
          if(length(X) == 0) throw('Sampling from non conditional distribution is not yet supported!')
          conditionalDensity <- self$getConditionalDensities(Y)

          # TODO: BUG! For some weird reason, the code doesn't work with NA
          ## FIXED
          # if(anyNA(datO)) {
          #   # TODO: warning('Some NA values were replaced with zeros!')
          #   # datO[is.na(datO)] <- 0
          # }

          sampled_data <- condensier::sample_value(conditionalDensity, datO)

          sampled_data
        },

        # Function to output the density estimations on top of the actual density to a series of pdfs
        create_output_plots = function(yValues, estimated_probabilities, dir = '/tmp/osl/') {
          # plot densitity first:
          vals <- unique(yValues)
          if(length(vals) == 2 ) {
            # If the outcome is binary, we can see how well it managed to predict the whole distribution
            # This error term should be small (~ 0.001)
            abs(mean(estimated_probabilities[yValues == vals[1] ]) - mean(yValues == vals[1]))
            abs(mean(estimated_probabilities[yValues == vals[2] ]) - mean(yValues == vals[2]))
          }
          dir.create(dir, showWarnings = FALSE)
          date <- as.integer(format(Sys.time(), "%y%m%d%H%M"))
          name <- runif(1,0,10000)
          pdf(paste(dir,date,'-',name,'.pdf',sep = ''))
          plot(density(yValues), ylim=c(0,max(estimated_probabilities)+.5))
          lines(yValues, estimated_probabilities, type = "p", cex = .3, col = "red")
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
        }
        ),
  public =
    list(
        initialize = function(nbins = 30, bin_estimator = condensier::speedglmR6$new(), online = FALSE, verbose = FALSE) {
          private$verbose <- Arguments$getVerbose(verbose)
          private$is_online_estimator <- Arguments$getLogical(online)
          private$nbins <- Arguments$getIntegers(as.numeric(nbins), c(1, Inf))
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

          lapply(private$randomVariables, function(rv) {
            if(sample) {
              private$sample(datO=data, Y=rv$getY, X=rv$getX)
            } else {
              private$predict_probability(datO=data, Y=rv$getY, X=rv$getX, plot = plot)
            }
          })
        },

        # Fits the densities according to the provided randomVariables
        process = function(data, randomVariables, update=FALSE) {
          private$verbose && cat(private$verbose, 'Fitting ', length(randomVariables), ' densities')

          # Convert the formula in vectors (can probably be done easier)
          for (rv in randomVariables) {
            private$randomVariables[[rv$getY]] <- Arguments$getInstanceOf(rv, 'RandomVariable')
          }

          # Fit conditional density for all of the randomVariables
          lapply(private$randomVariables, function(rv) {
            # TODO: Currently it is is not yet possible to sample from an non-conditional distribution!
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
