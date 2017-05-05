#devtools::load_all('~/Workspace/frbl/tmlenet/')
#' Density.Estimation
#'
#' @import tmlenet
#' @importFrom tmlenet def_sW def_sA
#' @include ML.Local.R
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom simcausal NetIndClass
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

        # Functions
        # =========
        fake_update = function(newdata, X, Y){
          #warning('This is not an online update! We fake  the online part!')
          private$data <- rbindlist(list(private$data, newdata))
          private$fit(private$data, X=X,Y=Y)
        },

        # Updates the used condistional density
        update = function(newdata, X, Y) {
          private$fake_update(newdata=newdata, X=X, Y=Y)
        },

        fit = function(datO, X, Y){
          #TODO: Make this step online (remove the following line)
          private$data <- datO
          nodeObjects <- private$define_node_objects(datO = datO, X = X, Y = Y)

          # Define est_params_list:
          subset_vars <- lapply(Y, function(var) {var})

          # Find the class of the provided variable
          outcome.class <- nodeObjects$datNetObs$datnetA$type.sVar[Y]

          # Put all est_params in RegressionClass (regression with speedglm package)
          regclass <- RegressionClass$new(bin_estimator = private$bin_estimator,
                                          nbins = private$nbins,
                                          outvar.class = outcome.class,
                                          outvar = Y,
                                          predvars = X,
                                          subset = subset_vars)

          # Create the conditional density, based on the regression just specified and fit it
          private$conditional_densities[Y] <- list(SummariesModel$new(reg = regclass, DatNet.sWsA.g0 = nodeObjects$datNetObs))
          self$getConditionalDensities(Y)$fit(data = nodeObjects$datNetObs)
        },

        predict_probability = function(datO, X, Y, plot = FALSE) {
          if(!(Y %in% names(datO))) throw('In order to predict the probability of an outcome, we also need the outcome')
          yValues <- datO[[Y]]
          conditionalDensity <- self$getConditionalDensities(Y)

          #TODO: Why would we want to use predict over Aeqa?
          # This fix might not be necessary. It seems that whenever we have 1 row of data and 1 covariate, everythin
          # crashes and burns. Using this simple fix actually fixes that problem. Unfortunately, when using this fix,
          # it doesnt work when there are more than 1 covariate.
          fixed <- FALSE
          if(nrow(datO) == 1) {
            print('Fixing!')
            datO <- rbind(datO, datO)
            fixed <- TRUE
          }

          nodeObjects <- private$define_node_objects(datO = datO, X = X, Y = Y)

          # Predict the instances where A=A (i.e., the outcome is the outcome)
          estimated_probabilities <- conditionalDensity$predictAeqa(newdata = nodeObjects$datNetObs)

          # We undo our fix here:
          if(fixed) { estimated_probabilities <- estimated_probabilities[[1]] }

          if (plot & length(yValues) > 1) {
            private$output_plots(yValues = yValues, estimated_probabilities = estimated_probabilities)
          }

          estimated_probabilities

          #subs <- conditionalDensity$getPsAsW.models()[[1]]$getPsAsW.models()
          #a <- lapply(seq(50), function(x) conditionalDensity$getPsAsW.models()[[1]]$sampleA(newdata = nodeObjectsSub$datNetObs))
          #df <- as.data.frame(t(as.data.frame(a))); rownames(df)<-NULL

          #subs <- conditionalDensity$getPsAsW.models()[[1]]$getPsAsW.models()
          #conditionalDensity$predict(newdata = nodeObjectsSub$datNetObs)
        },

        # Generates a sample given the provided data.
        sample = function(datO, X, Y) {
          # TODO: Implement sampling from a non conditional distribution
          if(length(X) == 0) throw('Sampling from non conditional distribution is not yet supported!')
          conditionalDensity <- self$getConditionalDensities(Y)

          # TODO: BUG! For some weird reason, the code doesn't work with NA 
          if(anyNA(datO)) {
            warning('Some NA values were replaced with zeros!')
            datO[is.na(datO)] <- 0
          }


          # Generate a datanet object (this needs to be refactored)
          nodeObjects <- private$define_node_objects(datO = datO, X = X, Y = Y)
          sampled_data <- conditionalDensity$sampleA(newdata = nodeObjects$datNetObs)


          sampled_data
        },

        # TODO: Refactor:
        define_node_objects = function(datO, X, Y) {
          # Define the nodes in the network, Anodes are the outcome nodes,
          # Wnodes are the covariate nodes / predictors
          nodes <- list(Anodes = Y, Wnodes = X , nFnode = "nF")

          X <- do.call(def_sW, as.list(X))
          Y <- do.call(def_sA, as.list(Y))

          # Create an empty friend matrix (simcausal package dependency)
          netind_cl <- NetIndClass$new(nobs = nrow(datO))

          # Define datNetObs:

          OdataDT_R6 <- OdataDT$new(Odata = datO, nFnode = "nF", iid_data_flag = FALSE)

          covariateDatNet <- DatNet$new(Odata = OdataDT_R6, netind_cl = netind_cl, nodes = nodes)
          covariateDatNet <- covariateDatNet$make.sVar(Odata = OdataDT_R6, sVar.object = X)

          outcomeDatNet   <- DatNet$new(Odata = OdataDT_R6, netind_cl = netind_cl, nodes = nodes)
          outcomeDatNet   <- outcomeDatNet$make.sVar(Odata = OdataDT_R6, sVar.object = Y)

          datNetObs <- DatNet.sWsA$new(Odata = OdataDT_R6,
                                        datnetW = covariateDatNet,
                                        datnetA = outcomeDatNet)
          datNetObs <- datNetObs$make.dat.sWsA()

          return(list(datNetObs = datNetObs, Y = Y, sW = X, nodes = nodes))
        },

        # Function to output the density estimations on top of the actual density to a series of pdfs
        output_plots = function(yValues, estimated_probabilities, dir = '/tmp/osl/') {
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
        get_bin_estimator = function() {
          return(private$bin_estimator)
        },

        get_nbins = function() {
          return(private$nbins)
        }
        ),
  public =
    list(
        initialize = function(nbins = 30, bin_estimator = tmlenet::speedglmR6$new(), verbose = FALSE) {
          private$verbose <- verbose
          private$nbins <- Arguments$getIntegers(as.numeric(nbins), c(1, Inf))
          private$bin_estimator <- bin_estimator
          private$conditional_densities <- list()
        },

        #TODO: Implement a way to run the prediction on a subset of outcomes
        predict = function(data, sample = FALSE, subset = NULL, plot = TRUE) {
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
            if (!is.a(rv, 'RandomVariable')) {
             throw('Please provide a list of randomvariables when running this function') 
            }
            private$randomVariables[[rv$getY]] <- rv
          }

          # Fit conditional density for all of the randomVariables
          lapply(randomVariables, function(rv) {
            # TODO: Currently it is is not yet possible to sample from an non-conditional distribution!
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
