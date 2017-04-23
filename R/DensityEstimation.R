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
          warning('This is not an online update! We fake  the online part!')
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
          nodeObjects <- private$defineNodeObjects(datO = datO, X = X, Y = Y)

          # Define est_params_list:
          subset_vars <- lapply(Y, function(var) {var})

          # Find the class of the provided variable
          outcome.class <- nodeObjects$datNetObs$datnetA$type.sVar[Y]

          # Put all est_params in RegressionClass (regression with speedglm package)
          regclass <- RegressionClass$new(bin_estimator = private$bin_estimator,
                                          nbins=private$nbins,
                                          outvar.class = outcome.class,
                                          outvar = Y,
                                          predvars = X,
                                          subset = subset_vars)

          # Create the conditional density, based on the regression just specified and fit it
          private$conditional_densities[Y] <- list(SummariesModel$new(reg = regclass, DatNet.sWsA.g0 = nodeObjects$datNetObs))
          self$getConditionalDensities(Y)$fit(data = nodeObjects$datNetObs)

          if(Y == 'A') {
            #browser()
            # Our prediction
            mean(self$predict(datO,X,Y) == datO$A)

            # GLM prediction
            model <- glm(A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family='binomial', data=datO)
            mean((predict(model, datO, type='response')>0.5) == (datO$A == 1))
          }
        },

        # Refactor:
        defineNodeObjects = function(datO, X, Y) {
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
          private$verbose <- Arguments$getVerbose(-8, timestamp=FALSE)
          #private$verbose = verbose
          private$nbins <- Arguments$getIntegers(as.numeric(nbins), c(1, Inf))
          private$bin_estimator <- bin_estimator
          private$conditional_densities <- list()
        },

        # Generates a sample given the provided data.
        sample = function(data) {
          if (is.null(private$randomVariables) | length(private$conditional_densities) == 0) {
            throw('The conditional_densities need to be fit first!')
          }

          lapply(private$randomVariables, function(rv) {
              self$predict(datO=data,
                            Y=rv$getY,
                            X=rv$getX)
            })
        },

        predict = function(datO, X, Y) {
          # TODO: Implement sampling from a non conditional distribution
          if(length(X) == 0) return(NULL)
          conditionalDensity <- self$getConditionalDensities(Y)


          # TODO: BUGS!!!!!!!! For some weird reason, the code doesn't work with NA 
          if(anyNA(datO)) {
            warning('Some NA values were replaced with zeros!')
            datO[is.na(datO)] <- 0
          }

          # This fix might not be necessary. It seems that whenever we have 1 row of data and 1 covariate, everythin
          # crashes and burns. Using this simple fix actually fixes that problem. Unfortunately, when using this fix,
          # it doesnt work when there are more than 1 covariate.
          #fixed <- FALSE
          #if(nrow(datO) == 1) {
            #print('Fixing!')
            #datO <- rbind(datO, datO)
            #fixed <- TRUE
          #}

          # Store the yValues for easy access
          yValues <- datO[[Y]]

          # Generate a datanet object (this needs to be refactored)
          nodeObjects <- private$defineNodeObjects(datO = datO, X = X, Y = Y)
          sampled_data <- conditionalDensity$sampleA(newdata = nodeObjects$datNetObs)
          #sampled_data <- conditionalDensity$getPsAsW.models()[[1]]$sampleA(newdata = nodeObjects$datNetObs)

          # TODO: We undo our fix here:
          #if(fixed) { sampled_data <- sampled_data[[1]] }

          if(FALSE){
            # Testing code
            # Create predictions
            #TODO: Why would we want to use predict over Aeqa?
            # Predict the instances where A=A (i.e., the outcome is the outcome)
            # plot densitity first:
            plot(density(datO[[Y]]), ylim=c(0,max(estimated_densities)+0.01))
            #plot(density(seq(min(yValues), max(yValues),length.out = length(estimated_densities))* estimated_densities[order(yValues)]))
            estimated_densities <- conditionalDensity$predictAeqa(newdata = nodeObjects$datNetObs)

            lines(yValues, estimated_densities, type = "p", cex = .3, col = "red")

            subs <- conditionalDensity$getPsAsW.models()[[1]]$getPsAsW.models()
            a <- lapply(seq(50), function(x) conditionalDensity$getPsAsW.models()[[1]]$sampleA(newdata = nodeObjectsSub$datNetObs))
            df <- as.data.frame(t(as.data.frame(a))); rownames(df)<-NULL

            subs <- conditionalDensity$getPsAsW.models()[[1]]$getPsAsW.models()
            conditionalDensity$predict(newdata = nodeObjectsSub$datNetObs)
          }

          sampled_data
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
                private$verbose && cat(private$verbose, 'Updating ', rv$getY)
                private$update(newdata = data, Y = rv$getY, X = rv$getX) 
              } else {
                private$verbose && cat(private$verbose, 'Fitting ', rv$getY)
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
          if(!(all(outcome %in% names(private$conditional_densities)))) throw(paste(outcome, 'is not a fitted outcome'))

          if (length(outcome) == 1) {
            return(private$conditional_densities[[outcome]])
          }
          return(private$conditional_densities[outcome])
        }
  )
)
