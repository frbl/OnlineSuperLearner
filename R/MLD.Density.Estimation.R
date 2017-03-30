devtools::load_all('~/Workspace/frbl/tmlenet/')
#' Density.Estimation
#'
#' @import tmlenet
#' @importFrom tmlenet def_sW def_sA
#' @include ML.Local.R
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom simcausal NetIndClass
MLD.Density.Estimation <-
  R6Class (
           "MLD.Density.Estimation",
           private =
             list(
                  conditional.densities = NULL,
                  data = NULL,
                  nbins = NULL,
                  SMG = NULL,
                  verbose = NULL,
                  formulae.parsed = NULL,

                  fakeUpdate = function(newData, X = c("W1"), Y = c("Y")){
                    warning('This is not an online update! We fake  the online part!')
                    private$data <- rbindlist(list(private$data, newData))
                    self$fit(data, X,Y)
                  },

                  predict = function(datO, X = c("W1"), Y = c("Y")) {

                    # Get the current conditional density
                    conditionalDensity <- self$getConditionalDensities(Y)

                    # Store the yValues for easy access
                    yValues <- datO[[Y]]

                    # Generate a datanet object (this needs to be refactored)
                    nodeObjects <- private$defineNodeObjects(datO = datO, X = X, Y = Y)

                    # Sample from the cond dist given the provided data
                    sampled_data <- conditionalDensity$sampleA(newdata = nodeObjects$datNetObs)
                    #sampled_data <- conditionalDensity$getPsAsW.models()[[1]]$sampleA(newdata = nodeObjects$datNetObs)

                    if(FALSE){
                      # Testing code
                      # Create predictions
                      #TODO: Why would we want to use predict over Aeqa?
                      # Predict the instances where A=A (i.e., the outcome is the outcome)
                      estimated_densities <- conditionalDensity$predictAeqa(newdata = nodeObjects$datNetObs)
                      # plot densitity first:
                      plot(density(datO[[Y]]), ylim=c(0,max(estimated_densities)+0.01))
                      #plot(density(seq(min(yValues), max(yValues),length.out = length(estimated_densities))* estimated_densities[order(yValues)]))

                      lines(yValues, estimated_densities, type = "p", cex = .3, col = "red")

                      subs <- conditionalDensity$getPsAsW.models()[[1]]$getPsAsW.models()
                      a <- lapply(seq(50), function(x) conditionalDensity$getPsAsW.models()[[1]]$sampleA(newdata = nodeObjectsSub$datNetObs))
                      df <- as.data.frame(t(as.data.frame(a))); rownames(df)<-NULL

                      subs <- conditionalDensity$getPsAsW.models()[[1]]$getPsAsW.models()
                      conditionalDensity$predict(newdata = nodeObjectsSub$datNetObs)
                    }

                    sampled_data
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
                    regclass <- RegressionClass$new(nbins=private$nbins,
                                                    outvar.class = outcome.class,
                                                    outvar = Y,
                                                    predvars = X,
                                                    subset = subset_vars)

                    # Create the conditional density, based on the regression just specified and fit it
                    private$conditional.densities[Y] <- list(SummariesModel$new(reg = regclass, DatNet.sWsA.g0 = nodeObjects$datNetObs))
                    self$getConditionalDensities(Y)$fit(data = nodeObjects$datNetObs)
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
                  ),
           public =
             list(
                  initialize = function(nbins = 30, verbose = FALSE, summaryMeasureGenerator) {
                    private$verbose = verbose
                    private$nbins <- Arguments$getIntegers(as.numeric(nbins), c(1, Inf))
                    private$conditional.densities <- list()
                    private$SMG <- summaryMeasureGenerator
                  },

                  parseFormula = function(formula){
                    vars <- all.vars(formula)
                    depvar <- head(vars, 1)
                    names(depvar) <- depvar

                    indepvar <- tail(vars, -1)
                    names(indepvar) <- indepvar

                    list(Y = depvar, X = indepvar)
                  },

                  # Generates a sample given the provided data.
                  sample = function(data) {
                    if (is.null(private$formulae.parsed)) {
                      throw('The conditional densities need to be fit first!')
                    }

                    lapply(private$formulae.parsed, function(parsed.formula) {
                        private$predict(datO=data,
                                     Y=parsed.formula$Y,
                                     X=parsed.formula$X)
                      })
                  },

                  sampleIteratively = function(data, ordering = c('W', 'A', 'Y'), iterations=10) {
                    # TODO: The ordering can probably be extracte from the formula
                    for (i in seq(iterations)) {
                      print(data)
                      data[,ordering] <- NA
                      for (variableToPredict in ordering) {
                        parsed.formula <- private$formulae.parsed[[variableToPredict]]
                        cat('Predicting', parsed.formula$Y,'using',parsed.formula$X,'\n')
                        data[[parsed.formula$Y]] <- private$predict(datO=data,
                                                                    Y=parsed.formula$Y,
                                                                    X=parsed.formula$X)
                      }
                      data <- private$SMG$getLatestCovariates(data)
                    }
                    
                  },

                  # Fits the densities according to the provided formulae
                  process = function(data, formulae) {
                    private$verbose && cat(private$verbose, 'Fitting', length(formulae), 'densities')

                    # Convert the formula in vectors (can probably be done easier)
                    private$formulae.parsed <- list()
                    for (i in 1:length(formulae)) {
                      variables <- self$parseFormula(formulae[[i]])
                      private$formulae.parsed[[variables$Y]] <- list(Y = variables$Y, X = variables$X)
                    }

                    # Fit conditional density for all of the formulae
                    lapply(private$formulae.parsed, function(f) { private$fit(datO=data, Y=f$Y, X=f$X) })
                    TRUE
                  },


                  # Updates the used condistional density
                  update = function(datO, X = c("W1"), Y = c("Y")) {
                    private$fakeUpdate(datO, X=X, Y=Y)
                  },


                  getConditionalDensities = function(outcome = NULL) {
                    if(is.null(private$conditional.densities)) throw('Densities not yet fitted')

                    # If no outcome type is provided, just return all of them
                    if(is.null(outcome)) return(private$conditional.densities)
                    if(!(all(outcome %in% names(private$conditional.densities)))) throw(paste(outcome, 'not a fitted outcome'))

                    if (length(outcome) == 1) {
                      return(private$conditional.densities[[outcome]])
                    }
                    return(private$conditional.densities[outcome])
                  }

                  )
           )
