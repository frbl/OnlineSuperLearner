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
                  fakeUpdate = function(newData, X = c("W1"), Y = c("Y")){
                    warning('This is not an online update! We fake  the online part!')
                    private$data <- rbindlist(list(private$data, newData))
                    self$fit(data, X,Y)
                  },
                  verbose = NULL,
                  formulae.parsed = NULL

                  predict = function(datO, X = c("W1"), Y = c("Y")) {

                    # Get the current conditional density
                    conditionalDensity <- self$getConditionalDensities(Y)

                    # Store the yValues for easy access
                    yValues <- datO[[Y]]

                    # Generate a datanet object (this needs to be refactored)
                    nodeObjects <- private$defineNodeObjects(datO = datO, X = X, Y = Y)

                    # Sample from the cond dist given the provided data
                    sampled_data <- conditionalDensity$sampleA(newdata = nodeObjects$datNetObs)

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
                  initialize = function(nbins = 30, verbose = FALSE) {
                    private$verbose = verbose
                    private$nbins = nbins
                    private$conditional.densities <- list()
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

                    lapply(private$formulae.parsed,
                           function(f) {
                             self$predict(datO=data, Y=f$Y, X=f$X)
                           })
                  },

                  sampleIteratively = function(data, ordering = c('W', 'A', 'Y'), iterations=10) {
                    # TODO: The ordering can probably be extracte from the formula
                    lapply(1:iterations, function(i) {
                             lapply(ordering, function(variableToPredict) {
                                      f <- private$formulae[[variableToPredict]]
                                      data <- self$predict(datO=data, Y=f$Y, X=f$X)
                                      data <- SMG$summarizeData(data)
                              })
                           })
                  },

                  # Fits the densities according to the provided formulae
                  process = function(data, formulae) {
                    private$verbose && cat(private$verbose, 'Fitting', length(formulae), 'densities')

                    # Convert the formula in vectors (can probably be done easier)
                    private$formulae.parsed <- lapply(formulae, function(formula) {
                                                        variables <- self$parseFormula(formula)
                                                        list(Y = variables$Y, X = variables$X)
                           })

                    # Fit conditional density for all of the formulae
                    lapply(private$formulae.parsed, function(f) { self$fit(datO=data, Y=f$Y, X=f$Y) })
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
