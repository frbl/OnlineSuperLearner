devtools::load_all('~/Workspace/frbl/tmlenet/')
#' Density.Estimation
#'
#' @import tmlenet
#' @importFrom tmlenet def_sW def_sA
#' @include ML.Local.R
#' @docType class
#' @importFrom R6 R6Class
Density.Estimation <-
  R6Class (
           "Density.Estimation",
                    inherit = ML.Local,
           private =
             list(
                  data = NULL,
                  nobs = 1000,
                  fakeUpdate = function(newData, X = c("W1"), Y = c("Y")){
                    warning('This is not an online update! We fake  the online part!')
                    data <- rbindlist(list(private$data, newData))
                    self$fit(data, X,Y)
                  }
                  ),
           active =
             list(
                  # This is data for testing purposes only
                  defaultDataTable = function() {
                    W1 = rbinom(n=private$nobs,size=1, prob=0.3)
                    mean = (100 * W1)
                    Y=(rnorm(private$nobs,mean,10))
                    data.table(W1=W1, Y = Y)
                  }
                  ),
           public =
             list(
                  initialize = function() {
                    set.seed(12345)
                    datO <- self$defaultDataTable
                    self$fit(datO = datO)
                    datO <- self$defaultDataTable
                    self$predict(datO = datO)
                  },

                  fit = function(datO, X = c("W1"), Y = c("Y"), nbins = 20){
                    #TODO: Make this step online (remove the following line)
                    private$data <- datO

                    nodeObjects <- self$defineNodeObjects(datO = datO, X = X, Y = Y)

                    DatNet_object <- nodeObjects$datNetObs

                    # Define est_params_list:
                    subset_vars <- lapply(Y, function(var) {var})

                    # Find the class of the provided variable
                    sA_class <- nodeObjects$datNetObs$datnetA$type.sVar[Y]

                    # Put all est_params in RegressionClass (regression with speedglm package)
                    regclass <- RegressionClass$new(nbins=nbins,
                                                    outvar.class = sA_class,
                                                    outvar = Y,
                                                    predvars = X,
                                                    subset = subset_vars)

                    # Create the model, based on the regression just specified and fit it
                    self$model <- SummariesModel$new(reg = regclass, DatNet.sWsA.g0 = nodeObjects$datNetObs)
                    self$model$fit(data = nodeObjects$datNetObs)
                  },

                  update = function(datO, X = c("W1"), Y = c("Y")) {
                    private$fakeUpdate(datO, X=X, Y=Y)
                  },

                  predict = function(datO, X = c("W1"), Y = c("Y")) {
                    nodeObjects <- self$defineNodeObjects(datO = datO, X = X, Y = Y)
                    nodeObjectsSub <- self$defineNodeObjects(datO = datO[1:30,], X = X, Y = Y)

                    # Create predictions
                    #TODO: Why would we want to use predict over Aeqa?
                    self$model$predict(newdata = nodeObjectsSub$datNetObs)


                    # Predict the instances where A=A (i.e., the outcome is the outcome)
                    estimated_densities <- self$model$predictAeqa(newdata = nodeObjects$datNetObs)
                    estimated_densities2 <- self$model$predictAeqa(newdata = nodeObjectsSub$datNetObs)
                    y_values <- datO[[Y]]

                    # TESTING:
                    #setWvals <- c(W1 = 0)
                    #subs <- (datO$W1==setWvals["W1"])
                    #y_values <- y_values[subs]
                    #estimated_densities <- estimated_densities[subs]

                    # plot densitity first:
                    plot(density(y_values), ylim=c(0,max(estimated_densities)+0.01))
                    #plot(density(seq(min(y_values), max(y_values),length.out = length(estimated_densities))* estimated_densities[order(y_values)]))
                    
                    lines(y_values, estimated_densities, type = "p", cex = .3, col = "red")

                    subs = self$model$getPsAsW.models()[[1]]$getPsAsW.models()
                    subs[[10]]$getfit
                    tot = -50
                    lapply(subs, function(i) {if(i$bw.j < 1000) tot <- tot + i$bw.j;print(tot)})
                    #sampleA(newdata = nodeObjectsSub$datNetObs)
                    browser()
                    print('')

                  },

                  # Refactor:
                  defineNodeObjects =  function(datO, X, Y) {
                    nodes <- list(Anodes = Y, Wnodes = X , nFnode = "nF")
                    sW <- def_sW(W1 = "W1")
                    Y <- def_sA(Y = "Y")
                    netind_cl <- simcausal::NetIndClass$new(nobs = nrow(datO))
                    print(netind_cl)
                    # Define datNetObs:
                    OdataDT_R6 <- OdataDT$new(Odata = datO, nFnode = "nF", iid_data_flag = FALSE)
                    datnetW <- DatNet$new(Odata = OdataDT_R6, netind_cl = netind_cl, nodes = nodes)$make.sVar(Odata = OdataDT_R6, sVar.object = sW)
                    datnetA <- DatNet$new(Odata = OdataDT_R6, netind_cl = netind_cl, nodes = nodes)$make.sVar(Odata = OdataDT_R6, sVar.object = Y)
                    datNetObs <- DatNet.sWsA$new(Odata = OdataDT_R6, datnetW = datnetW, datnetA = datnetA)$make.dat.sWsA()
                    return(list(datNetObs = datNetObs, Y = Y, sW = sW, nodes = nodes))
                  }
                  )
           )
