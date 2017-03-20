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
                  nobs = 100000
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
                    datO <- self$defaultDataTable[sample(.N,100)]
                    self$fit(datO = datO)
                    datO <- self$defaultDataTable
                    self$predict(datO = datO)
                  },

                  fit = function(datO, X = c("W1"), Y = c("Y"), nbins = 20){
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
                    throw('Not yet implemented')
                  },

                  predict = function(datO, X = c("W1"), Y = c("Y")) {
                    nodeObjects <- self$defineNodeObjects(datO = datO, X = X, Y = Y)

                    # Create predictions
                    browser()
                    self$model$predict(newdata = nodeObjects$datNetObs)

                    estimated_densities <- self$model$predictAeqa(newdata = nodeObjects$datNetObs)
                    y_values <- datO[[Y]]

                    # TESTING:
                    #setWvals <- c(W1 = 0)
                    #subs <- (datO$W1==setWvals["W1"])
                    #y_values <- y_values[subs]
                    #estimated_densities <- estimated_densities[subs]

                    # plot densitity first:
                    plot(density(datO[[Y]]))
                    lines(y_values, estimated_densities, type = "p", cex = .3, col = "red")

                    estimated_densities
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
