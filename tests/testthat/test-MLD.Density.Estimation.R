context("MLD.Density.Estimation.R")
#set.seed(12345)
#datO <- self$defaultDataTable
#self$fit(datO = datO)
#datO <- self$defaultDataTable
#self$predict(datO = datO)

# This is data for testing purposes only
defaultDataTable = function() {
  nobs = 1000
  W1 = rbinom(n=nobs,size=1, prob=0.3)
  mean = (100 * W1)
  Y=(rnorm(nobs,mean,10))
  data.table(W1=W1, Y = Y)
}

# TESTING:
#nodeObjectsSub <- self$defineNodeObjects(datO = datO[1:40,], X = X, Y = Y)
#estimated_densities2 <- conditionalDensity$predictAeqa(newdata = nodeObjectsSub$datNetObs)
#setWvals <- c(W1 = 0)
#subs <- (datO$W1==setWvals["W1"])
#yValues <- yValues[subs]
#estimated_densities <- estimated_densities[subs]
