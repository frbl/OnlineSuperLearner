## testing function "simulator.R"
library(R6)
library(R.utils)
## library(inline)
## library(Rcpp)
source("../R/simulator.R")

log <- Arguments$getVerbose(-8, timestamp=TRUE)

sim <- Simulator$new()

## ###############################
## Simulation scheme, scenario one
## ###############################

if (FALSE) {
  ## 'nobs' iid observations
  nobs <- 50
  ## W(t)|Past(W(t))~N(0,1)
  ## A(t)|Past(A(t))~Ber(0.5)
  ## Y(t)|Past(Y(t))~Ber(0.5)
  data.iid <- sim$simulateWAYScenarioOne(nobs, verbose=log) ## very slow!
}

if (FALSE) {
  version <- c("slow", "faster")[1]
  comp.time <- c(slow=NA, faster=NA)
  tic <- Sys.time()
  ## 'nobs' observations
  nobs <- 1000
  ## W(t)|Past(W(t))~N(0.5*Y(t-1)-0.25*A(t-1)+0.1*W(t-1), 1)
  ## A(t)|Past(A(t))~Ber(expit(-0.1+0.1*W(t)+0.25*Y(t-1)))
  ## Y(t)|Past(Y(t))~Ber(expit(0.1+0.1*A(t)+0.1*W(t)+0.05*Y(t-1)-0.01*A(t-1)))
  ## initializations: zero
  paramW <- c(0, 0.5, -0.25, 0.1)
  qw <- sim$generateMechanismScenarioOne(paramW, family="gaussian")
  paramA <- c(-0.1, 0.1, 0.25)
  ga <- sim$generateMechanismScenarioOne(paramA, family="bernoulli")
  paramY <- c(0.1, 0.1, 0.1, 0.05, -0.01)
  Qy <- sim$generateMechanismScenarioOne(paramY, family="bernoulli")
  data.dep <- sim$simulateWAYScenarioOne(nobs, qw=qw, ga=ga, Qy=Qy, verbose=log, version=version)
  toc <- Sys.time()
  comp.time[version] <- toc-tic
}

## ###############################
## Simulation scheme, scenario two
## ###############################

if (TRUE) {
  tic <- Sys.time()
  ## 'nobs' observations
  nobs <- 1e6
  llW <- list(stochMech=rnorm,
              param=c(0, 0.5, -0.25, 0.1),
              rgen=identity)
  llA <- list (stochMech=function(ww) {
    rbinom(length(ww), 1, expit(ww))
  },
  param=c(-0.1, 0.1, 0.25),
  rgen=function(xx, delta=0.05){
    rbinom(length(xx), 1, delta+(1-2*delta)*expit(xx))
  })
  llY <- list(stochMech=function(aa, ww){
    aa*ww+(1-aa)*(-ww)
  },
  param=c(0.1, 0.1, 0.1, 0.05, -0.01),
  rgen=identity)
  ##
  data <- sim$simulateWAYScenarioTwo(nobs, qw=llW, ga=llA, Qy=llY, verbose=log)
  toc <- Sys.time()
  comp.time <- toc-tic
}
