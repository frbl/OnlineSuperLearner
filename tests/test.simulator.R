## testing function "simulator.R"
library(R6)
library(R.utils)
library(inline)
library(Rcpp)
source("../R/simulator.R")

log <- Arguments$getVerbose(-8, timestamp=TRUE)

sim <- Simulator$new()

if (FALSE) {
  ## 'nobs' iid observations
  nobs <- 50
  ## W(t)|Past(W(t))~N(0,1)
  ## A(t)|Past(A(t))~Ber(0.5)
  ## Y(t)|Past(Y(t))~Ber(0.5)
  data.iid <- sim$simulateBinaryWAY(nobs, verbose=log) ## very slow!
}

if (TRUE) {
  version <- c("slow", "faster")[1]
  comp.time <- c(slow=NA, faster=NA)
  tic <- Sys.time()
  ## 'nobs' observations
  nobs <- 250
  ## W(t)|Past(W(t))~N(0.5*Y(t-1)-0.25*A(t-1)+0.1*W(t-1), 1)
  ## A(t)|Past(A(t))~Ber(expit(-0.1+0.1*W(t)+0.25*Y(t-1)))
  ## Y(t)|Past(Y(t))~Ber(expit(0.1+0.1*A(t)+0.1*W(t)+0.05*Y(t-1)-0.01*A(t-1)))
  ## initializations: zero
  paramW <- c(0, 0.5, -0.25, 0.1)
  qw <- sim$generateMechanism(paramW, family="gaussian")
  paramA <- c(-0.1, 0.1, 0.25)
  ga <- sim$generateMechanism(paramA, family="bernoulli")
  paramY <- c(0.1, 0.1, 0.1, 0.05, -0.01)
  Qy <- sim$generateMechanism(paramY, family="bernoulli")
  data.dep <- sim$simulateBinaryWAY(nobs, qw=qw, ga=ga, Qy=Qy, verbose=log, version=version)
  toc <- Sys.time()
  comp.time[version] <- toc-tic
}

