context("Simulator.GAD.R")
test_that("it should be tested properly", {
  log <- Arguments$getVerbose(-8, timestamp=TRUE)

  sim <- Simulator.GAD$new()

  tic <- Sys.time()
  ## 'nobs' observations
  nobs <- 1e3
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
  data <- sim$simulateWAY(nobs, qw=llW, ga=llA, Qy=llY, verbose=log)
  toc <- Sys.time()
  comp.time <- toc-tic
})
