context("Simulator.GAD.R")
test_that("it should be tested properly", {
  log <- Arguments$getVerbose(-8, timestamp=TRUE)

  sim <- Simulator.GAD$new()

  ## One trajectory
  tic <- Sys.time()
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
  data.1 <- sim$simulateWAYOneTrajectory(nobs, qw=llW, ga=llA, Qy=llY, verbose=log)
  toc <- Sys.time()
  comp.time <- toc-tic
  ## Three trajectories merged into one
  tic <- Sys.time()
  ntraj <- 3
  data.3 <- sim$simulateWAYiidTrajectories(nobs, ntraj,
                                           qw=llW, ga=llA, Qy=llY, verbose=log)
  toc <- Sys.time()
  comp.time <- c(comp.time, toc-tic)
})
