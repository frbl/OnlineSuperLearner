context("Simulator.GAD.R")
context(" simulateWAY")
nobs <- 4
sim <- Simulator.GAD$new()
result <- sim$simulateWAY(by = nobs)

test_that("it should return a dataframe with the correct columnames", {
  expect_equal(colnames(result), c('W', 'A', 'Y'))
})

test_that("it should contain some non zero data", {
  expect_false(all(result == 0))
})

test_that("it should have the correct number of rows in the output", {
  expect_true(nrow(result) == nobs)
})

test_that("it should be tested properly", {
  log <- Arguments$getVerbose(-8, timestamp=TRUE)
  log <- FALSE

  sim <- Simulator.GAD$new()

  ## One trajectory
  tic <- Sys.time()
  nobs <- 1e2
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

  llY <- list(rgen={function(AW){
    aa <- AW[, "A"]
    ww <- AW[, grep("[^A]", colnames(AW))]
    mu <- aa*(0.4-0.2*sin(ww)+0.05*ww) +
      (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
    rnorm(length(mu), mu, sd=1)}})
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
  ## Simulating under an intervention
  tic <- Sys.time()
  intervention <- list(when=c(10, 15, 20),
                       what=c(1, 1, 0))
  ## -> parameter E((Y_{1,10}+Y_{1,15}+Y_{0,20})/3)
  B <- 1e2
  
  #'psi.approx' is a Monte-Carlo approximation of the parameter of interest.
  # This is a little slow, because 'simulateWAYOneTrajectory' is designed to simulate quickly a long time series,
  # as opposed to many short time series. 
  # TODO: Workaround: parallelize!
  psi.approx <- mean(sapply(1:B, function(bb) {
    when <- max(intervention$when)
    data.int <- sim$simulateWAYOneTrajectory(max(when), qw=llW, ga=llA, Qy=llY,
                                             intervention=intervention, verbose=FALSE)
    data.int$Y[when]
  }))
  ## 'psi.approx' approximates the parameter of interest
  toc <- Sys.time()
  comp.time <- c(comp.time, toc-tic)
  
})
