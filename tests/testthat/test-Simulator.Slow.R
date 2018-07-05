context("Simulator.Slow.R")
test_that("it should return a data table", {
  log <- Arguments$getVerbose(-8, timestamp=TRUE)

  sim <- Simulator.Slow$new()
  ## 'nobs' observations
  nobs <- 10
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
  data.dep <- sim$simulateWAY(numberOfBlocks = nobs, qw=qw, ga=ga, Qy=Qy, verbose=log, version=version)
  expect_is(data.dep, 'data.table')
})

test_that("it should be relatively fast", {
  log <- FALSE

  sim <- Simulator.Slow$new()
  comp.time <- NA
  tic <- Sys.time()
  ## 'nobs' observations
  nobs <- 100
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
  data.dep <- sim$simulateWAY(numberOfBlocks = nobs, qw=qw, ga=ga, Qy=Qy, verbose=log, version=version)
  toc <- Sys.time()
  comp.time <- toc-tic
  expect_true(comp.time < 1)
})
