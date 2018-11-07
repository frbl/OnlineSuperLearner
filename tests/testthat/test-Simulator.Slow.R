context("Simulator.Slow.R")
described.class <- Simulator.Slow
subject <- Simulator.Slow$new()

context(" initialize")
##====================================================================
test_that("it should initialize with no errors", {
  expect_error(described.class$new(), NA)
})

test_that("it should validate the qw mechanism", {
  
})


context(" simulateWAY")
##====================================================================
test_that("it should return a data table", {

  version = 'slow'
  log <- Arguments$getVerbose(-8, timestamp=TRUE)

  ## 'nobs' observations
  nobs <- 10

  ## W(t)|Past(W(t))~N(0.5*Y(t-1)-0.25*A(t-1)+0.1*W(t-1), 1)
  ## A(t)|Past(A(t))~Ber(expit(-0.1+0.1*W(t)+0.25*Y(t-1)))
  ## Y(t)|Past(Y(t))~Ber(expit(0.1+0.1*A(t)+0.1*W(t)+0.05*Y(t-1)-0.01*A(t-1)))
  ## initializations: zero
  paramW <- c(0, 0.5, -0.25, 0.1)
  qw <- subject$generateMechanism(paramW, family="gaussian")
  paramA <- c(-0.1, 0.1, 0.25)
  ga <- subject$generateMechanism(paramA, family="bernoulli")
  paramY <- c(0.1, 0.1, 0.1, 0.05, -0.01)
  Qy <- subject$generateMechanism(paramY, family="bernoulli")

  data.dep <- subject$simulateWAY(numberOfBlocks = nobs, qw=qw, ga=ga, Qy=Qy, verbose=log, version=version)
  expect_is(data.dep, 'data.table')
})

test_that("it should be relatively fast", {
  log <- FALSE
  version = 'slow'

  comp.time <- NA
  tic <- Sys.time()
  ## 'nobs' observations
  nobs <- 100
  timelimit <- nobs / 10

  ## W(t)|Past(W(t))~N(0.5*Y(t-1)-0.25*A(t-1)+0.1*W(t-1), 1)
  ## A(t)|Past(A(t))~Ber(expit(-0.1+0.1*W(t)+0.25*Y(t-1)))
  ## Y(t)|Past(Y(t))~Ber(expit(0.1+0.1*A(t)+0.1*W(t)+0.05*Y(t-1)-0.01*A(t-1)))
  ## initializations: zero
  paramW <- c(0, 0.5, -0.25, 0.1)
  qw <- subject$generateMechanism(paramW, family="gaussian")
  paramA <- c(-0.1, 0.1, 0.25)
  ga <- subject$generateMechanism(paramA, family="bernoulli")
  paramY <- c(0.1, 0.1, 0.1, 0.05, -0.01)
  Qy <- subject$generateMechanism(paramY, family="bernoulli")
  data.dep <- subject$simulateWAY(numberOfBlocks = nobs, qw=qw, ga=ga, Qy=Qy, verbose=log, version=version)
  toc <- Sys.time()
  comp.time <- toc-tic
  expect_true(comp.time < timelimit)
})
