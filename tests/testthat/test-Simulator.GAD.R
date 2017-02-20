context("Simulator.GAD.R")
context(" simulateWAY")
nobs <- 4
sim <- Simulator.GAD$new()
result <- sim$simulateWAY(by = nobs)

test_that("it should return a dataframe with the correct columnames", {
  expect_equal(colnames(result), c('W1', 'A1', 'Y1'))
})

test_that("it should contain some non zero data", {
  expect_false(all(result == 0))
})

test_that("it should have the correct number of rows in the output", {
  expect_true(nrow(result) == nobs)
})

test_that("it should be tested properly", {
  log <- FALSE

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
