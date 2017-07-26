context("OutputPlotGenerator.R")

defaultDataTable = function() {
  nobs = 10000
  W = rbinom(n=nobs,size=1, prob=0.75)
  D = seq(nobs)
  mean = (1000 * W) + 1
  Y=(rnorm(nobs,mean,10))
  data.table(D=D, W=W, Y = Y)
}

test_that("it should generate the correct plots", {
  set.seed(12345)
  dat <- defaultDataTable()
  est <- rep(0.24,length(dat$W))
  est[dat$W == 1] <- 0.76
  dir <- '~/tmp/osl/'

  print(length(est))
  print(length(dat$W))
  result <- OutputPlotGenerator.create_density_plot(yValues = dat$W,
                                          est,
                                          output = 'test-W-density',
                                          dir = dir
                                          )
  expect_true(file.exists(dir))
  expect_true(file.exists(result))
})

