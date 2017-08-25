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

  result <- OutputPlotGenerator.create_density_plot(yValues = dat$W,
                                          est,
                                          output = 'test-W-density',
                                          dir = dir
                                          )
  expect_true(file.exists(dir))
  expect_true(file.exists(result))
})

context(" OutputPlotGenerator.get_colors")
test_that("it should return the correct number of colors", {
  for (i in seq(1,12)) {
    result <- OutputPlotGenerator.get_colors(i)
    expect_equal(length(result), i)
    all_not_empty <- lapply(seq(i), function(j){
      result[[j]] != '' && !is.null(result[[j]]) 
    }) %>% unlist
    expect_true(all(all_not_empty))
  }
})

test_that("it should return null if the number is too high", {
  result <- OutputPlotGenerator.get_colors(13)
  expect_true(is.null(result))
})

test_that("it should return null if the number is too low", {
  result <- OutputPlotGenerator.get_colors(-1)
  expect_true(is.null(result))
  result <- OutputPlotGenerator.get_colors(0)
  expect_true(is.null(result))
})

context(" OutputPlotGenerator.get_simple_colors")
test_that("it should return the correct number of colors", {
  for (i in seq(1,12)) {
    result <- OutputPlotGenerator.get_simple_colors(i)
    expect_equal(length(result), i)
    all_not_empty <- lapply(seq(i), function(j){
      result[[j]] != '' && !is.null(result[[j]]) 
    }) %>% unlist
    expect_true(all(all_not_empty))
  }
})

test_that("it should return null if the number is too low", {
  result <- OutputPlotGenerator.get_simple_colors(-1)
  expect_true(is.null(result))
  result <- OutputPlotGenerator.get_simple_colors(0)
  expect_true(is.null(result))
})
