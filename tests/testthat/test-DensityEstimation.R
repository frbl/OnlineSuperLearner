context("DensityEstimation.R")
described.class <- DensityEstimation
#set.seed(12345)
#datO <- self$defaultDataTable
#self$fit(datO = datO)
#datO <- self$defaultDataTable
#self$predict(datO = datO)

# This is data for testing purposes only

suppressWarnings(devtools::load_all())
defaultDataTable = function() {
  nobs = 1000
  W = rbinom(n=nobs,size=1, prob=0.5)
  D = seq(nobs)
  mean = (1000 * W)
  Y=(rnorm(nobs,mean,10))
  data.table(D=D, W=W, Y = Y)
}

# TESTING:
rv.W <- RandomVariable$new(formula = W ~ D, family = 'binomial')
rv.Y <- RandomVariable$new(formula = Y ~ W, family = 'gaussian')



#nodeObjectsSub <- self$defineNodeObjects(datO = datO[1:40,], X = X, Y = Y)
#estimated_densities2 <- conditionalDensity$predictAeqa(newdata = nodeObjectsSub$datNetObs)
#setWvals <- c(W1 = 0)
#subs <- (datO$W1==setWvals["W1"])
#yValues <- yValues[subs]
#estimated_densities <- estimated_densities[subs]

context(' initialize')
test_that("it should initialize by default", {
  expect_error(described.class$new(), NA) 
})

test_that("it should throw if a wrong number of bins is specified", {
  expect_error(described.class$new(nbins = -1)) 
  expect_error(described.class$new(nbins = 0)) 
  lapply(seq(50), function(nbins) expect_error(described.class$new(nbins = nbins), NA)) 
})

test_that("it should floor the number of bins if it is a float", {
  subject <- described.class$new(nbins = 1.9) 
  expect_equal(subject$get_nbins, 1)
})

context(' sample')
test_that("it should throw if the cond densities have not yet been fitted", {
  subject <- described.class$new(nbins = 10) 
  expect_error(subject$sample(NULL), 'The conditional_densities need to be fit first!')
})

test_that("it should sample if from the cond densities once they've been fitted", {
  set.seed(12345) 
  subject <- described.class$new(nbins = 10) 

  # TODO: fix these warnings
  suppressWarnings(
    subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  )

  Y_val <- 0
  for(W_val in c(0,1)) {
    dat <- data.frame(D = 1, W=c(W_val), Y=Y_val)
    res <- subject$sample(dat)
    expect_true(is.a(res, 'list'))
    expect_true(length(res) == 2)
    expect_false(is.null(res$W))
    expect_false(is.null(res$Y))
  }
})

test_that("it should work with multiple entries in the data", {
 # TODO: Implement test 
})


context(' predict')
test_that("it should throw if the cond densities have not yet been fitted", {
  
})

test_that("it should predict from the cond densities once it has been fitted", {
  set.seed(12345) 
  subject <- described.class$new(nbins = 10) 

  # TODO: fix these warnings
  suppressWarnings(
    subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  )

  Y_val <- 0
  for(W_val in c(0,1)) {
    dat <- data.frame(D= 1, W=c(W_val), Y=Y_val)
    res <- subject$predict(dat, X=rv.Y$getX, Y=rv.Y$getY)
    expect_true(abs(res - W_val * 1000) < 50)
  }
})

test_that("it should predict from the cond densities once they've been fitted also with NA", {
  set.seed(12345) 
  subject <- described.class$new(nbins = 10) 

  # TODO: fix these warnings
  suppressWarnings(
    subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  )

  Y_val <- NA 
  for(W_val in c(0,1)) {
    dat <- data.frame(D= 1, W=c(W_val), Y=Y_val)
    suppressWarnings(res <- subject$predict(dat, X=rv.Y$getX, Y=rv.Y$getY))
    expect_true(abs(res - W_val * 1000) < 50)
  }
})

test_that("it should when sampled with NA, it should give a warning", {
  subject <- described.class$new(nbins = 3) 

  # TODO: fix these warnings
  suppressWarnings(
    subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  )

  W <- 1
  dat <- data.frame(W=c(W), Y=c(NA))
  expect_warning(subject$predict(dat, X=rv.Y$getX, Y=rv.Y$getY))
})

context(' process')
test_that("it should fit the conditional densities", {
  subject <- described.class$new(nbins = 3) 
  # TODO: fix these warnings
  suppressWarnings(subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y)))

  result <- subject$getConditionalDensities()
  expect_false(length(result) == 0)

  expect_equal(names(result), c(unname(rv.W$getY), unname(rv.Y$getY)))
})

test_that("it should throw when the list provided does not consist of randomvariables", {
  subject <- described.class$new(nbins = 3) 
  expect_error(subject$process(defaultDataTable(), randomVariables = c(rv.W, 'not-an-rv!')),
               'Please provide a list of randomvariables when running this function')
  
})

context(' update')

context(' getConditionalDensities')
test_that("it should throw if the densities were not yet fitted", {
  subject <- described.class$new(nbins = 3) 
  expect_error(length(subject$getConditionalDensities()) == 0, 'Densities not yet fitted')
})

test_that("it should return all CDs when no outcome is provided", {
  subject <- described.class$new(nbins = 3) 
  # TODO: fix these warnings
  suppressWarnings(subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y)))

  result <- subject$getConditionalDensities()
  expect_false(length(result) == 0)
  expect_equal(names(result), c(unname(rv.W$getY), unname(rv.Y$getY)))
})

test_that("it should provide just the CD with a given name when an outcome is provided", {
  subject <- described.class$new(nbins = 3) 
  # TODO: fix these warnings
  suppressWarnings(subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y)))

  result <- subject$getConditionalDensities(outcome = rv.Y$getY)
  expect_true(is.a(result, 'SummariesModel'))
  expect_equal(result$outvar, rv.Y$getY)
})

test_that("it should throw whenever a CD is provided as outcome that has not been fitted", {
  subject <- described.class$new(nbins = 3) 
  # TODO: fix these warnings
  suppressWarnings(subject$process(defaultDataTable(), randomVariables = c(rv.W, rv.Y)))

  expect_error(subject$getConditionalDensities(outcome = 'this-should-never-exist'),
               'this-should-never-exist is not a fitted outcome')
  
})

