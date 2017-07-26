context("DensityEstimation.R")
described.class <- DensityEstimation
#set.seed(12345)
#datO <- self$defaultDataTable
#self$fit(datO = datO)
#datO <- self$defaultDataTable
#self$predict(datO = datO)

# This is data for testing purposes only
W_prob = 0.5
defaultDataTable = function(nobs = 5000, w_prob = W_prob) {
  W = rbinom(n=nobs,size=1, prob=w_prob)
  D = seq(nobs)
  mean = 10 + (15 * W) 
  Y=(rnorm(nobs,mean,5))
  data.table(D=D, W=W, Y = Y)
}

otherDefaultDataTable = function() {
  nobs = 10
  W = rbinom(n=nobs,size=1, prob=0.1)
  D = rnorm(nobs,100,100)
  mean = (1000 * W)
  Y=(rnorm(nobs,mean,1))
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

context(' predict')
test_that("it should throw if the provided data is not a datatable", {
  subject <- described.class$new(nbins = 10)
  expect_error(subject$predict(NULL), "Argument 'data' is neither of nor inherits class data.table: NULL")
})

test_that("it should throw if the provided plot argument is not a boolean", {
  subject <- described.class$new(nbins = 10)
  expect_error(subject$predict(data = defaultDataTable(), plot = 10), 
               "Argument 'plot' is non-logical: numeric", fixed=TRUE)
})

test_that("it should throw if the cond densities have not yet been fitted", {
  subject <- described.class$new(nbins = 10)
  expect_error(subject$predict(defaultDataTable()), 'The conditional_densities need to be fit first!')
})

context(' > sample')
test_that("it should sample from the cond densities once they've been fitted", {
  set.seed(12345)
  subject <- described.class$new(nbins = 30)

  # TODO: fix these warnings
  # suppressWarnings(
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  # )

  nobs <- 13000

  my_w_prob <- 0.5
  dat <- defaultDataTable(nobs=nobs, w_prob=my_w_prob)
  #Y_val <- 0
  #for(W_val in c(0,1)) {
    #dat <- data.table(D = 1, W=c(W_val), Y=Y_val)
  res <- subject$predict(dat, sample=TRUE)
  #print(res)
  expect_true(is.a(res, 'list'))
  expect_true(length(res) == 2)
  expect_false(is.null(res$W))
  expect_false(is.null(res$Y))

  dat$Y[dat$W==0] %>% mean %>% print
  dat$Y[dat$W==1] %>% mean %>% print
  

  res$Y[dat$W==0] %>% mean %>% print
  res$Y[dat$W==1] %>% mean %>% print
  #}
})

test_that("it should sample from the cond densities once they've been fitted also with NA", {
  # In this test we check what would happen if we'd want to *sample* Y given some predefined
  # W. In this case, we provide Y as NA, and it should not crash.
  set.seed(1234)
  subject <- described.class$new(nbins = 10)

  # TODO: fix these warnings
  # suppressWarnings(
    subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  # )
  accepted_error <- 20
  Y_val <- NA
  for(W_val in c(0,1)) {
    dat <- data.table(D = 1, W = c(W_val), Y = Y_val)
    # suppressWarnings()
    res <- subject$predict(dat, sample = TRUE)
    expect_true(abs(res$Y - W_val * 15) < accepted_error)
  }
})

#test_that("it should, when sampled with NA, give a warning", {
  #subject <- described.class$new(nbins = 3)

  ## TODO: fix these warnings
  #suppressWarnings(
    #subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  #)

  #W <- 1
  #dat <- data.table(D = 1, W=c(W), Y=c(NA))
  #expect_warning(subject$predict(dat, sample = TRUE))
#})

context(' > predict')
test_that("it should get the correct probabilities from the cond densities", {
  set.seed(12345)
  subject <- described.class$new(nbins = 13)

  # TODO: fix these warnings
  # suppressWarnings(
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  # )

  n <- 4
  Y_val <- 1
  # In this configuration, the chance of W should be approx .5 (which it is)
  # The probability of Y should be approx 0, because it should be 1000 here
  #dat <- data.table(D = rep(1,n), W=rep(seq(0,1), n/2), Y=c(rep(Y_val, n/2), rep(Y_val + 1000, n/2)))
  nobs <- 13000

  my_w_prob <- 0.5
  dat <- defaultDataTable(nobs=nobs, w_prob=my_w_prob)
  res <- subject$predict(dat, sample=FALSE, plot = TRUE)

  expect_true(is.a(res, 'list'))
  expect_true(length(res) == 2)

  expect_false(is.null(res$W))
  expect_false(is.null(res$Y))

  expect_equal(length(res$Y), nobs)
  expect_equal(length(res$W), nobs)

  # W
  expect_true(abs(mean(res$W[dat$W==1]) - W_prob) < 0.05)

  # Y
  print('High!')

  OutputPlotGenerator.create_density_plot(yValues = dat$Y[dat$W==1],
                                          res$Y[dat$W==1],
                                          output = 'test-W-density1'
                                          )
  OutputPlotGenerator.create_density_plot(yValues = dat$Y[dat$W==0],
                                          res$Y[dat$W==0],
                                          output = 'test-W-density0'
                                          )

  # TODO: This is not working, but should probably work:
  #expect_true(mean(res$Y[dat$W == 1 & dat$Y >= 17]) > mean(res$Y[dat$W == 0 & dat$Y < 17]))
  #expect_true(mean(res$Y[dat$W == 0 & dat$Y >= 17]) < mean(res$Y[dat$W == 1 & dat$Y < 17]))

  #print(mean(res$Y[dat$W == 1 & dat$Y >= 17]))
  #print(mean(res$Y[dat$W == 1 & dat$Y < 17]))
  #print(mean(res$Y[dat$W == 0 & dat$Y >= 17]))
  #print(mean(res$Y[dat$W == 0 & dat$Y < 17]))

})

test_that("cond density predictions should work for only one row of data", {
  set.seed(12345)
  subject <- described.class$new(nbins = 10)
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  n <- 1
  Y_val <- 1000
  dat <- data.table(D = rep(1,n), W=seq(n), Y=rep(Y_val, n))
  res <- subject$predict(dat, sample=FALSE)
  expect_true(is.a(res, 'list'))
  expect_true(length(res) == 2)
  expect_false(is.null(res$W))
  expect_false(is.null(res$Y))
  expect_equal(length(res$Y), n)
  expect_equal(length(res$W), n)
})

test_that("it should throw if no output column is provided in the data", {
  set.seed(12345)
  subject <- described.class$new(nbins = 10)
  expected_error <- 'In order to predict the probability of an outcome, we also need the outcome'

  # TODO: fix these warnings
  # suppressWarnings()
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))

  n <- 3
  dat <- data.table(D = rep(1,n), W=seq(n))
  res <- expect_error(subject$predict(dat, sample = FALSE), expected_error, fixed = TRUE)
})

context(' fit')
test_that("it should fit the conditional densities", {
  subject <- described.class$new(nbins = 3)
  # TODO: fix these warnings
  # suppressWarnings()
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))

  result <- subject$getConditionalDensities()
  expect_false(length(result) == 0)

  expect_equal(names(result), c(unname(rv.W$getY), unname(rv.Y$getY)))
})

test_that("it should throw when the list provided does not consist of randomvariables", {
  subject <- described.class$new(nbins = 3)
  expect_error(subject$fit(defaultDataTable(), randomVariables = c(rv.W, 'not-an-rv!')),
               "Argument 'rv' is neither of nor inherits class RandomVariable: character", fixed=TRUE)

})

context(' update')
test_that("it should update existing estimators with new data and still sets the correct names", {
  subject <- described.class$new(nbins = 20)

  subject$fit(otherDefaultDataTable(), randomVariables = c(rv.W, rv.Y))
  result_pre <- copy(subject$getConditionalDensities())
  result_pre <- result_pre[[1]]$getPsAsW.models()[[1]]$getfit$coef

  subject$update(defaultDataTable())
  result_post <- copy(subject$getConditionalDensities())
  result_post <- result_post[[1]]$getPsAsW.models()[[1]]$getfit$coef

  for (i in seq_along(result_pre)) {
    # Test if in fact all entries have been updated
    expect_false(equals(result_pre[[i]], result_post[[i]]))
  }
})


context(' getConditionalDensities')
test_that("it should throw if the densities were not yet fitted", {
  subject <- described.class$new(nbins = 3)
  expect_error(length(subject$getConditionalDensities()) == 0, 'Densities not yet fitted')
})

test_that("it should return all CDs when no outcome is provided", {
  subject <- described.class$new(nbins = 3)
  # TODO: fix these warnings
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  result <- subject$getConditionalDensities()
  expect_false(length(result) == 0)
  expect_equal(names(result), c(unname(rv.W$getY), unname(rv.Y$getY)))
})

test_that("it should provide just the CD with a given name when an outcome is provided", {
  subject <- described.class$new(nbins = 3)
  # TODO: fix these warnings
  # suppressWarnings()
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  result <- subject$getConditionalDensities(outcome = rv.Y$getY)
  expect_true(is.a(result, 'SummariesModel'))
  expect_equal(result$outvar, rv.Y$getY)
})

test_that("it should throw whenever a CD is provided as outcome that has not been fitted", {
  subject <- described.class$new(nbins = 3)
  # TODO: fix these warnings
  # suppressWarnings()
  subject$fit(defaultDataTable(), randomVariables = c(rv.W, rv.Y))
  expect_error(subject$getConditionalDensities(outcome = 'this-should-never-exist'),
               'this-should-never-exist is not a fitted outcome')

})

context(' Static methods')
context(' > DensityEstimation.are_all_estimators_online')
test_that("it should return true if a whole list of estimators is online", {
  SL.Library <- list()
  SL.Library <- append(SL.Library, list( described.class$new(nbins = 3, online=TRUE)))
  SL.Library <- append(SL.Library, list( described.class$new(nbins = 3, online=TRUE)))
  SL.Library <- append(SL.Library, list( described.class$new(nbins = 3, online=TRUE)))
  expect_true(DensityEstimation.are_all_estimators_online(SL.Library))
})

test_that("it should return false if any estimator is not online", {
  SL.Library <- list()
  SL.Library <- append(SL.Library, list( described.class$new(nbins = 3, online=TRUE)))
  SL.Library <- append(SL.Library, list( described.class$new(nbins = 3, online=FALSE)))
  SL.Library <- append(SL.Library, list( described.class$new(nbins = 3, online=TRUE)))
  expect_false(DensityEstimation.are_all_estimators_online(SL.Library))
})

