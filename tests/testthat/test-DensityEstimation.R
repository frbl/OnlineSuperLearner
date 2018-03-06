context("DensityEstimation.R")
described.class <- DensityEstimation
#set.seed(12345)
#datO <- self$defaultDataTable
#self$fit(datO = datO)
#datO <- self$defaultDataTable
#self$predict(datO = datO)

# This is data for testing purposes only
W_prob = 0.5
Nobs = 5000
defaultDataTable = function(nobs = Nobs, w_prob = W_prob) {
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
rv.W <- RelevantVariable$new(formula = W ~ D, family = 'binomial')
rv.Y <- RelevantVariable$new(formula = Y ~ W, family = 'gaussian')


#nodeObjectsSub <- self$defineNodeObjects(datO = datO[1:40,], X = X, Y = Y)
#estimated_densities2 <- conditionalDensity$predictAeqa(newdata = nodeObjectsSub$datNetObs)
#setWvals <- c(W1 = 0)
#subs <- (datO$W1==setWvals["W1"])
#yValues <- yValues[subs]
#estimated_densities <- estimated_densities[subs]

context(' initialize')
#==========================================================
test_that("it should initialize by default", {
  expect_error(described.class$new(), NA)
})

test_that("it should throw if a wrong value for online is specified", {
  expect_error(described.class$new(online = 'test'),"Argument 'online' is non-logical: character", fixed=TRUE)
})

test_that("it should throw if a wrong value for verbose is specified", {
  expect_error(described.class$new(verbose = 'test'), "Argument 'verbose' is non-logical: character", fixed=TRUE)
})

test_that("it should throw if a wrong number of bins is specified", {
  expect_error(described.class$new(nbins = -1))
  expect_error(described.class$new(nbins = 0))
  lapply(seq(10), function(nbins) expect_error(described.class$new(nbins = nbins), NA))
})

test_that("it should floor the number of bins if it is a float", {
  subject <- described.class$new(nbins = 1.9)
  expect_equal(subject$get_nbins, 1)
})

test_that("it should set a bin estimator by default", {
  subject <- described.class$new(bin_estimator = NULL)
  expect_false(is.null(subject$get_bin_estimator))
  expect_is(subject$get_bin_estimator, 'glmR6')
})

test_that("it should initialize the conditional densities with a list", {
  subject <- described.class$new()
  expect_false(is.null(subject$get_raw_conditional_densities))
  expect_is(subject$get_raw_conditional_densities, 'list')
})

test_that("it should set a name by default", {
  subject <- described.class$new()
  expect_false(is.null(subject$get_name))
  expect_equal(subject$get_name, 'default')
})

test_that("it should set the provided name", {
  subject <- described.class$new(name = 'test')
  expect_false(is.null(subject$get_name))
  expect_equal(subject$get_name, 'test')
})

context(' predict')
#==========================================================
test_that("it should throw if the provided data is not a datatable and check is true", {
  subject <- described.class$new()
  expect_error(subject$predict(NULL, check = TRUE), "Argument 'data' is neither of nor inherits class data.table: NULL")
})

test_that("it should throw if the provided plot argument is not a boolean", {
  subject <- described.class$new()
  expect_error(subject$predict(data = defaultDataTable(), plot = 10, check = TRUE), 
               "Argument 'plot' is non-logical: numeric", fixed=TRUE)
})

test_that("it should throw if the cond densities have not yet been fitted", {
  subject <- described.class$new()
  expect_error(subject$predict(defaultDataTable(), check = TRUE), 
               'The conditional_densities need to be fit first!')
})

test_that("it should call the predict function if sample is false, with the correct arguments", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.plot <- FALSE
  cur.check <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  stub(subject$predict, 'self$predict_probability', 
    function(datO, X, Y, plot, check) {
      expect_equal(datO, cur.data)
      expect_equal(X, cur.relevantVariables[[iter]]$getX)
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      expect_equal(plot, cur.plot)
      expect_equal(check, cur.check)
      iter <<- iter + 1
    }
  )

  iter <<- 1
  subject$predict(cur.data, sample = FALSE, subset = NULL, plot = cur.plot, check = cur.check)
})

test_that("it should use a subset of the relevant variables for predicting if a subset is provided", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.plot <- FALSE
  cur.check <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  stub(subject$predict, 'self$predict_probability', 
    function(datO, X, Y, plot, check) {
      expect_equal(datO, cur.data)
      expect_equal(X, cur.relevantVariables[[1]]$getX)
      expect_equal(Y, cur.relevantVariables[[1]]$getY)
      expect_equal(plot, cur.plot)
      expect_equal(check, cur.check)
    }
  )
  subject$predict(cur.data, sample = FALSE, subset = cur.relevantVariables[[1]]$getY, plot = cur.plot, check = cur.check)

  stub(subject$predict, 'self$predict_probability', 
    function(datO, X, Y, plot, check) {
      expect_equal(datO, cur.data)
      expect_equal(X, cur.relevantVariables[[2]]$getX)
      expect_equal(Y, cur.relevantVariables[[2]]$getY)
      expect_equal(plot, cur.plot)
      expect_equal(check, cur.check)
    }
  )
  subject$predict(cur.data, sample = FALSE, subset = cur.relevantVariables[[2]]$getY, plot = cur.plot, check = cur.check)
})

test_that("it should call the sample function if sample is true, with the correct arguments", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  stub(subject$predict, 'self$sample', 
    function(datO, X, Y, plot) {
      expect_equal(datO, cur.data)
      expect_equal(X, cur.relevantVariables[[iter]]$getX)
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      expect_equal(plot, cur.plot)
      iter <<- iter + 1
    }
  )

  iter <<- 1
  subject$predict(cur.data, sample = TRUE, subset = NULL, plot = cur.plot, check = FALSE)
})

test_that("it should use a subset of the relevant variables for sampling if a subset is provided", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  stub(subject$predict, 'self$sample', 
    function(datO, X, Y, plot) {
      expect_equal(datO, cur.data)
      expect_equal(X, cur.relevantVariables[[1]]$getX)
      expect_equal(Y, cur.relevantVariables[[1]]$getY)
      expect_equal(plot, cur.plot)
    }
  )
  subject$predict(cur.data, sample = TRUE, subset = cur.relevantVariables[[1]]$getY, plot = cur.plot, check = FALSE)

  stub(subject$predict, 'self$sample', 
    function(datO, X, Y, plot) {
      expect_equal(datO, cur.data)
      expect_equal(X, cur.relevantVariables[[2]]$getX)
      expect_equal(Y, cur.relevantVariables[[2]]$getY)
      expect_equal(plot, cur.plot)
    }
  )
  subject$predict(cur.data, sample = TRUE, subset = cur.relevantVariables[[2]]$getY, plot = cur.plot, check = FALSE)
})


test_that("it should return a list with the correct names", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.plot <- FALSE
  cur.check <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  stub(subject$predict, 'self$predict_probability', 
    function(datO, X, Y, plot, check) {
      expect_equal(datO, cur.data)
      expect_equal(X, cur.relevantVariables[[iter]]$getX)
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      expect_equal(plot, cur.plot)
      expect_equal(check, cur.check)
      iter <<- iter + 1
      return(123)
    }
  )

  iter <<- 1
  result <- subject$predict(cur.data, sample = FALSE, subset = NULL, plot = cur.plot, check = FALSE)
  expect_is(result, 'list')
  expect_length(result, length(cur.relevantVariables))
  expect_named(result, unname(c(rv.W$getY, rv.Y$getY)))
})

test_that("integration - it should get the correct probabilities from the cond densities", {
  set.seed(12345)
  subject <- described.class$new()

  ## Really fit a density
  subject$fit(defaultDataTable(), relevantVariables = c(rv.W, rv.Y))
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
  #OutputPlotGenerator.create_density_plot(yValues = dat$Y[dat$W==1],
                                          #res$Y[dat$W==1],
                                          #output = 'test-W-density1'
                                          #)
  #OutputPlotGenerator.create_density_plot(yValues = dat$Y[dat$W==0],
                                          #res$Y[dat$W==0],
                                          #output = 'test-W-density0'
                                          #)

  expect_lt(mean(res$Y[dat$W == 0 & dat$Y >= 17]), mean(res$Y[dat$W == 0 & dat$Y < 17]))
  expect_lt(mean(res$Y[dat$W == 1 & dat$Y < 17]),  mean(res$Y[dat$W == 1 & dat$Y >= 17]))

  #print(mean(res$Y[dat$W == 1 & dat$Y < 17])) 
  #print(mean(res$Y[dat$W == 1 & dat$Y >= 17]))
  #print(mean(res$Y[dat$W == 0 & dat$Y >= 17]))
  #print(mean(res$Y[dat$W == 0 & dat$Y < 17]))

})


context(' predict_probability')
#==========================================================

test_that("it should call the condensier package with the correct data", {
  subject <- described.class$new()
  cur.rv <- rv.W
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = c(cur.rv))

  mock_cond_density <- mock(123)

  stub(subject$predict_probability, 'self$getConditionalDensities', 
    function(Y) {
      expect_equal(Y, cur.rv$getY)
      return(mock_cond_density)
    }
  )

  with_mock(`condensier::predict_probability` = mock_cond_density,
    subject$predict_probability(cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
  )

  expect_called(mock_cond_density, 1)

  args <- mock_args(mock_cond_density)[[1]]
  expect_length(args, 2)
  expect_equal(args$model_fit, mock_cond_density)
  expect_equal(args$newdata, cur.data)
})


test_that("it should call the plotting package according to the plot setting", {
  subject <- described.class$new()
  cur.rv <- rv.W
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = c(cur.rv))

  mock_cond_density <- mock(123, cycle = TRUE)
  mock_output_plot_generator <- mock(321)

  stub(subject$predict_probability, 'self$getConditionalDensities', 
    function(Y) {
      expect_equal(Y, cur.rv$getY)
      return(mock_cond_density)
    }
  )

  with_mock(OutputPlotGenerator.create_density_plot = mock_output_plot_generator,
    with_mock(`condensier::predict_probability` = mock_cond_density,
      subject$predict_probability(cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
    )
  )

  expect_called(mock_output_plot_generator, 0)
  
  cur.plot <- TRUE
  with_mock(OutputPlotGenerator.create_density_plot = mock_output_plot_generator,
    with_mock(`condensier::predict_probability` = mock_cond_density,
      subject$predict_probability(cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
    )
  )

  expect_called(mock_output_plot_generator, 1)

  args <- mock_args(mock_output_plot_generator)[[1]]
  expect_equal(args$yValues, cur.data[[cur.rv$getY]])
  expect_equal(args$estimated_probabilities, mock_cond_density())
  expect_equal(args$output, paste(subject$get_name, cur.rv$getY))
})

test_that("it should return the estimated probabilities", {
  subject <- described.class$new()
  cur.rv <- rv.W
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = c(cur.rv))

  ## The cycle is true because we need it in the expect at the end
  mock_cond_density <- mock(123, cycle=TRUE)

  stub(subject$predict_probability, 'self$getConditionalDensities', 
    function(Y) {
      expect_equal(Y, cur.rv$getY)
      return(mock_cond_density)
    }
  )

  with_mock(`condensier::predict_probability` = mock_cond_density,
    result <- subject$predict_probability(cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
  )

  expect_equal(result, mock_cond_density())
})

test_that("it should throw if the data.table is incomplete and check is true", {
  subject <- described.class$new()
  expected_error <- 'In order to predict the probability of an outcome, we also need the outcome'
  dat <- data.table(D = rep(1,10))
  res <- expect_error(subject$predict_probability(dat, X= 'D', Y= 'W', check= TRUE), expected_error, fixed = TRUE)
})


test_that("cond density predictions should work for only one row of data", {
  set.seed(12345)
  subject <- described.class$new(nbins = 10)
  subject$fit(defaultDataTable(), relevantVariables = c(rv.W, rv.Y))

  n <- 1
  Y_val <- 1000

  dat <- data.table(D = rep(1,n), W=seq(n), Y=rep(Y_val, n))
  res <- subject$predict_probability(dat, X = rv.W$getX, Y = rv.Y$getY)

  expect_is(res, 'numeric')
  expect_false(is.null(res))
  expect_equal(length(res), n)

  # This outcome should be very unlikely
  expect_lt(res, 0.0001)
})

context(' sample')
#==========================================================

test_that("it should call the condensier package with the correct data", {
  subject <- described.class$new()
  cur.rv <- rv.W
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = c(cur.rv))

  mock_cond_density <- mock(123)

  stub(subject$sample, 'self$getConditionalDensities', 
    function(Y) {
      expect_equal(Y, cur.rv$getY)
      return(mock_cond_density)
    }
  )

  with_mock(`condensier::sample_value` = mock_cond_density,
    subject$sample(datO = cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
  )

  expect_called(mock_cond_density, 1)

  args <- mock_args(mock_cond_density)[[1]]
  expect_length(args, 2)
  expect_equal(args$model_fit, mock_cond_density)
  expect_equal(args$newdata, cur.data)
})

test_that("it should call the plotting package according to the plot setting with the density function", {
  subject <- described.class$new()
  cur.rv <- rv.W
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = c(cur.rv))

  mock_sampled_values <- mock(seq(10), cycle = TRUE)
  mock_output_plot_generator <- mock(321)
  mock_density <- mock(list(x=321, y=1), cycle = TRUE)

  stub(subject$sample, 'self$getConditionalDensities', 
    function(Y) {
      expect_equal(Y, cur.rv$getY)
      return(mock_sampled_values)
    }
  )

  with_mock(OutputPlotGenerator.create_density_plot = mock_output_plot_generator,
    with_mock(`condensier::sample_value` = mock_sampled_values,
      subject$sample(cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
    )
  )

  expect_called(mock_output_plot_generator, 0)
  
  cur.plot <- TRUE
  with_mock(density = mock_density,
    with_mock(OutputPlotGenerator.create_density_plot = mock_output_plot_generator,
      with_mock(`condensier::sample_value` = mock_sampled_values,
        subject$sample(cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
      )
    )
  )

  expect_called(mock_density, 1)
  args <- mock_args(mock_density)[[1]]
  expect_equal(args$x, mock_sampled_values())
  expect_called(mock_output_plot_generator, 1)

  args <- mock_args(mock_output_plot_generator)[[1]]
  expect_equal(args$yValues, cur.data[[cur.rv$getY]])
  expect_equal(args$estimated_probabilities, mock_density()$y)
  expect_equal(args$estimated_y_values, mock_density()$x)
  expect_equal(args$output, paste('sampled', subject$get_name, cur.rv$getY, sep='-'))
})

test_that("it should return the sampled values", {
  subject <- described.class$new()
  cur.rv <- rv.W
  cur.plot <- FALSE
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = c(cur.rv))

  ## The cycle is true because we need it in the expect at the end
  ## Note that we inject the rnorm, so it is only ran once
  mock_sampled_values <- rnorm(100,0,1) %>% mock(., cycle=TRUE)

  stub(subject$sample, 'self$getConditionalDensities', 
    function(Y) {
      expect_equal(Y, cur.rv$getY)
      return(mock_sampled_values)
    }
  )

  with_mock(`condensier::sample_value` = mock_sampled_values,
    result <- subject$sample(cur.data, X = cur.rv$getX, Y = cur.rv$getY, plot = cur.plot)
  )

  expect_equal(result, mock_sampled_values())
})

test_that("it should throw if the data.table is incomplete and check is true", {
  subject <- described.class$new()
  expected_error <- 'Sampling from non conditional distribution is not yet supported!'
  dat <- data.table(D = rep(1,10))
  res <- expect_error(subject$sample(dat, X = c(), Y= 'W', check= TRUE),
                      expected_error, fixed = TRUE)
})


test_that("cond density predictions should work for only one row of data", {
  subject <- described.class$new(nbins = 10)
  subject$fit(defaultDataTable(), relevantVariables = c(rv.W, rv.Y))

  n <- 1

  dat <- defaultDataTable()[n,]
  res <- subject$sample(dat, X = rv.W$getX, Y = rv.Y$getY)

  expect_is(res, 'numeric')
  expect_false(is.null(res))
  expect_equal(length(res), n)
})

test_that("it should sample better than random", {
  set.seed(12345)
  subject <- described.class$new(nbins = 10)
  subject$fit(defaultDataTable(), relevantVariables = c(rv.W, rv.Y))
  n <- 500

  ## Create some completely not related data
  W = rbinom(n = Nobs, size = 1, prob = W_prob)
  D = seq(Nobs)
  Y = (rnorm(Nobs,10 + (15 * 1),5))
  random_dat = data.table(D = D, W = W, Y = Y)[n,]
  dat <- defaultDataTable()[n,]

  res <- subject$sample(dat, X = rv.W$getX, Y = rv.Y$getY)
  res_random <- subject$sample(random_dat, X = rv.W$getX, Y = rv.Y$getY)

  true_err <- abs(res - dat$Y)
  random_err <- abs(res - random_dat$Y)
  expect_lt(true_err, random_err)
})

test_that("integration - it should sample from the cond densities once they've been fitted", {
  set.seed(12345)
  subject <- described.class$new(nbins = 30)
  subject$fit(defaultDataTable(), relevantVariables = c(rv.W, rv.Y))

  nobs <- 13000

  my_w_prob <- 0.5
  dat <- defaultDataTable(nobs=nobs, w_prob=my_w_prob)
  res <- subject$sample(dat, X = rv.Y$getX, Y = rv.Y$getY)

  expect_true(is.a(res, 'numeric'))
  expect_false(is.null(res))
  expect_true(length(res) == nobs)

  ## 0.5 is a random number. The thing is that the difference should be small
  expect_lt(abs(dat$Y[dat$W==0] %>% mean - res[dat$W==0] %>% mean), 0.5)
  expect_lt(abs(dat$Y[dat$W==1] %>% mean - res[dat$W==1] %>% mean), 0.5)
})

test_that("integration - it should sample from the cond densities once they've been fitted also with NA", {
  ## In this test we check what would happen if we'd want to *sample* Y given some predefined
  ## W. In this case, we provide Y as NA, and it should not crash.
  set.seed(12345)
  subject <- described.class$new(nbins = 30)
  dat <- defaultDataTable(nobs=10000)

  subject$fit(dat, relevantVariables = c(rv.W, rv.Y))
  accepted_error <- 1 
  Y_val <- NA

  for(W_val in c(0,1)) {
    nobs <- 50
    dat <- data.table(D = rep(1, nobs), W = rep(c(W_val), nobs), Y = rep(Y_val, nobs))
    res <- subject$sample(dat, X = rv.Y$getX, Y = rv.Y$getY)
    true_mean <- 10 + (15 * W_val) 
    my_mean <- mean(res)

    expect_lt(abs(my_mean - true_mean), accepted_error)
  }
})

context(' fit')
#==========================================================
test_that("it should store the relevant variables the first time", {
  subject <- described.class$new(nbins = 3)
  cur.relevantVariables <- c(rv.W, rv.Y)
  expect_null(subject$get_relevant_variables)

  stub(subject$fit, 'self$fit_single_rv', function(...) {} )
  stub(subject$fit, 'self$set_relevant_variables', 
    function(relevantVariables) { 
      expect_equal(relevantVariables, cur.relevantVariables)
    }
  )

  subject$fit(defaultDataTable(), relevantVariables = cur.relevantVariables )
  stub(subject$fit, 'self$set_relevant_variables', 
    function(...) { stop('It should only call this function once!') }
  )
  stub(subject$fit, 'self$get_relevant_variables', 
    function(...) { list('something, not nil!') }
  )
  expect_error(subject$fit(defaultDataTable(), relevantVariables = cur.relevantVariables ), NA)
})

test_that("it should call the fit_single_rv with the correct parameters", {
  subject <- described.class$new(nbins = 3)
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.dat <- defaultDataTable()

  stub(subject$fit, 'self$fit_single_rv', 
    function(datO, X, Y, family) {
      expect_equal(datO, cur.dat)
      expect_equal(X, cur.relevantVariables[[iter]]$getX)
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      expect_equal(family, cur.relevantVariables[[iter]]$getFamily)
      iter <<- iter + 1
      mock()
    }
  )

  stub(subject$fit, 'private$store_conditional_density', function(...) {})

  iter <<- 1
  subject$fit(cur.dat, relevantVariables = cur.relevantVariables)
})

test_that("it should store the fitted density in the list of conditional densities", {
  subject <- described.class$new(nbins = 3)
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.dat <- defaultDataTable()

  mocks <- list(mock(1), mock(2))
  stub(subject$fit, 'self$fit_single_rv', function(datO, X, Y, family) { mocks[[iter]] })

  stub(subject$fit, 'private$store_conditional_density', 
    function(Y, density) {
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      expect_equal(density, mocks[[iter]])
      iter <<- iter + 1
    }
  )

  iter <<- 1
  subject$fit(cur.dat, relevantVariables = cur.relevantVariables)
})


context(' fit_single_rv')
#==========================================================
test_that("it should only use 2 bins when the rv family is binomial", {
  nbins <- 200
  subject <- described.class$new(nbins = nbins)

  mock_cond_density <- mock(123)
  with_mock(`condensier::fit_density` = mock_cond_density,
    subject$fit_single_rv(defaultDataTable(), X = rv.W$X, Y = rv.W$Y, family = 'binomial')
  )

  expect_called(mock_cond_density, 1)
  args <- mock_args(mock_cond_density)[[1]]
  expect_false(equals(args$nbins, nbins))
  expect_equal(args$nbins, 2)
})

test_that("it should call the condensier to fit the conditional densities", {
  cur.nbins <- 200
  subject <- described.class$new(nbins = cur.nbins)
  cur.data <- defaultDataTable()

  mock_cond_density <- mock(123)
  with_mock(`condensier::fit_density` = mock_cond_density,
    subject$fit_single_rv(cur.data, X = rv.W$getX, Y = rv.W$getY, family = 'gaussian')
  )

  expect_called(mock_cond_density, 1)
  args <- mock_args(mock_cond_density)[[1]]

  expect_false(is.null(args$nbins))
  expect_equal(args$nbins, cur.nbins)

  expect_false(is.null(args$X))
  expect_equal(args$X, rv.W$getX)

  expect_false(is.null(args$Y))
  expect_equal(args$Y, rv.W$getY)

  expect_false(is.null(args$bin_estimator))
  expect_equal(args$bin_estimator, subject$get_bin_estimator)

  expect_false(is.null(args$input_data))
  expect_equal(args$input_data, cur.data)
})

test_that("it should return the fit of the density", {
  cur.nbins <- 200
  subject <- described.class$new(nbins = cur.nbins)
  cur.data <- defaultDataTable()

  mock_cond_density <- mock(123, cycle = TRUE)
  with_mock(`condensier::fit_density` = mock_cond_density,
    result <- subject$fit_single_rv(cur.data, X = rv.W$getX, Y = rv.W$getY, family = 'gaussian')
  )

  expect_equal(result, mock_cond_density())
 
})

context(' set_relevant_variables')
#==========================================================
test_that("it should throw when the list provided does not consist of relevantvariables", {
  subject <- described.class$new(nbins = 3)
  expect_error(subject$fit(defaultDataTable(), relevantVariables = c(rv.W, 'not-an-rv!')),
               "Argument 'rv' is neither of nor inherits class RelevantVariable: character", fixed=TRUE)

})

test_that("it should store the relevantvariables under their corresponding outcome variable", {
  cur.relevantVariables <- c(rv.W, rv.Y)
  subject <- described.class$new(nbins = 3)
  subject$fit(defaultDataTable(), relevantVariables = cur.relevantVariables)

  rv_names <- lapply(cur.relevantVariables, function(rv) rv$getY) %>% unlist %>% unname
  expect_named(subject$get_relevant_variables, rv_names)
})

context(' update')
#==========================================================
test_that("it should update existing estimators with new data and still sets the correct names", {
  subject <- described.class$new(nbins = 20)

  subject$fit(otherDefaultDataTable(), relevantVariables = c(rv.W, rv.Y))
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

test_that("it should call the update function for each of the densities", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  mock_data_store <- mock('data_store', cycle = TRUE)
  mock_update <- mock('update_density_function', cycle = TRUE)
  mock_density <- list(update = mock_update, cycle = TRUE)

  stub(subject$update, "private$create_data_store",
    function(newdata, Y, X) {
      expect_equal(newdata, cur.data)
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      expect_equal(X, cur.relevantVariables[[iter]]$getX)
      mock_data_store
    } 
  )

  stub(subject$update, 'self$getConditionalDensities', 
    function(Y) {
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      iter <<- iter + 1
      return(mock_density)
    }
  )

  stub(subject$update, 'private$store_conditional_density', function(...) {})

  iter <<- 1
  subject$update(cur.data)
  
})

test_that("it should store the updated densities", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  mock_data_store <- mock('data_store', cycle = TRUE)
  mock_update <- mock('update_density_function', cycle = TRUE)
  mock_density <- list(update = function(newdata) { mock_update })

  stub(subject$update, "private$create_data_store",
    function(newdata, Y, X) { mock_data_store } 
  )

  stub(subject$update, 'self$getConditionalDensities', 
    function(Y) { return(mock_density) }
  )

  stub(subject$update, 'private$store_conditional_density', 
    function(Y, density) {
      expect_equal(Y, cur.relevantVariables[[iter]]$getY)
      expect_equal(density, mock_update)
      iter <<- iter + 1
    }
  )

  iter <<- 1
  subject$update(cur.data)
})

test_that("it should return true", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)
  cur.data <- defaultDataTable()
  subject$set_relevant_variables(relevantVariables = cur.relevantVariables)

  mock_data_store <- mock('data_store', cycle = TRUE)
  mock_update <- mock('update_density_function', cycle = TRUE)
  mock_density <- list(update = function(newdata) { mock_update })

  stub(subject$update, "private$create_data_store",
    function(newdata, Y, X) { return(mock_data_store) } 
  )

  stub(subject$update, 'self$getConditionalDensities', 
    function(Y) { return(mock_density) }
  )

  stub(subject$update, 'private$store_conditional_density', 
    function(Y, density) { }
  )
  result <- subject$update(cur.data)
  expect_true(result)
})


context(' getConditionalDensities')
#==========================================================
test_that("it should throw if the densities were not yet fitted", {
  subject <- described.class$new()
  expect_error(subject$getConditionalDensities() , 'Densities not yet fitted')
})

test_that("it should return all CDs when no outcome is provided", {
  subject <- described.class$new()
  cur.relevantVariables <- c(rv.W, rv.Y)

  ## Use the fit function here, so we don't have to mock the private function
  subject$fit(defaultDataTable(), relevantVariables = cur.relevantVariables)

  result <- subject$getConditionalDensities()
  expect_false(length(result) == 0)
  expect_is(result, 'list')
  expect_named(result, unname(unlist(lapply(cur.relevantVariables, function(rv) rv$getY))))
})

test_that("it should provide just the CD with a given name when an outcome is provided", {
  subject <- described.class$new(nbins = 3)
  cur.relevantVariables <- c(rv.W, rv.Y)

  ## Use the fit function here, so we don't have to mock the private function
  subject$fit(defaultDataTable(), relevantVariables = cur.relevantVariables)
  result <- subject$getConditionalDensities(outcome = rv.Y$getY)
  expect_true(is.a(result, 'SummariesModel'))
  expect_equal(result$outvar, rv.Y$getY)
})

test_that("it should throw whenever a CD is provided as outcome that has not been fitted", {
  subject <- described.class$new(nbins = 3)

  stub(subject$getConditionalDensities, 'private$conditional_densities', 
    function(Y) { return(mock_density) }
  )

  subject$fit(defaultDataTable(), relevantVariables = c(rv.W, rv.Y))
  expect_error(subject$getConditionalDensities(outcome = 'this-should-never-exist'),
               'this-should-never-exist is not a fitted outcome')
})


context(' set_name')
#==========================================================
test_that("it should set the new name", {
  subject <- described.class$new()
  new_name <- 'the_new_name'
  subject$set_name(new_name)
  expect_equal(subject$get_name, new_name)
})

test_that("it should throw if the name is not correct", {
  subject <- described.class$new()
  new_name <- glm
  expect_error(subject$set_name(new_name), 
               "cannot coerce type 'closure' to vector of type 'character'", fixed = TRUE)
})


context(' Static methods')
#==========================================================
context(' > DensityEstimation.are_all_estimators_online')
#==========================================================
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

