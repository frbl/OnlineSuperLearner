library('mockery')
context("ML.XGBoost.R")
described.class <- ML.XGBoost
subject <- described.class$new()

context(" initialize")
#===================================================
test_that("it should initialize with the defaults", {
  subject <- described.class$new()
  expect_is(subject, 'ML.XGBoost')
})

test_that("it should throw if the rounds are 0 or negative", {
  expect_error(described.class$new(rounds = -1))
  expect_error(described.class$new(rounds = 0))
})

test_that("it should throw if alpha is < 0 or > 1", {
  expect_error(described.class$new(alpha = -0.0001))
  expect_error(described.class$new(alpha = 1.0001))
})

test_that("it should throw if lambda is < 0 or > 1", {
  expect_error(described.class$new(lambda = -0.0001))
  expect_error(described.class$new(lambda = 1.0001))
})

test_that("it should throw if eta is <= 0", {
  expect_error(described.class$new(eta = 0.0001), NA)
  expect_error(described.class$new(eta = 0))
  expect_error(described.class$new(eta = -1))
})

test_that("it should throw if gamma is < 0", {
  expect_error(described.class$new(gamma = -1))
})

test_that("it should run in parallel if nthread = -1", {
  subject <- described.class$new(nthread = -1)
  expect_false(subject$get_nthread == -1)
  expect_equal(subject$get_nthread, parallel::detectCores())
})

test_that("it should throw if verbose is invalid", {
  expect_error(described.class$new(verbose = 'abc'))
})

context(" fitfunname")
#===================================================
test_that("it should define a fitfunname", {
  expect_false(is.null(subject$fitfunname)) 
})

context(" lmclass")
#===================================================
test_that("it should define a fitfunname", {
  expect_false(is.null(subject$lmclass)) 
})

context(" get_validity")
#===================================================
test_that("it should return true if the config is valid", {
  allowed_boosters <- c('gbtree', 'gblinear', 'dart')
  for (booster in allowed_boosters) {
    subject <- described.class$new(booster = booster)
    expect_true(subject$get_validity) 
  }
})

test_that("it should return throw if the config is not valid", {
  allowed_boosters <- c('gbtree', 'gblinear', 'dart')
  expected <- paste('Booster <not correct!> is not in list of allowed boosters:', paste(allowed_boosters, collapse=' '))
  expect_error(described.class$new(booster = '<not correct!>'), expected, fixed = TRUE) 
})

context(" fit")
#===================================================
test_that("it should return an xgbmodel", {
  subject <- described.class$new()
  nobs <- 20
  delta <- 0.05
  X_mat <- data.table(A = rnorm(nobs, 0,1), B = rnorm(nobs, 0,1))
  probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta

  X_mat <- as.matrix(X_mat)
  Y_vals <- rbinom(nobs, 1, probs)

  result <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  expect_is(result, 'xgb.Booster')
})

test_that("it should call the xbtrain function", {
  subject <- described.class$new()
  nobs <- 20
  X_mat <- as.matrix(data.table(A = rnorm(nobs, 0,1), B = rnorm(nobs, 0,1)))
  Y_vals <- rbinom(nobs, 1, 0.5)

  m <- mock('xgbtraining')
  with_mock(`xgboost::xgb.train` = m,
    subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  )

  expect_called(m, 1)
  args <- mock_args(m)[[1]]
  expect_is(args$data, 'xgb.DMatrix')

  expect_false(is.null(subject$get_params))
  expect_equal(args$params, subject$get_params)
  expect_false(is.null(subject$get_rounds))
  expect_equal(args$nrounds, subject$get_rounds)
  expect_equal(args$xgb_model, NULL)
})

context(" predict")
#===================================================
test_that("it should be able to do a prediction", {
  set.seed(12345)
  subject <- described.class$new(rounds = 10)
  nobs <- 40
  delta <- 0.05
  X_mat <- data.table(A = rnorm(nobs, 0,1), B = rnorm(nobs, 0,1))
  probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta

  X_mat <- as.matrix(X_mat)
  Y_vals <- rbinom(nobs, 1, probs)

  initial_model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)

  result <- subject$perform_prediction(X_mat = X_mat, m.fit = list(coef = initial_model))

  misclassifications <- sum(as.numeric(result > 0.5) - Y_vals)
  expect_equal(misclassifications, 0)
})

context(" update")
#===================================================
test_that("it should call the do.fit function with the provided model", {
  subject <- described.class$new(rounds=1, booster='gblinear')
  nobs <- 20
  delta <- 0.05
  X_mat <- data.table(A = rnorm(nobs, 0,1), B = rnorm(nobs, 0,1))
  probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta
  X_mat <- as.matrix(X_mat)
  Y_vals <- rbinom(nobs, 1, probs)
  initial_model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  # save model to R's raw vector otherwise an handle error occurs and no update will take place.
  raw = xgb.save.raw(initial_model)
  
  expect_equal(initial_model$niter, 1)
  updated_model <- subject$perform_update(X_mat, Y_vals, m.fit = list(coef = raw))
  
  
  expect_is(updated_model, 'xgb.Booster')
  ## Test if it was updated
  expect_equal(updated_model$niter, 2)
})

test_that("it should improve the fit if we update the model", {
  set.seed(12345)
  subject <- described.class$new()
  nobs <- 5
  niter <- 1000
  nrounds <- 1 
  test_nobs <- 200
  delta <- 0.05
  misclassifications <- 0

  ## Create a test set
  test_X_mat <- data.table(A = rnorm(test_nobs, 5,20), B = rnorm(test_nobs, 5,20))
  test_probs <- pmax(as.numeric(test_X_mat$A > 0) - 2*delta, 0) + delta
  test_X_mat <- as.matrix(test_X_mat)
  test_Y_vals <- rbinom(test_nobs, 1, test_probs)
  dtest_1 <- xgb.DMatrix(data = test_X_mat,
                        label = test_Y_vals)
  
	nobs <- nobs + 1 

	## Create a train set  
	X_mat_1 <- data.table(A = rnorm(nobs, 5,10), B = rnorm(nobs, 5,10))
	probs_1 <- pmax(as.numeric(X_mat_1$A > 0) - 2*delta, 0) + delta
	X_mat_1 <- as.matrix(X_mat_1)
	Y_vals_1 <- rbinom(nobs, 1, probs_1)
	
	model_1 <-  subject$perform_fit(X_mat = X_mat_1, Y_vals = Y_vals_1)

	## save model to R's raw vector otherwise an handle error occurs and no update will take place.
	raw <- xgb.save.raw(model_1)
	
	mse_model_1 <- mse(subject$perform_prediction( X_mat = test_X_mat, m.fit = list(coef = model_1)), test_Y_vals)  
	

	###create new dataset
	X_mat <- data.table(A = rnorm(nobs, 0, 1), B = rnorm(nobs, 0, 1))
	probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta
	X_mat <- as.matrix(X_mat)
	Y_vals <- rbinom(nobs, 1, probs)
	
	dtrain <- xgb.DMatrix(data = X_mat,
												label = Y_vals)

	## Update the fit
	## use the raw model
	model_2 <- subject$perform_update(X_mat = X_mat, Y_vals = Y_vals, m.fit = list(coef = raw))
	mse_model_2 <-mse(subject$perform_prediction( X_mat = test_X_mat, m.fit = list(coef = model_2)), test_Y_vals)  
	expect_that(mse_model_2,is_less_than(mse_model_1))
})

