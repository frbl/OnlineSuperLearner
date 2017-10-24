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
  m <- mock(1)

  with_mock(`parallel::detectCores` = m,
    described.class$new(nthread = -1)
  )

  expect_called(m, 1)
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
  subject <- described.class$new()
  nobs <- 40
  delta <- 0.05
  X_mat <- data.table(A = rnorm(nobs, 0,1), B = rnorm(nobs, 0,1))
  probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta

  X_mat <- as.matrix(X_mat)
  Y_vals <- rbinom(nobs, 1, probs)

  initial_model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)

  result <- subject$perform_prediction(X_mat = X_mat, m.fit = list(coef = initial_model))

  misclassifications <- sum(as.numeric(result > 0.5) - Y_vals)
  expect_equal(misclassifications, 1)
})

context(" update")
#===================================================
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #
# NOT YET WORKING, SEE https://github.com/dmlc/xgboost/issues/2545 #
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! #

#test_that("it should call the do.fit function with the provided model", {
  #subject <- described.class$new()
  #nobs <- 20
  #delta <- 0.05
  #X_mat <- data.table(A = rnorm(nobs, 0,1), B = rnorm(nobs, 0,1))
  #probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta

  #X_mat <- as.matrix(X_mat)
  #Y_vals <- rbinom(nobs, 1, probs)

  #initial_model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)

  #expect_equal(initial_model$niter, 200)
  #updated_model <- subject$perform_update(X_mat, Y_vals, m.fit = list(coef = initial_model))

  #expect_is(updated_model, 'xgb.Booster')

  ### Test if it was updated
  #expect_equal(updated_model$niter, 400)
#})

#test_that("it should improve the fit if we update the model", {
  #set.seed(12345)
  ##subject <- described.class$new()
  #nobs <- 20
  #niter <- 100
  #test_nobs <- 200
  #delta <- 0.05

  #X_mat <- data.table(A = rnorm(nobs, 0,1), B = rnorm(nobs, 0,1))
  #probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta
  #X_mat <- as.matrix(X_mat)
  #Y_vals <- rbinom(nobs, 1, probs)

  ### Create a test set
  #test_X_mat <- data.table(A = rnorm(test_nobs, 0,1), B = rnorm(test_nobs, 0,1))
  #test_probs <- pmax(as.numeric(test_X_mat$A > 0) - 2*delta, 0) + delta
  #test_X_mat <- as.matrix(test_X_mat)
  #test_Y_vals <- rbinom(test_nobs, 1, test_probs)


  #params <- list(objective = Arguments$getCharacter('binary:logistic'),
    #nthread = 1)

  #dtrain <- xgb.DMatrix(data = X_mat,
                        #label = Y_vals)


  #previous_model <- xgb.train(
    #data = dtrain,
    #params     = params,
    #nrounds    = 200,
    ##watchlist = watchlist,
    #xgb_model  = NULL,
    #verbose    = 0
  #) #private$verbosity)

  #result <- predict(previous_model, newdata = test_X_mat, type='response')
  #print(result)


  #prev_misclassifications <- sum(abs(as.numeric(result > 0.5) - test_Y_vals))

  #print(prev_misclassifications)
  #misclassifications <- Inf
  #params <- modifyList(params, list(process_type = 'update', updater = 'refresh', refresh_leaf = TRUE))
  #for (i in 1:niter) {
    #X_mat <- data.table(A = rnorm(nobs, 0, 1), B = rnorm(nobs, 0, 1))
    #probs <- pmax(as.numeric(X_mat$A > 0) - 2*delta, 0) + delta
    #X_mat <- as.matrix(X_mat)
    #Y_vals <- rbinom(nobs, 1, probs)
    

    #dtrain <- xgb.DMatrix(data = X_mat,
                          #label = Y_vals)
    #previous_model <- xgb.train(
      #data = dtrain,
      #params     = params,
      #nrounds    = 10,
      ##watchlist = watchlist,
      #xgb_model  = previous_model,
      #verbose    = 0
    #) #private$verbosity)

    #### Update the fit
    ##previous_model <- subject$perform_update(X_mat = X_mat, Y_vals = Y_vals, m.fit = list(coef = previous_model))
    ##result <- subject$perform_prediction(X_mat = test_X_mat, m.fit = list(coef = previous_model))
    #result <- predict(previous_model, newdata = test_X_mat, type='response')
    #print(result)

    ##result <- subject$perform_prediction(X_mat = test_X_mat, m.fit = list(coef = previous_model))
    ##misclassifications <- sum(abs(as.numeric(result > 0.5) - test_Y_vals))
  #}
  
  ##expect_lte(misclassifications, prev_misclassifications)
  ##expect_lte(misclassifications, 10)
#})
