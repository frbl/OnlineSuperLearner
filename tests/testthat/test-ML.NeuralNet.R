context("ML.NeuralNet.R")
described.class <- ML.NeuralNet

context(" initialize")
#=========================================================
test_that("it should initialize", {
  expect_error(described.class$new(), NA)
})

context(" do.fit")
#==========================================================
test_that("it should fit a basic model", {
  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10),
    x2=c(1,2,3,4,5,6,7,8,9,10)
  )
  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  model <- subject$perform_fit(X_mat = data, Y_vals = y, save_model = FALSE)
  expect_is(model, 'nn')
})

test_that("it should store a cache of the neural network", {
  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10),
    x2=c(1,2,3,4,5,6,7,8,9,10)
  )
  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  file.remove(subject$get_file_name)
  model <- subject$perform_fit(X_mat = data, Y_vals = y, save_model = TRUE)
  expect_true(file.exists(subject$get_file_name))
})

context(" do.update")
#==========================================================
test_that("it should update the model and return the updated version",{
  #make first a model and update this model
  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10),
    x2=c(1,2,3,4,5,6,7,8,9,10)
  )
  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  model <- subject$perform_fit(X_mat = data, Y_vals = y)

  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10),
    x2=c(1,2,3,4,5,6,7,8,9,10)
  )

  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  testfit <- subject$perform_update(X_mat = data, Y_vals = y, m.fit = model)
  expect_is(testfit, 'nn')
  expect_false(equals(testfit, model))
  # TODO: Actually perform the test here
  #print(testfit)
})

test_that("it should update a model with new data that actually makes it better", {
  skip('Needs to be implemented')
})

test_that("it should update the saved model",{
  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10),
    x2=c(1,2,3,4,5,6,7,8,9,10)
  )
  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  model <- subject$perform_fit(X_mat = data, Y_vals = y, save_model = TRUE)
  pre <- readRDS(subject$get_file_name)

  data <- data.table(
    x = c(1,2,3,4,5,6,7,8,9,10)
  )
  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  subject$perform_update(X_mat = data, Y_vals = y, save_model = TRUE, m.fit = NULL)
  post <- readRDS(subject$get_file_name)

  expect_false(equals(pre, post))
  # TODO: Actually perform the test here
})

context(" do.predict")
#========================================================
test_that("this should make a prediction with a new model",{
  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10),
    x2=c(1,2,3,4,5,6,7,8,9,10)
  )
  y <- c(0,0,0,0,0,0,1,0,1,1)
  subject <- described.class$new()
  model <- subject$perform_fit(X_mat = data, Y_vals = y)
  #then predict
  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10),
    x2=c(1,2,3,4,5,6,7,8,9,10)
  )
  subject <- described.class$new()
  prob <- subject$perform_prediction(X_mat = data, m.fit=list(coef=model))
  # TODO: Actually perform the test here
})

test_that("this should make a prediction using a saved model",{
  data <- data.table(
    x1=c(1,2,3,4,5,6,7,8,9,10)
  )
  subject <- described.class$new()
  prob <- subject$perform_prediction(X_mat = data,m.fit=NULL)
  # TODO: Actually perform the test here
})

