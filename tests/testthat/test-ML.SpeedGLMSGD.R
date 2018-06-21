context("ML.SpeedGLMSGD.R")
described.class <- ML.SpeedGLMSGD

context(" initialize")
#=========================================================
test_that("it should initialize", {
  expect_error(described.class$new(), NA)
})

context(" do.fit")
#==========================================================
test_that("it should fit a basic model", {
  Y_vals <- c(0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,1,0,1,0)
  X_mat <- data.table(x1 =	c(3,1,3,1,3,3,1,3,3,2,3,1,3,3,3,2,3,2,3),
                      x2 =	c(1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0))
  subject <- described.class$new(save_model = FALSE)
  model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  expect_is(model, 'speedlm')
})

test_that("it should store the coef of the speedGLMSGD", {
  X_mat <- data.table(
    x1=c(1,2,3,1,2,3,1,2,3,1),
    x2=c(0,0,1,1,0,1,1,0,1,0)
  )
  Y_vals <- c(0,1,0,1,0,1,1,0,1,1)
  subject <- described.class$new(save_model = TRUE)
  #file.remove(subject$get_file_name)
  model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  expect_true(file.exists(subject$get_file_name))
})

context(" do.update")
#==========================================================
test_that("it should update the model and return the updated version",{
  #make first a model and update this model
  Y_vals <- c(0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,1,0,1,0)
  X_mat <- data.table(x1 =	c(3,1,3,1,3,3,1,3,3,2,3,1,3,3,3,2,3,2,3),
                      x2 =	c(1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0))
  subject <- described.class$new(save_model = FALSE)
  model<-subject$perform_fit(X_mat =X_mat, Y_vals = Y_vals)
  
  X_mat <- data.table(
    x1=c(1,2,3,1,2,3,1,2,3,1),
    x2=c(0,0,1,1,0,1,1,0,1,0)
  )
  
  Y_vals <- c(0,1,0,1,0,1,1,0,1,1)
  subject <- described.class$new()
  testfit<-subject$perform_update(X_mat = X_mat, Y_vals = Y_vals, m.fit = coef(model))
  expect_is(testfit, 'matrix')
  expect_false(equals(testfit, coef(model)))
 
})
context(" do.update")
#==========================================================
test_that("it should fit the model and save the coefficients and return the updated version",{
  #make first a model and save it
  Y_vals <- c(0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,1,0,1,0)
  X_mat <- data.table(x1 =	c(3,1,3,1,3,3,1,3,3,2,3,1,3,3,3,2,3,2,3),
                      x2 =	c(1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0))
  subject <- described.class$new(save_model = TRUE )
  model<-subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  
  X_mat <- data.table(
    x1=c(1,2,3,1,2,3,1,2,3,1),
    x2=c(0,0,1,1,0,1,1,0,1,0)
  )
  
  Y_vals <- c(0,1,0,1,0,1,1,0,1,1)
  subject <- described.class$new()
  testfit <- subject$perform_update(X_mat = X_mat, Y_vals = Y_vals, m.fit = NULL)

  # We are returning a list of coefficients, hence numeric
  expect_is(testfit, 'numeric')
  expect_false(equals(testfit, coef(model)))
})

context(" do.update")
#==========================================================
test_that("it should fit the model twice and not save the coefficients and return the updated version",{
  #make first a model and save it
  Y_vals <- c(0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,1,0,1,0)
  X_mat <- data.table(x1 =	c(3,1,3,1,3,3,1,3,3,2,3,1,3,3,3,2,3,2,3),
                      x2 =	c(1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0))
  subject <- described.class$new(save_model = FALSE )
  model<-subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  
 X_mat <- data.table(
    x1=c(1,2,3,1,2,3,1,2,3,1),
    x2=c(0,0,1,1,0,1,1,0,1,0)
  )
  
  Y_vals <- c(0,1,0,1,0,1,1,0,1,1)
  subject <- described.class$new()
  testfit<-subject$perform_update(X_mat = X_mat, Y_vals = Y_vals, m.fit = NULL)
  #expect_is(testfit, 'numeric')
  expect_false(equals(testfit, coef(model)))
  
})

context(" do.predict")
#==========================================================
test_that("it should be able to do a prediction", {
    subject <- described.class$new()
    Y_vals <- c(0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,1,0,1,0)
    X_mat <- data.table(x1 =	c(3,1,3,1,3,3,1,3,3,2,3,1,3,3,3,2,3,2,3),
                        x2 =	c(1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0))
    
    initial_model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
    
    result <- subject$perform_prediction(X_mat = X_mat, Y_vals = Y_vals, m.fit = coef(initial_model))
    
    
    mse <- mean((result-Y_vals)^2)
    
    expect_lt(mse,0.2)
   
})

context(" do.predict")
#==========================================================
test_that("it should be able to do a prediction with a saved model", {
  subject <- described.class$new(save_model = TRUE)
  Y_vals <- c(0,1,1,1,0,0,0,0,1,1,1,1,0,0,0,1,0,1,0)
  X_mat <- data.table(x1 =	c(3,1,3,1,3,3,1,3,3,2,3,1,3,3,3,2,3,2,3),
                      x2 =	c(1,0,0,0,1,1,1,1,0,0,0,0,1,1,0,0,1,1,0))
  
  initial_model <- subject$perform_fit(X_mat = X_mat, Y_vals = Y_vals)
  
  result <- subject$perform_prediction(X_mat = X_mat, Y_vals = Y_vals, m.fit = NULL)
  mse <- mean((result-Y_vals)^2)
  
  expect_lt(mse,0.2)
  
})
