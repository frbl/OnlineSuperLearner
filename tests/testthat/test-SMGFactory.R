context("SMGFactory.R")
described.class <- SMGFactory
context(" initialize")
context(" fabricate")
test_that("it should work when no lagged params are provided", {
  
})

test_that("it should return a SummaryMeasureGenerator", {
  subject <- described.class$new() 
  W  <- RandomVariable$new(family = 'gaussian', formula = W  ~ Y_lag_2)
  Y  <- RandomVariable$new(family = 'gaussian', formula = Y  ~ W)
  result <- subject$fabricate(c(W,Y))
  expect_true(is.a(result, 'SummaryMeasureGenerator'))
})

test_that("it should create the all SMGs", {
  subject <- described.class$new() 
  W  <- RandomVariable$new(family = 'gaussian', formula = W  ~ Y_lag_2)
  Y  <- RandomVariable$new(family = 'gaussian', formula = Y  ~ W)
  result <- subject$fabricate(c(W,Y))$get_smg_list
  expect_equal(length(result), 2)
  vars <- lapply(result, function(smg) smg$exposedVariables) %>%
    unlist

  expect_true(all(c('W', 'Y_lag_2') %in% vars))
})

test_that("it should create only the necessary SMGs", {
  subject <- described.class$new() 
  W  <- RandomVariable$new(family = 'gaussian', formula = W  ~ Y)
  Y  <- RandomVariable$new(family = 'gaussian', formula = Y  ~ W)
  result <- subject$fabricate(c(W,Y))$get_smg_list
  expect_equal(length(result), 1)
  vars <- lapply(result, function(smg) smg$exposedVariables) %>%
    unlist

  expect_true(all(c('W', 'Y') %in% vars))
  expect_false(any(c('W_lag_1', 'Y_lag_1', 'W_lag_2', 'Y_lag_2') %in% vars))
})

