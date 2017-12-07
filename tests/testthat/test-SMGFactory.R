context("SMGFactory.R")
described.class <- SMGFactory
#==========================================================
context(" initialize")
#==========================================================
test_that("it should initialize without any problems", {
  expect_error(described.class$new(), NA)
})

context(" fabricate")
#==========================================================
test_that("it should work when no lagged params are provided", {
  subject <- described.class$new() 
  W  <- RandomVariable$new(family = 'gaussian', formula = W  ~ A)
  Y  <- RandomVariable$new(family = 'gaussian', formula = Y  ~ W)

  stub(subject$fabricate, 'SummaryMeasureGenerator$new', 
    function(SMG.list, ...) {
      called <<- TRUE
      expect_length(SMG.list, 1)
      provided_smgs <- lapply(SMG.list, function(x) class(x)) %>% unlist
      expect_true('SMG.Latest.Entry' %in% provided_smgs)
      expect_false('SMG.Lag' %in% provided_smgs)
      return(TRUE)
    }
  )
  called <<- FALSE
  result <- subject$fabricate(c(W,Y))
  expect_true(called)

  expect_true(result)
})

test_that("it should include the lagged generator when provided", {
  subject <- described.class$new() 
  W  <- RandomVariable$new(family = 'gaussian', formula = W  ~ Y_lag_1)
  Y  <- RandomVariable$new(family = 'gaussian', formula = Y  ~ W)

  stub(subject$fabricate, 'SummaryMeasureGenerator$new', 
    function(SMG.list, ...) {
      called <<- TRUE
      provided_smgs <- lapply(SMG.list, function(x) class(x)) %>% unlist
      expect_length(SMG.list, 2)
      expect_true('SMG.Latest.Entry' %in% provided_smgs)
      expect_true('SMG.Lag' %in% provided_smgs)
      return(TRUE)
    }
  )
  called <<- FALSE
  result <- subject$fabricate(c(W,Y))
  expect_true(called)

  expect_true(result)
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

