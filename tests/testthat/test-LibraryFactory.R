context("LibraryFactory")
described.class <- LibraryFactory

context(" initialize")
test_that("it should initialize", {
 expect_error(described.class$new(),NA)
})

context(" get_validity")
test_that("it should always return true", {
  # TODO: Implement proper validity checking
  lf <- described.class$new()
  expect_true(lf$get_validity)
})

context(" check_entry_validity")
# TODO: Test all routes
context("  > with vector") ####################
test_that("it should should throw if the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- c('Wont work!') 
  expect_error(subject$check_entry_validity(SL.library), 'The model Wont work! is not a valid ML model algorithm')
})

context("  > with list") ####################
test_that("it should should throw if the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- list(algorithm = 'Wont work!') 
  expect_error(subject$check_entry_validity(SL.library), 'The entry Wont work! is not specified correctly: The model Wont work! is not a valid ML model algorithm')
})

test_that("it should throw if the model provided library does not contain an algorithm", {
  described.class <- LibraryFactory
  subject <- described.class$new()
  SL.library <- list(params=list(nbins=10)) 
  # TODO: I think this is a bug in R or testthat? The following test fails, although the msg is exactly the same.
  #expect_error(subject$check_entry_validity(SL.library), 'The entry list(nbins = 1) is not specified correctly: Algorithm not specified')
})

context(" fabricate")
context("  > without gridsearch")
test_that("it should should throw if one of the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm', 'Wont work!') 
  expect_error(subject$fabricate(SL.library), 'The entry Wont work! is not specified correctly: The model Wont work! is not a valid ML model algorithm')
})

test_that("it should should throw if the provide list contains invalid entries", {
  subject <- described.class$new()
  SL.library <- list(algorithm = 'ML.Local.lm', thisisnotsupposedtobethere = 'abc') 
  expect_error(subject$check_entry_validity(SL.library), 'The entry ML.Local.lm abc is not specified correctly')
})


test_that("it should work with list of estimators", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm') 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  for (i in 1:length(result)) {
    model <- result[[i]]
    expect_true(is.a(model, 'DensityEstimation'))
    expect_true(is.a(model$get_bin_estimator, 'ML.Base'))
    expect_true(is.a(model$get_bin_estimator, SL.library[i]))
  }
})

test_that("it should work with a single estimator", {
  subject <- described.class$new()
  SL.library <- 'ML.Local.lm' 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  
  expect_true(is.a(result[[1]], 'DensityEstimation'))
  expect_true(is.a(result[[1]]$get_bin_estimator, 'ML.Base'))
  expect_true(is.a(result[[1]]$get_bin_estimator, 'ML.Local.lm'))
})

context("  > with gridsearch")
test_that("it should throw if one of the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- list(list(algorithm='ML.Local.lm'), list(algorithm = 'Wont work!')) 
  expect_error(subject$fabricate(SL.library), 'The entry Wont work! is not specified correctly: The model Wont work! is not a valid ML model algorithm')
})


test_that("it should work without providing parameters (except for the name of the algorithm)", {
  subject <- described.class$new()
  model_string <- 'ML.Local.lm'
  SL.library <- list(list(algorithm=model_string)) 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  for (i in 1:length(result)) {
    model <- result[[i]]
    expect_true(is.a(model, 'DensityEstimation'))
    expect_true(is.a(model$get_bin_estimator, SL.library[[i]]$algorithm))
    expect_true(is.a(model$get_bin_estimator, model_string))
  }
})

test_that("it should expand a grid for gridsearch for the density", {
  subject <- described.class$new()

  nbins1 <- c(122,123,1234,12345)
  nbins2 <- c(1,2,3,100)
  SL.library <- list(list(algorithm = 'ML.Local.lm',
                          params = list(nbins = nbins1)))

  SL.library <- append(SL.library, list(list( algorithm = 'ML.Local.lm',
                          params = list(nbins = nbins2))))

  result <- subject$fabricate(SL.library)
  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 4 * 2 )
  i <- 1
  for (model in result) {
    expect_true(is.a(model, 'DensityEstimation'))
    expect_true(is.a(model$get_bin_estimator, 'ML.Local.lm'))
    expect_equal(model$get_nbins, c(nbins1, nbins2)[i])
    i <- i + 1
  }
  # TODO: Also test with an actual grid of params
})

test_that("it should expand a grid for the gridsearch of both the density aswell as the candidate", {
  subject <- described.class$new()

  nbins1 <- c(122,123,1234,12345)
  algorithm_params1 <- list(
    learning.rate = c(0.1, 0.2, 0.5), 
    family='gaussian', 
    initialization.random=c(FALSE, TRUE))


  nbins2 <- c(1,2,3)
  algorithm_params2 <- list(
    learning.rate = 0.5, 
    family='gaussian', 
    initialization.random=c(FALSE, TRUE))
  
  SL.library <- list(list(params = list(nbins = nbins1),
                          algorithm = 'ML.Local.lm',
                          algorithm_params = algorithm_params1))

  SL.library <- append(SL.library, list(list(
                          params = list(nbins = nbins2),
                          algorithm = 'ML.Local.lm',
                          algorithm_params = algorithm_params2)))

  result <- subject$fabricate(SL.library)



  expect_true(is.a(result, 'list' ))
  expected_length1 <- length(nbins1) * 
    length(algorithm_params1$learning.rate) *
    length(algorithm_params1$family) *
    length(algorithm_params1$initialization.random)
  
  expected_length2 <- length(nbins2) * 
    length(algorithm_params2$learning.rate) *
    length(algorithm_params2$family) *
    length(algorithm_params2$initialization.random)

  expected_length <- expected_length1 + expected_length2

  expect_equal(length(result), expected_length)
  i <- 0
  j <- expected_length1
  for (model in result) {
    expect_true(is.a(model, 'DensityEstimation'))
    expect_true(is.a(model$get_bin_estimator, 'ML.Local.lm'))

    nbins <- ifelse(j > 0, nbins1[(i %% length(nbins1))+1], nbins2[(i %% length(nbins2))+1])
    expect_equal(model$get_nbins, nbins)

    i <- i + 1
    j <- j - 1
  }
  # TODO: Also test with an actual grid of params
  # TODO: Also test the params of the inner algorithms
})

test_that("it should set the correct names", {
  subject <- described.class$new()

  algorithm1 <- 'ML.Local.lm'
  nbins1 <- c(122,123,1234,12345)
  algorithm_params1 <- list(
    learning.rate = c(0.1, 0.2, 0.5), 
    family='gaussian', 
    initialization.random=c(FALSE, TRUE))


  algorithm2 <- 'ML.XGBoost'
  nbins2 <- c(1,2,3,100)
  algorithm_params2 <- list(
    lambda = c(0.1,0.2,0.3), 
    alpha = c(0.5,0.6))
  
  SL.library <- list(list(params = list(nbins = nbins1),
                          algorithm = algorithm1,
                          algorithm_params = algorithm_params1))

  SL.library <- append(SL.library, list(list(
                          params = list(nbins = nbins2),
                          algorithm = algorithm2,
                          algorithm_params = algorithm_params2)))

  result <- names(subject$fabricate(SL.library))
  generate_grid_list <- function(algorithm, algorithm_params, nbins) {
    expected <- apply(expand.grid(algorithm_params),1, function(entry){
      paste(names(entry), entry, sep='-', collapse='_')
    }) %>% paste(algorithm, ., sep='-')

    expected <- lapply(nbins,function(entry){
      paste(expected, paste('nbins', entry, sep='-'), sep='.')
    }) %>% unlist %>% gsub(" ", "", ., fixed = TRUE)
    expected
  } 

  expected <- lapply(list(list(algorithm = algorithm1, algorithm_params = algorithm_params1, nbins = nbins1),
              list(algorithm = algorithm2, algorithm_params = algorithm_params2, nbins = nbins2)
         ), function(entry) do.call(generate_grid_list, args=entry)) %>% unlist

  all_in_there <- all(lapply(result, function(entry) entry %in% expected) %>% unlist)
  expect_true(all_in_there)
  expect_equal(length(result), length(expected))
})

