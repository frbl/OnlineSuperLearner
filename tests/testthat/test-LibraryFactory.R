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

context(' getDescriptions')
test_that("it should return a vector of descriptions when a vector of ML algorithms is given", {
  lf <- described.class$new()
  algos <- c('Alg1', 'Alg2')
  result <- lf$getDescriptions(algos)
  expect_equal(length(result), 2)
  expect_equal(result, c('Alg1', 'Alg2'))
})

test_that("it should return the descriptions in the list if a list of ml algorithms is given", {
  lf <- described.class$new()

  algos <- list(list(description='Alg1',
                          algorithm = 'DensityEstimation',
                          params = list(nbins = 3)))

  algos <- append(algos, list(list(description='Alg2',
                          algorithm = 'DensityEstimation',
                          params = list(nbins = 100))))
  
  result <- lf$getDescriptions(algos)
  expect_equal(length(result), 2)
  expect_equal(result, c('Alg1', 'Alg2'))
})

context(" fabricate")
context("  > without gridsearch")
test_that("it should should throw if the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- c('Wont work!') 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model algorithm')
})

test_that("it should should throw if one of the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm', 'Wont work!') 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model algorithm')
})

test_that("it should work with list of estimators", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm') 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  for (i in 1:length(result)) {
    model <- result[[i]]
    expect_true(is.a(model, SL.library[i]))
    expect_true(is.a(model, 'ML.Base'))
  }
})

test_that("it should work with a single estimator", {
  subject <- described.class$new()
  SL.library <- 'ML.Local.lm' 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  expect_true(is.a(result[[1]], 'ML.Local.lm'))
  expect_true(is.a(result[[1]], 'ML.Base'))
})

context("  > with gridsearch")
test_that("it should should throw if the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- list(list(algorithm = 'Wont work!', description = 'nothing')) 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model algorithm')
})

test_that("it should throw if one of the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- list(list(algorithm='ML.Local.lm', description = 'abc'), list(algorithm = 'Wont work!', description='cba')) 
  expect_error(subject$fabricate(SL.library), 'The model Wont work! is not a valid ML model algorithm')
})

test_that("it should should throw if the provide list contains invalid entries", {
  subject <- described.class$new()
  SL.library <- list(list(algorithm = 'ML.Local.lm')) 
  expect_error(subject$fabricate(SL.library), 'The entry ML.Local.lm is not specified correctly')
})

test_that("it should work without providing parameters", {
described.class <- LibraryFactory
  subject <- described.class$new()
  SL.library <- list(list(algorithm='ML.Local.lm', description='ML.Local.lm')) 
  result <- subject$fabricate(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  for (i in 1:length(result)) {
    model <- result[[i]]
    expect_true(is.a(model, SL.library[[i]]$algorithm))
    expect_true(is.a(model, 'ML.Base'))
  }
})

test_that("it should expand a grid for gridsearch", {
  subject <- described.class$new()

  nbins1 <- c(122,123,1234,12345)
  nbins2 <- c(1,2,3,100)
  SL.library <- list(list(description='Alg1',
                          algorithm = 'DensityEstimation',
                          params = list(nbins = nbins1)))

  SL.library <- append(SL.library, list(list(description='Alg2',
                          algorithm = 'DensityEstimation',
                          params = list(nbins = nbins2))))

  result <- subject$fabricate(SL.library)
  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 4 * 2 )
  i <- 1
  for (model in result) {
    expect_true(is.a(model, 'DensityEstimation'))
    expect_equal(model$get_nbins, c(nbins1, nbins2)[i])
    i <- i + 1
  }
  # TODO: Also test with an actual grid of params
})
