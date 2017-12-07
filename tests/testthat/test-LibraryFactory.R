context("LibraryFactory")
#==========================================================
described.class <- LibraryFactory

context(" initialize")
#==========================================================
test_that("it should initialize", {
 expect_error(described.class$new(),NA)
})

test_that("it should throw if the provided verbosity is not correct", {
  expect_error(described.class$new(verbose = 'Random'), "Argument 'verbose' is non-logical: character")
})

test_that("it should store the allowed ML models", {
  allowed_ml_models <- c('allwed_models')
  subject <- described.class$new(ML.models.allowed = allowed_ml_models)
  expect_false(is.null(subject$get_allowed_ml_models))
  expect_equal(subject$get_allowed_ml_models, allowed_ml_models)
})

context(" get_validity")
#==========================================================
test_that("it should always return true", {
  # TODO: Implement proper validity checking
  lf <- described.class$new()
  expect_true(lf$get_validity)
})


context(" check_entry_validity")
#==========================================================
test_that("it should call the check_entry_validity_of_list for a list entry", {
  subject <- described.class$new()
  entry <- list('abc')
  stub(subject$check_entry_validity, 'self$check_entry_validity_of_list', 
    function(SL.library.entry) {
      expect_equal(SL.library.entry, entry)
      called <<- TRUE 
      return(c())
    }
  )
  subject <- described.class$new()

  called <<- FALSE
  subject$check_entry_validity(entry)
  expect_true(called)
})

test_that("it should raise the errors from the check_entry_validity_of_list", {
  subject <- described.class$new()
  entry <- list('abc')
  stub(subject$check_entry_validity, 'self$check_entry_validity_of_list', 
    function(SL.library.entry) {
      expect_equal(SL.library.entry, entry)
      return(c('my errors'))
    }
  )
  subject <- described.class$new()
  expect_error(subject$check_entry_validity(entry),
               "The entry abc is not specified correctly: my errors",
               fixed = TRUE)
})

test_that("it should call the check_entry_validity_of_character for a list entry", {
  subject <- described.class$new()
  entry <- 'abc'
  stub(subject$check_entry_validity, 'self$check_entry_validity_of_character', 
    function(SL.library.entry) {
      expect_equal(SL.library.entry, entry)
      called <<- TRUE 
      return(c())
    }
  )
  subject <- described.class$new()

  subject$check_entry_validity(entry)
  expect_true(called)
})

test_that("it should raise the errors from the check_entry_validity_of_character", {
  subject <- described.class$new()
  entry <- 'abc'
  stub(subject$check_entry_validity, 'self$check_entry_validity_of_character', 
    function(SL.library.entry) {
      expect_equal(SL.library.entry, entry)
      return(c('my errors'))
    }
  )
  subject <- described.class$new()
  expect_error(subject$check_entry_validity(entry),
               "The entry abc is not specified correctly: my errors",
               fixed = TRUE)
})

test_that("it should raise an error if none of these entries is provided", {
  subject <- described.class$new()
  entry <- NULL
  subject <- described.class$new()
  expect_error(subject$check_entry_validity(entry), 
               "The entry  is not specified correctly: Entry is not a character nor a list",
               fixed = TRUE)
})


context(" check_entry_validity_of_character") 
test_that("it should should throw if the models provided are not valid", {
  subject <- described.class$new()
  SL.library <- 'Wont work!'
  result <- subject$check_entry_validity_of_character(SL.library)
  expected <- 'The model Wont work! is not a valid ML model algorithm'

  expect_equal(result, expected)
})


context(" check_entry_validity_of_character") 
#==========================================================
test_that("it should test the arguments provided as entry", {
  subject <- described.class$new()
  SL.library <- list(not_supported = 'Wont work!', algorithm = 'ML.NeuralNet') 

  result <- subject$check_entry_validity_of_list(SL.library) 
  expected <- 'Entry in SL specification: not_supported not supported!'
  expect_equal(result, expected)
})

test_that("it should check whether the specified ML algorithm is allowed", {
  subject <- described.class$new()
  SL.library <- list(algorithm = 'not_allowed') 

  result <- subject$check_entry_validity_of_list(SL.library) 
  expected <- 'The model not_allowed is not a valid ML model algorithm'
  expect_equal(result, expected)
})

test_that("it should check whether the algorithm is actually specified", {
  subject <- described.class$new()
  SL.library <- list() 

  result <- subject$check_entry_validity_of_list(SL.library) 
  expected <- 'Algorithm not specified'
  expect_equal(result, expected)
})


context(" > checks the params") 
test_that("it should check whether the provided params are a list", {
  subject <- described.class$new()
  SL.library <- list(algorithm = 'ML.NeuralNet', params = '') 

  result <- subject$check_entry_validity_of_list(SL.library) 
  expected <- 'The params entry should be a list'
  expect_equal(result, expected)
})

test_that("it should check whether the provided list is not empty", {
  subject <- described.class$new()

  not_allowed <- list(list(), list(list()) )
  for (param in not_allowed) {
    SL.library <- list(algorithm = 'ML.NeuralNet', params = param) 
    result <- subject$check_entry_validity_of_list(SL.library) 
    expected <- 'If you add a params entry, also add params to it.'
    expect_equal(result, expected)
  }
})

context(" > checks the algorithm_params") 
test_that("it should check whether the provided params are a list", {
  subject <- described.class$new()
  SL.library <- list(algorithm = 'ML.NeuralNet', algorithm_params = '') 

  result <- subject$check_entry_validity_of_list(SL.library) 
  expected <- 'The algorithm_params entry should be a list'
  expect_equal(result, expected)
})

test_that("it should check whether the provided list is not empty", {
  subject <- described.class$new()

  not_allowed <- list(list(), list(list()) )
  for (param in not_allowed) {
    SL.library <- list(algorithm = 'ML.NeuralNet', algorithm_params = param) 
    result <- subject$check_entry_validity_of_list(SL.library) 
    expected <- 'If you add a algorithm_params entry, also add params to it.'
    expect_equal(result, expected)
  }
})

context(" fabricate")
#==========================================================
test_that("it should call the fabricate_grid function for a list of elements and return the result of that function", {
  subject <- described.class$new()
  entry <- list('abc')
  stub(subject$fabricate, 'self$fabricate_grid', 
    function(SL.library) {
      expect_equal(SL.library, entry)
      called <<- TRUE 
      return(entry)
    }
  )

  stub(subject$fabricate, 'self$inject_names_in_estimators', 
    function(fabricatedLibrary) { return(entry) }
  )

  subject <- described.class$new()

  called <- FALSE
  result <- subject$fabricate(entry)
  expect_true(called)

  expect_equal(result, entry)
})


test_that("it should call the fabricate_default function for other elements and return the result of that function", {
  subject <- described.class$new()
  entry <- 'abc'
  stub(subject$fabricate, 'self$fabricate_default', 
    function(SL.library) {
      expect_equal(SL.library, entry)
      called <<- TRUE 
      return(entry)
    }
  )

  stub(subject$fabricate, 'self$inject_names_in_estimators', 
    function(fabricatedLibrary) { return(entry) }
  )

  subject <- described.class$new()

  called <<- FALSE
  result <- subject$fabricate(entry)
  expect_true(called)

  expect_equal(result, entry)
})

test_that("it should call the inject_names_in_estimators function for the outcome of the fabricate_default and fabricate_grid functions", {
  subject <- described.class$new()
  entry_grid <- list('abc')
  entry_default <- list('abc')
  stub(subject$fabricate, 'self$fabricate_default', 
    function(SL.library) { return(SL.library) }
  )

  stub(subject$fabricate, 'self$fabricate_grid', 
    function(SL.library) { return(SL.library) }
  )

  stub(subject$fabricate, 'self$inject_names_in_estimators', 
    function(fabricatedLibrary) {
      called <<- TRUE
      return(fabricatedLibrary) 
    }
  )

  subject <- described.class$new()

  called <<- FALSE
  result <- subject$fabricate(entry_grid)
  expect_true(called)
  expect_equal(result, entry_grid)

  called <<- FALSE
  result <- subject$fabricate(entry_default)
  expect_true(called)
  expect_equal(result, entry_default)
})


context(" fabricate_default")
#==========================================================
test_that("it should throw if the provided library is not a character vector", {
  subject <- described.class$new()
  entry <- glm
  expect_error(subject$fabricate_default(entry), 
               "cannot coerce type 'closure' to vector of type 'character'",
               fixed = TRUE)
})

test_that("it should check the validity of each entry provided", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm', 'ML.NeuralNet') 
  stub(subject$fabricate_default, 'self$check_entry_validity', 
    function(SL.library.entry) {
      expect_equal(SL.library.entry, SL.library[[iter]])
      iter <<- iter + 1
      return(TRUE)
    }
  )

  iter <<- 1
  subject$fabricate_default(SL.library)
  expect_equal(iter, 3)
})

test_that("it should return the correct result", {
  subject <- described.class$new()
  SL.library <- c('ML.Local.lm', 'ML.NeuralNet') 
  stub(subject$fabricate_default, 'self$check_entry_validity', 
    function(SL.library.entry) { return(TRUE) }
  )

  result <- subject$fabricate_default(SL.library)
  expect_equal(iter, 3)

  expect_is(result, 'list')
  expect_length(result, 2)
  for (i in 1:length(result)) {
    model <- result[[i]]

    ## Check whether the eval works
    expect_true(is.a(model, 'DensityEstimation'))
    expect_true(is.a(model$get_bin_estimator, 'ML.Base'))
    expect_true(is.a(model$get_bin_estimator, SL.library[i]))
  }
  expect_named(result, SL.library)
})

test_that("it should also work with a single estimator", {
  subject <- described.class$new()
  SL.library <- 'ML.Local.lm' 

  result <- subject$fabricate_default(SL.library)

  expect_true(is.a(result, 'list' ))
  expect_true(length(result) == 1 )
  
  expect_true(is.a(result[[1]], 'DensityEstimation'))
  expect_true(is.a(result[[1]]$get_bin_estimator, 'ML.Base'))
  expect_true(is.a(result[[1]]$get_bin_estimator, 'ML.Local.lm'))
})



context(" fabricate_grid")
#==========================================================
test_that("it should check the validity for each entry", {
  
})



##############################
# Refactored specs till here #
##############################

test_that("it should work without providing parameters (except for the name of the algorithm)", {
  subject <- described.class$new()
  model_string <- 'ML.Local.lm'
  SL.library <- list(list(algorithm=model_string)) 
  result <- subject$fabricate_grid(SL.library)

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

