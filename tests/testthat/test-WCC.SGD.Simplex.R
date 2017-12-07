context("WCC.SGD.Simplex.R")
#================================================
described.class <- WCC.SGD.Simplex
set.seed(12345)
n <- 1e5
Z <- rnorm(n)

pred <- cbind(Z, Z^2, Z^3, Z^4)
true_params <- c(.2, .8, 0, 0)

Y <- pred %*% true_params +rnorm(n)
initial_weights <- rep(NA, 4) 
libraryNames <- c('a', 'b', 'c', 'd')
names(initial_weights) <- libraryNames

context(' initialize')
#================================================
test_that("it should initialize with a random set of weights", {
  subject <- described.class$new(weights.initial = initial_weights)
  expect_false(any(is.na(subject$get_weights)))
  expect_true(all(is(subject$get_weights, 'numeric')))
})

test_that("it should throw if initialized with a null weight vector", {
  expect_error(
    described.class$new(weights.initial = NULL),
    "Please provide initial weights (or NA vector with the correct size)",
    fixed = TRUE
  )
})

test_that("it should give a warning if the provided vector is not NAs", {
  expected_warning <- 'The weights provided will be overridden by a random vector! If you dont want this message, please provide a vector (of the correct size) with NAs'
  expect_warning(described.class$new(weights.initial = c(1/4, 1/4, 1/4, 1/4)), expected_warning, fixed = TRUE)
  expect_warning(described.class$new(weights.initial = initial_weights), NA)
})

test_that("it should accept a stepsize variable and store it", {
  result <- described.class$new(weights.initial = c(NA,NA,NA,NA), auto_update_stepsize = FALSE)

  ## The default stepsize
  expect_equal(result$get_step_size, 0.0001)
  result <- described.class$new(weights.initial = c(NA,NA,NA,NA), step_size = 42, auto_update_stepsize = FALSE)
  expect_equal(result$get_step_size, 42)
})

test_that("it should be possible to automate stepsize selection", {
  subject <- described.class$new(weights.initial = c(NA,NA,NA,NA), auto_update_stepsize = TRUE)
  expect_equal(subject$get_step_size, 1)
  expect_equal(subject$is_auto_updating_stepsize, TRUE)
})

test_that("it should initialze the step_count to 0", {
  subject <- described.class$new(weights.initial = c(NA,NA,NA,NA), auto_update_stepsize = TRUE)
  ## The default stepsize
  expect_equal(subject$get_step_count, 0)
})

test_that("it should store the provided number of iterations", {
  result <- described.class$new(weights.initial = c(NA,NA,NA,NA))

  ## The default iterations
  expect_equal(result$get_iterations, 1000)
  result <- described.class$new(weights.initial = c(NA,NA,NA,NA), iterations = 42)
  expect_equal(result$get_iterations, 42)
})

context(' compute')
#================================================
test_that("it should compute the correct convex combination", {
  set.seed(12345)
  subject <- described.class$new(
    weights.initial = initial_weights,
    auto_update_stepsize = FALSE,
    step_size = 0.0000001,
    iterations = 350
  )

  subject$compute(Z = pred, Y = Y, libraryNames = libraryNames)

  ## All weights should be 0<= wt <= 1
  for (wt in subject$get_weights) {
    expect_lte(wt, 1)
    expect_gte(wt, 0)
  }

  ## Their sum should (almost) equal one
  expect_lt(1 - sum(subject$get_weights), 1e-2)

  # Check that we approximate the true parameters
  difference <- subject$get_weights - true_params

  for (diff in difference) {
    expect_lt(abs(diff), 1e-2)
  }
})

context(" get_step_size")
#================================================
test_that("it should should return the correct stepsize", {
  step_size = 0.123
  subject <- described.class$new(weights.initial = initial_weights, step_size = step_size)
  expect_equal(subject$get_step_size, step_size)
})

test_that("it should update the stepsize if updating is enabled", {
  step_size = 0.123
  subject <- described.class$new(weights.initial = initial_weights, step_size = step_size, auto_update_stepsize = TRUE)
  for (i in 1:10) {
    expect_equal(subject$get_step_size, 1/i)
  }
})
