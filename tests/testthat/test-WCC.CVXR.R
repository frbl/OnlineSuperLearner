## Test
##install.packages('CVXR')
##library('CVXR')

### Simple minimal constrained optimization function, for the CVXR package
### Variables minimized over
##k = 10
##n = 100

##true_beta <- rep(1/k, k)
##X <- matrix(rnorm(k * n), n, k)
##y <- X %*% true_beta + rnorm(n, 0, 0.1)

##beta <- Variable(k)
##objective <- Minimize(sum_squares(y - X %*% beta))
##constraints <- list(beta > 0, sum(beta) == 1)
##prob3.1 <- Problem(objective, constraints)
##CVXR_solution3.1 <- solve(prob3.1)
##CVXR_solution3.1$status
##value <- CVXR_solution3.1$getValue(beta)
##value
##sum(value)

context("WCC.CVXR")
described.class <- WCC.CVXR
set.seed(12345)
n <- 1e2
Z <- rnorm(n)

pred <- cbind(Z, Z^2, Z^3, Z^4, Z^5)
true_params <- c(.2, .8, 0, 0, 0)

Y <- pred %*% true_params + rnorm(n, 0, 0.1)
num_params <- length(true_params)
initial_weights <- rep(1/num_params, num_params)
libraryNames <- c('a', 'b', 'c', 'd', 'e')

context(" initialize")
#=====================================================================
test_that("it should initialize and set the correct default initial weights", {
  subject <- described.class$new(weights.initial = initial_weights)
  expect_equal(subject$get_weights, initial_weights)
})

context(" process")
#=====================================================================
test_that("it should compute the correct convex combination", {
  subject <- described.class$new(weights.initial = initial_weights)
  subject$process(pred, Y, libraryNames)
  expect_length(subject$get_weights, length(true_params))

  # Check that we approximate the true parameters
  difference <- abs(subject$get_weights - true_params)
  expect_true(all(difference < 1e-2))
})

test_that("it should compute the correct convex combination with random initial weights", {
  set.seed(12345)
  weights <- runif(num_params, 0, 1)
  weights <- weights / sum(weights)
  subject <- described.class$new(weights.initial = weights)
  subject$process(pred, Y, libraryNames)

  expect_length(subject$get_weights, length(true_params))

  # Check that we approximate the true parameters
  difference <- abs(subject$get_weights - true_params)
  expect_true(all(difference < 1e-2))
})

test_that("it should give a warning whenever the optimization_solution is not optimal", {
  subject <- described.class$new(weights.initial = initial_weights)
  mock_solve <- function(...) {
    list(
      status = 'infeasible',
      getValue = function(...) { true_params }
    )
  }

  with_mock(solve = mock_solve,
    expect_warning(
      subject$process(pred, Y, libraryNames),
      "The optimization problem was not solved optimally!",
      fixed=TRUE
    )
  )

})

test_that("it should take into account the earlier predicted weights", {
  set.seed(12345)
  n <- 10
  Z <- rnorm(n)
  pred <- cbind(Z, Z^2, Z^3, Z^4, Z^5)
  true_params <- c(.2, .8, 0, 0, 0)
  Y <- pred %*% true_params
  num_params <- length(true_params)
  initial_weights <- rep(1/num_params, num_params)
  libraryNames <- c('a', 'b', 'c', 'd', 'e')

  subject <- described.class$new(weights.initial = initial_weights)
  ## Increase the number of observations
  for(i in seq(10000)) {
    subject$increment_nobs
  }
  fake_but_very_important_weights <- c(0,0,0.3,.3,.4)
  subject$set_weights(fake_but_very_important_weights)

  ## This is a very simple test, and we should probably elaborate it. What
  ## we're currently doing is simulating that after 10000 iterations the alphas
  ## are fake_but_very_important_weights. Then we actually run the
  ## optimization, but in essence it should not matter at all (as the fake
  ## weights are 10000 times more important). This is a silly test, but does at
  ## least some checking.
  subject$process(pred, Y, libraryNames)
  difference <- abs(subject$get_weights - fake_but_very_important_weights)
  expect_true(all(difference < 1e-4))
})

