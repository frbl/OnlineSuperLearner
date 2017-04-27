context("WCC.NMBFGS.R")
described.class <- WCC.NMBFGS
set.seed(12345)
n <- 1e5
Z <- rnorm(n)

pred <- cbind(Z, Z^2, Z^3, Z^4)
true_params <- c(.2, .8, 0, 0)

Y <- pred %*% true_params +rnorm(n)
initial_weights <- c(1/4, 1/4, 1/4, 1/4)
libraryNames <- c('a', 'b', 'c', 'd')

test_that("it should set the default parameters when not yet processed", {
  subject <- described.class$new(weights.initial = initial_weights)
  expect_equal(subject$get_weights, initial_weights)
})

test_that("it should compute the correct convex combination", {
  set.seed(12345)
  subject <- described.class$new(weights.initial = initial_weights)
  subject$process(pred, Y, libraryNames)

  # Check that we approximate the true parameters
  difference <- subject$get_weights - true_params
  expect_true(all(difference < 1e-2))
})

test_that("it should throw if something other than a function is provided", {
  expect_error(
    described.class$new(weights.initial = initial_weights, function_to_optimize = c(1,2,3)),
    "Mode of 'fun' should be 'function', not numeric"
  )
})

test_that("it should throw if the epsilon provided is not correct", {
  expect_error(
    described.class$new(weights.initial = initial_weights, epsilon = 10),
    "Argument 'epsilon' is out of range [0,0.025]: 10",
    fixed=TRUE
  )
})

test_that("it should ", {
#n <- 1e6
#X <- rnorm(n)
#Y <- .2*X+.8*X^2+rnorm(n)
#pred <- cbind(X, X^2, X^3, X^4)

#cOpt <- convexOptimization$new()
#comp.time <- c()
#if (FALSE) {
  ###
  ### version 1
  ###
  #data <- list(Y = Y, pred = pred)
  #fun <- function(alpha, data) {
    #-2*t(alpha) %*% (t(data$pred) %*% data$Y) +
      #t(alpha) %*% t(data$pred) %*% data$pred %*% alpha
  #}
  #tic <- Sys.time()
  #optFirst <- cOpt$convexOpt(fun,
                             #init = c(1/4, 1/4, 1/4, 1/4),
                             #epsilon = 1e-3,
                             #method = "Nelder-Mead",
                             #data = data)
  #optSecond <- cOpt$convexOpt(fun,
                              #init = optFirst$par,
                              #epsilon = 1e-3,
                              #method = "BFGS",
                              #data = data)
  #tac <- Sys.time()
  #comp.time <- c(compt.time, tac - tic)
#}
###
### version 2
###
#data <- list(Qa = t(pred) %*% Y, Qb=t(pred) %*% pred)
#fun <- function(alpha, data) {
  #-2*t(alpha) %*% data$Qa + t(alpha) %*% data$Qb %*% alpha
#}
#tic <- Sys.time()
#optFirst <- cOpt$convexOpt(fun,
                           #init = c(1/4, 1/4, 1/4, 1/4),
                           #epsilon = 1e-3,
                           #method = "Nelder-Mead",
                           #data = data)
#optSecond <- cOpt$convexOpt(fun,
                            #init = optFirst$par,
                            #epsilon = 0,
                            #method = "BFGS",
                            #data = data)
#tac <- Sys.time()
#comp.time <- c(comp.time, tac - tic)

  
})
