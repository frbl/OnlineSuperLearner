context("WCC.SGD.R")
described.class <- WCC.SGD

test_that("it should work on a simple testcase", {
  described.class <- WCC.SGD
    # Dimensions
  N <- 1e3
  d <- 1e2

  # Generate data.
  set.seed(42)
  X <- matrix(rnorm(N*d), ncol=d)

  theta <- rep(5, d+1)
  eps <- rnorm(N)
  y <- cbind(1, X) %*% theta + eps

  set.seed(1)
  subject <- described.class$new(rep(NA, d))
  libraryNames <- stringi::stri_rand_strings(d, 5)
  sgd.theta <- subject$process(X, y, libraryNames)
})

