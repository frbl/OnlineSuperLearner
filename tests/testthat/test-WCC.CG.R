context("WCC.CG")
described.class <- WCC.CG
#===========================
test_that("initial test", {
  number_of_algorithms <- 5
  subject <- described.class$new(rep(0, number_of_algorithms))
  expect_equal(subject$get_weights, rep(1/number_of_algorithms, number_of_algorithms))
})

test_that("initial test", {
  number_of_algorithms <- 5
  library_names <- c('a','b','c','d','e')
  nobs <- 50

  Y <- rnorm(nobs)
  Z <- matrix(rnorm(number_of_algorithms * nobs), nobs, number_of_algorithms)

  subject <- described.class$new(rep(0, number_of_algorithms))
  result <- subject$process(Z, Y, library_names)
  print('')
  result %>% print
})

test_that("it should sum all weights to 1", {
  library_names <- c('a','b','c','d','e')
  number_of_algorithms <- length(library_names)
  nobs <- 50

  Y <- rnorm(nobs)
  Z <- matrix(rnorm(number_of_algorithms * nobs), nobs, number_of_algorithms)

  subject <- described.class$new(rep(0, number_of_algorithms))
  result <- subject$process(Z, Y, library_names)
  result %<>% sum
  expect_equal(result, 1)
})

test_that("it should only return positive weights", {
  library_names <- c('a','b','c','d','e')
  number_of_algorithms <- length(library_names)
  nobs <- 50

  Y <- rnorm(nobs)
  Z <- matrix(rnorm(number_of_algorithms * nobs), nobs, number_of_algorithms)

  subject <- described.class$new(rep(0, number_of_algorithms))
  result <- subject$process(Z, Y, library_names)
  expect_true(all(as.vector(result) >= 0))
})
