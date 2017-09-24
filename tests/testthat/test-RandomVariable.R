context("RandomVariable.R")
described.class <- RandomVariable
log <- Arguments$getVerbose(-8, timestamp=TRUE)
log <- FALSE

context(" initialize")
test_that("it should throw a warning whenever an interaction term is included", {
  expect_that(described.class$new(formula = a ~ x + y + z*a, family='gaussian'), gives_warning())
})
test_that("it should work with no dependend variables", {
  subject <- described.class$new(formula = a ~ ., family='gaussian')
  expect_equal(subject$getX, c())
})

context(" getFamily")
test_that("it should return the family of the subject", {
  family <- 'gaussian'
  rv <- described.class$new(formula = (W ~ W_lag_1), family = family)
  expect_equal(family, rv$getFamily)
})

context(" getX")
test_that("it should return the covariates", {
  family <- 'gaussian'
  rv <- described.class$new(formula = (W ~ A + W_lag_1), family = family)
  expect_equal(rv$getX, c(A='A', W_lag_1='W_lag_1'))
})
test_that("it should not include interactions", {
  family <- 'gaussian'
  suppressWarnings(
    rv <- described.class$new(formula = (W ~ A + W_lag_1 + A*Y), family = family)
  )
  expect_equal(rv$getX, c(A = 'A', W_lag_1 = 'W_lag_1', Y = 'Y'))
})

context(" getY")
test_that("it should return the outcome", {
  family <- 'gaussian'
  rv <- described.class$new(formula = (W ~ W_lag_1), family = family)
  expect_equal(rv$getY, c(W='W'))
})

context(" getValidity")
test_that("it should throw if the provided formula is not a formula", {
  expect_error(described.class$new(formula = 'not a formula', family='gaussian'), 
    "Argument 'formula' is neither of nor inherits class formula: character", fixed=TRUE
  )
})
test_that("it should throw if a provided family is not supported", {
  expect_error(described.class$new(formula = a ~ x + y , family='this-family'), 'Provided family this-family not supported')
})
test_that("it should not throw if a provided family is not supported", {
  for (family in RandomVariable.get_supported_families()) {
    expect_error(described.class$new(formula = a ~ x + y , family=family), NA)
  }
})

context(" get_formula")
test_that("it should create a formula with delta output", {
  subject <- described.class$new(formula = (W ~ W_lag_1), family = 'gaussian')
  result <- subject$get_formula_string(Y='delta')
  expect_equal(result, 'delta ~ W_lag_1')
})

test_that("it should create a formula with delta input", {
  subject <- described.class$new(formula = (W ~ W_lag_1), family = 'gaussian')
  result <- subject$get_formula_string(X='delta')
  expect_equal(result, 'W ~ delta')
})

test_that("it should retrieve the initial formula", {
  subject <- described.class$new(formula = (W ~ W_lag_1), family = 'gaussian')
  result <- subject$get_formula_string()
  expect_equal(result, 'W ~ W_lag_1')
})

context(" parseFormula")
test_that("it should parse the provided formula in the correct (in)dependent variables", {
 formula <- Y ~ A + B + C + D 
 expected <- list(Y=c(Y = 'Y'), X= c(A = 'A', B = 'B', C = 'C', D = 'D'))
 rv <- described.class$new(formula = formula, family = 'binomial')
 result <- rv$parseFormula(formula)
 expect_true(is.a(result, 'list'))
 expect_equal(names(result), c('Y', 'X'))
 expect_equal(result, expected)
})

context(" Static functions")
context("  RandomVariable.get_supported_families")
test_that("it should return the family of the subject", {
  family <- 'gaussian'
  families <- RandomVariable.get_supported_families
  expect_false(is.null(families))
  expect_true(length(families) > 0)
})

context("  RandomVariable.find_ordering")
test_that("it should return the correct ordered list", {
  W <- described.class$new(formula = (W ~ W_lag_1), family = 'gaussian')
  A <- described.class$new(formula = (A ~ W), family = 'binomial')
  Y <- described.class$new(formula = (Y ~ A + W), family = 'gaussian')
  randomVariables <- c(Y,A,W)
 
  result <- RandomVariable.find_ordering(randomVariables, log)
  expect_false(is.null(result))
  expect_true(length(result) == length(randomVariables))
  expect_equal(result[[1]], W) 
  expect_equal(result[[2]], A) 
  expect_equal(result[[3]], Y) 
})

test_that("it should set the correct names on the array", {
  #log <- FALSE
  W <- described.class$new(formula = (W ~ W_lag_1), family = 'gaussian')
  A <- described.class$new(formula = (A ~ W), family = 'binomial')
  Y <- described.class$new(formula = (Y ~ A + W), family = 'gaussian')
  randomVariables <- c(Y,A,W)
 
  result <- RandomVariable.find_ordering(randomVariables, log)
  expect_false(is.null(result))
  expect_true(length(result) == length(randomVariables))
  expect_equal(names(result), c('W','A','Y'))
})

test_that("it should throw if no ordering is avaiable", {
  W <- described.class$new(formula = (W ~ A), family = 'gaussian')
  A <- described.class$new(formula = (A ~ Y), family = 'binomial')
  Y <- described.class$new(formula = (Y ~ W), family = 'gaussian')
  randomVariables <- c(Y,A,W)
  expect_error(RandomVariable.find_ordering(randomVariables),
               'Intractable problem! There are interdependencies that cannot be solved!')
})

test_that("it should also work if there are no inter dependencies", {
  W <- described.class$new(formula = (W ~ W_lag_1), family = 'gaussian')
  randomVariables <- c(W)
 
  result <- RandomVariable.find_ordering(randomVariables)
  expect_false(is.null(result))
  expect_true(length(result) == length(randomVariables))
  expect_equal(result[[1]], W) 
})
