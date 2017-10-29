library(mockery)
context("ConstrainedGlm.R")

set.seed(12345)
nobs <- 100
cov1 <- rnorm(nobs, 0, 1)
cov2 <- rnorm(nobs, 1, 1)
cov3 <- ifelse(cov1 > 0, rbinom(nobs,1, 0.999), rbinom(nobs,1, 0.3))
cov4 <- rnorm(nobs, 3, 1)
y <- as.numeric(expit(0.1*cov1 - 0.8*cov2 - 0.1*cov3 + 0.4*cov4 + rnorm(nobs,0,10)) > 0.5)
dat <- data.table(cov1, cov2, cov3, cov4, y)

context(" ConstrainedGlm.fit")
test_that("it should create a GLM based on the formula, delta, and data, and should respect the boundaries", {
  formula <- y ~ cov1 + cov2 + cov3 + cov4
  for(delta in c(0.05, 0.1, 0.15)) {
    the_glm <- ConstrainedGlm.fit(formula = formula, delta = delta, data = dat)
    predictions <- predict(the_glm, type='response')
    expect_lte(max(predictions), 1- delta)
    expect_gte(min(predictions), delta)
  }
})

test_that("it should throw a warning whenever the data contains NA values", {
  data <- data.table(x = c(1,2,NA), y = c(1,0,1))
  expect_warning(ConstrainedGlm.fit(formula = formula(y~x), delta = 0, data = data),
                 'Data contains NA values!')
})

test_that("it should, with a delta of 0, return the same as a normal glm", {
  set.seed(12345)
  x <- rep(1/2, 10)
  y <- rbinom(10, size = 1, prob=x)
  dat <- data.frame(x=x, y=y)

  formula <- y ~ x
  fit1 <- glm(formula, family=binomial(), data=dat)
  fit2 <- ConstrainedGlm.fit(formula, delta=0, data=dat)
  expect_equal(coef(fit1), coef(fit2))
})

test_that("it should return the same as a glm with a delta of 0 and more complex data", {
    formula <- y ~ cov1 + cov2 + cov3 + cov4
    fit1 <- glm(formula, family=binomial(), data=dat)
    fit2 <- ConstrainedGlm.fit(formula, delta=0, data=dat)
    expect_equal(coef(fit1), coef(fit2))
})

test_that("it should work with the data from a file", {
  url <- system.file("testdata",'test-fit-glm-and-constrained-glm.csv',package="OnlineSuperLearner")
  read_data <- read.csv(url) %>% as.data.table
  formula <- Delta ~ Y_lag_1

  hide_warning_probabilities_numerically_zero_or_one(
    hide_warning_convergence({
      fit1 <- glm(formula, family=binomial(), data=read_data)
      fit2 <- ConstrainedGlm.fit(formula, delta=0, data=read_data)
    })
  )
  expect_equal(coef(fit1), coef(fit2))
})

test_that("it should create a new glm if no previous one was provided", {
  true_formula <- y ~ cov1 + cov2 + cov3 + cov4

  m <- mock(function(formula, family, data, fall_back_to_glm, ...) return(42))
  not_called <- mock(function(...) return(911))

  with_mock(ConstrainedGlm.update_glm = not_called, { 
    with_mock(ConstrainedGlm.fit_new_glm = m, { 
      ConstrainedGlm.fit(true_formula, delta=0.5, data=dat)
    })
  })

  expect_called(not_called,0)
  expect_called(m,1)
  args <- mock_args(m)[[1]]

  expect_equal(args$formula, true_formula)
  expect_is(args$family, 'family')
  expect_equal(args$data, dat)
  expect_equal(args$fall_back_to_glm, TRUE)
})

test_that("it should create the correct family based on the delta", {
  true_formula <- y ~ cov1 + cov2 + cov3 + cov4
  m <- mock(function(formula, family, data, fall_back_to_glm, ...) return(42))

  with_mock(ConstrainedGlm.fit_new_glm = m, { 
    ConstrainedGlm.fit(true_formula, delta=0.5, data=dat)
  })

  args <- mock_args(m)[[1]]

  expect_is(args$family, 'family')
  expect_equal(args$family$link, 'bounded-logit')

  expect_false(is.null(args$family$linkfun))
  expect_false(is.null(args$family$linkinv))
  expect_false(is.null(args$family$mu.eta))

  expect_false(is.null(args$family$valideta))
  expect_false(is.null(args$family$validmu))

  ## It should always return a valid eta
  expect_true(args$family$valideta(1))
})


test_that("it should update the previous glm if provided to the function", {
  true_formula <- y ~ cov1 + cov2 + cov3 + cov4

  m <- mock(function(previous_glm, data, ...) return(42))
  not_called <- mock(function(...) return(911))
  previous_glm <- mock(1234)

  with_mock(ConstrainedGlm.update_glm = m, { 
    with_mock(ConstrainedGlm.fit_new_glm = not_called, { 
      ConstrainedGlm.fit(true_formula, delta=0, data=dat, previous_glm = previous_glm)
    })
  })

  expect_called(not_called,0)
  expect_called(m,1)
  args <- mock_args(m)[[1]]

  expect_equal(args$previous_glm, previous_glm)
  expect_equal(args$data, dat)
})

context(" ConstrainedGlm.fit_new_glm")
## Testing this from the fit function, to not generate the whole family here.
test_that("it should throw a warning whenever the initial glm fails (with delta), and fall back to the default glm", {
  data <- data.table(x = c(1,2,3,4), y = c(1,0,1,0))
  expect_warning(ConstrainedGlm.fit(formula = formula(y~x), delta = 0.3, data = data), 'Constrained GLM failed, using glm binomial')
  expect_warning(ConstrainedGlm.fit(formula = formula(y~x), delta = 0.3, data = data), 'Value 1.125 out of range (0, 1)', fixed = TRUE)
})

test_that("it should return the glm", {
  data <- data.table(x = c(1,2,3,4), y = c(1,0,1,0))
  result <- ConstrainedGlm.fit_new_glm(formula = formula(y~x), family = binomial, data = data)
  expect_is(result, 'glm')
})


context(" ConstrainedGlm.update_glm")
test_that("it should call the update function", {
  m <- mock(function(previous_glm, data) return(42))
  previous_glm <- mock(1234)

  with_mock(update = m, { 
    ConstrainedGlm.update_glm(previous_glm, dat)
  })

  expect_called(m,1)
  args <- mock_args(m)[[1]]

  expect_equal(args$object, previous_glm)
  expect_equal(args$data, dat)
})

test_that("it should throw if the glm is null", {
  expect_error(ConstrainedGlm.update_glm(NULL, dat), '!is.null(previous_glm) is not TRUE', fixed = TRUE)
})

context(" ConstrainedGlm.predict")
test_that("it should should throw whenever the glm is null", {
  expect_error(ConstrainedGlm.predict(NULL, dat), '!is.null(constrained_glm) is not TRUE', fixed = TRUE)
})

test_that("it should should throw whenever the data is null", {
  m <- mock(1234)
  expect_error(ConstrainedGlm.predict(m, NULL), '!is.null(newdata) is not TRUE', fixed = TRUE)
})

test_that("it should call the predict function with the glm", {
  set.seed(12345)
  nobs <- 100
  cov1 <- rnorm(nobs, 0, 1)
  cov2 <- rnorm(nobs, 1, 1)
  cov3 <- ifelse(cov1 > 0, rbinom(nobs,1, 0.999), rbinom(nobs,1, 0.3))
  cov4 <- rnorm(nobs, 3, 1)
  y <- as.numeric(expit(0.1*cov1 - 0.8*cov2 - 0.1*cov3 + 0.4*cov4 + rnorm(nobs,0,10)) > 0.5)
  dat <- data.table(cov1, cov2, cov3, cov4, y)
  previous_glm <- mock(1234)# glm(dat, formula = formula(y ~ cov1 + cov2 + cov3 + cov4), family= binomial())

  m <- mock(42)
  with_mock(predict = m, { ConstrainedGlm.predict(constrained_glm = previous_glm, newdata = dat)})

  expect_called(m,1)
  args <- mock_args(m)[[1]]

  expect_equal(args$object, previous_glm)
  expect_equal(args$newdata, dat)
  expect_equal(args$type, 'response')
})

