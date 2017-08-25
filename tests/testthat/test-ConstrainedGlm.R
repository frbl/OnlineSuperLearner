context("ConstrainedGlm.R")
test_that("it should create a GLM based on the formula, delta, and data, and should respect the boundaries", {
  set.seed(12345)
  nobs <- 1000

  x1 = rnorm(nobs,0,1)
  x2 = rnorm(nobs,3,1)
  x3 = rnorm(nobs,10,1)
  Y = expit(x1) > 0.5
  data <- data.table(
    x1 = x1,
    x2 = x2,
    x3 = x3,
    Y = Y
  )
  delta <- 0.25
  form <- Y ~ x1 + x2 + x3
  for(delta in c(0.05, 0.25, 0.4)) {
    suppressWarnings({
      the_glm <- ConstrainedGlm.fit(formula = form, delta = delta, data = data)
      predictions <- predict(the_glm, type='response')
    })
    expect_lte((1-delta) - max(predictions), 0.01)
    expect_lte((delta) - min(predictions), 0.01)
  }
})


