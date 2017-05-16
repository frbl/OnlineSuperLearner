context("Integration: SummaryMeasureGenerator-with-generators")
described.class <- SummaryMeasureGenerator

dataset <- data.table(Y= (seq(1,10)%%2) , A=rep(1,10), W=seq(10,1))
data <- Data.Static$new(dataset = dataset)

context(" update")
test_that("the update function should work for all provided smgs", {
  colnames <- c('W','A','Y')
  mylist <- c(SMG.Lag$new(lags = 2, colnames.to.lag = colnames),
              SMG.Latest.Entry$new(colnames.to.use = colnames))
  subject <- described.class$new(data= data, SMG.list = mylist)

  expect_true(is.a(subject, 'SummaryMeasureGenerator'))
  expect_true(is.a(subject$getCache, "data.table"))

  # Iteration 1
  pre_outcome <- subject$getNext()

  pre_outcome$W <- 123
  pre_outcome$A <- 0
  pre_outcome$Y <- 1

  expect_false(pre_outcome$W_lag_1 == 123)
  expect_false(pre_outcome$A_lag_1 == 0)
  expect_false(pre_outcome$Y_lag_1 == 1)

  post_outcome <- subject$getLatestCovariates(pre_outcome)
  expect_equal(length(pre_outcome), length(post_outcome))

  expect_equal(post_outcome$W_lag_1, 123)
  expect_equal(post_outcome$A_lag_1, 0)
  expect_equal(post_outcome$Y_lag_1, 1)

  expect_equal(post_outcome$W_lag_2, pre_outcome$W_lag_1)
  expect_equal(post_outcome$A_lag_2, pre_outcome$A_lag_1)
  expect_equal(post_outcome$Y_lag_2, pre_outcome$Y_lag_1)

  expect_true(all(is.na(c(post_outcome$W, post_outcome$A, post_outcome$Y))))

  # Iteration 2
  pre_outcome <- post_outcome

  pre_outcome$W <- 7128943
  pre_outcome$A <- 1
  pre_outcome$Y <- 0

  expect_false(pre_outcome$W_lag_1 == 7128943)
  expect_false(pre_outcome$A_lag_1 == 1)
  expect_false(pre_outcome$Y_lag_1 == 0)

  post_outcome <- subject$getLatestCovariates(pre_outcome)
  expect_equal(length(pre_outcome), length(post_outcome))

  expect_equal(post_outcome$W_lag_1, 7128943)
  expect_equal(post_outcome$A_lag_1, 1)
  expect_equal(post_outcome$Y_lag_1, 0)

  expect_equal(post_outcome$W_lag_2, pre_outcome$W_lag_1)
  expect_equal(post_outcome$A_lag_2, pre_outcome$A_lag_1)
  expect_equal(post_outcome$Y_lag_2, pre_outcome$Y_lag_1)

  expect_true(all(is.na(c(post_outcome$W, post_outcome$A, post_outcome$Y))))

})
