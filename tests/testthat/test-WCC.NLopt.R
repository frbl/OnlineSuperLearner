context("WCC.NLopt.R")
described.class <- WCC.NLopt

context(" compute")
# Initialize
K <- 10
nobs <- 20
obsWeights <- rep(1/K, K)
subject<- described.class$new(obsWeights) 

test_that("it should return a vector of weights, with a sum of 1", {
 set.seed(1234)
 Y <- seq(nobs)
 Z <- matrix(rep(Y, K), byrow=F, ncol=K)
 Z <- Z + rnorm(K * nobs, mean=0, sd=0.001)
 subject<- described.class$new(obsWeights) 
 result <- subject$compute(Z,Y)

 expect_type(result,'double')
 expect_equal(length(result), K)
 expect_equal(sum(result),1)
  
})

test_that("it should create the best weighted combination and should return the params", {
 set.seed(1234)
 Y <- seq(nobs)
 Z <- matrix(rep(Y,K), byrow=F, ncol=K)
 Z[,1] <- 0 
 Z[,2] <- 1 
 Z[,3] <- Z[,3] + rnorm(nobs, mean=0, sd=0.1)
 Z[,4] <- Z[,4] + rnorm(nobs, mean=0, sd=0.1)
 Z[,5] <- Z[,5] + rnorm(nobs, mean=0, sd=0.01)
 Z[,6] <- Z[,6] + rnorm(nobs, mean=0, sd=0.01)
 Z[,7] <- Z[,7] + rnorm(nobs, mean=0, sd=0.01)
 Z[,8] <- Z[,8] + rnorm(nobs, mean=0, sd=0.01)
 Z[,9] <- Z[,9] + rnorm(nobs, mean=0, sd=0.0001)
 Z[,10] <- Z[,10] + rnorm(nobs, mean=0, sd=0.00000000001)
 result <- subject$compute(Z,Y)

 # The last two have the least noise, so they should have the highest weights
 best.idx <- which(result == max(result))
 expect_equal(best.idx, 10)

 result[best.idx] <- NA
 best.idx <- which(result == max(result, na.rm=TRUE))
 expect_equal(best.idx, 9)

 # The first two add no information, so they should probably be very close to zero
 expect_lt((result[1] + result[2]), 0.1)
})

test_that("it should update the obsWeights in the object", {
 set.seed(1234)
 Y <- seq(nobs)
 Z <- matrix(rep(Y, K), byrow=F, ncol=K)
 Z <- Z + rnorm(K * nobs, mean=0, sd=0.001)
 subject<- described.class$new(obsWeights) 
 result <- subject$compute(Z,Y)
 expect_false(all(obsWeights == subject$getWeights))
 expect_true(all(result == subject$getWeights))
})
