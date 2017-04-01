context("WeightedCombinationComputer.R")

context(" initialize")
test_that("it should initialize", {
 K <- 10
 obsWeights <- rep(1/K, K)
 expect_error(WeightedCombinationComputer$new(obsWeights), NA) 
})

context(" getWeights")
test_that("it should return the set weights", {
 K <- 10
 obsWeights <- rep(1/K, K)
 subject<- WeightedCombinationComputer$new(obsWeights) 
 expect_equal(subject$getWeights, obsWeights )
})

context(" process")
test_that("it should throw an error", {
 K <- 10
 obsWeights <- rep(1/K, K)
 subject<- WeightedCombinationComputer$new(obsWeights) 
 expect_error(subject$process(K, K, K), 'This method is not implemented, please inherit this class and implement it.')
})
