context("WeightedCombinationComputer.R")

context(" process")
test_that("it should throw an error", {
 K <- 10
 obsWeights <- rep(1/K, K)
 subject<- WeightedCombinationComputer$new(obsWeights) 
 expect_error(subject$process(K, K, K), 'This method is not implemented, please inherit this class and implement it.')
})
