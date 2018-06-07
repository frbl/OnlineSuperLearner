devtools::load_all(".")

#' @importFrom condensier condensier_options
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach

set.seed(12345)

## Make sure we use all cores
doParallel::registerDoParallel(cores = parallel::detectCores())

## Generate observations for training
llW <- list(stochMech=function(numberOfBlocks) {
              rnorm(numberOfBlocks, 0, 10)
            },
            param=c(0, 0.5, -0.25, 0.1),
            rgen=identity)

llA <- list(
  stochMech=function(ww) {
      rbinom(length(ww), 1, expit(ww))
  },
  param=c(-0.1, 0.1, 0.25),
  rgen=function(xx, delta=0.05){
    probability <- delta+(1-2*delta)*expit(xx)
    rbinom(length(xx), 1, probability)
  }
)

llY <- list(rgen={function(AW){
  aa <- AW[, "A"]
  ww <- AW[, grep("[^A]", colnames(AW))]
  mu <- aa*(0.4-0.2*sin(ww)+0.05*ww) +
    (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
  mu <- aa*(0.9) + (1-aa)*(0.3)
  rnorm(length(mu), mu, sd=0.1)}})


## We'd like to use the following features in our estimation:
W <- RelevantVariable$new(formula = Y ~ A + W, family = 'gaussian')
A <- RelevantVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
Y <- RelevantVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
relevantVariables <- c(W, A, Y)

## Generate a dataset we will use for testing.
training_set_size <- 1e3
test_set_size <- 100

sim  <- Simulator.GAD$new()

log = FALSE

## What is the maximum number of iterations the OSL can use while going over the data?
## Note that in this case we split the data in equal parts with this number of iterations
max_iterations <- 10

## Generate some fake data for testing and training
data.train <- sim$simulateWAY(training_set_size, qw=llW, ga=llA, Qy=llY, verbose=log)
data.test <- sim$simulateWAY(test_set_size, qw=llW, ga=llA, Qy=llY, verbose=log)

## Define a list of algorithms to use
algos <- list()
algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                 params = list(nbins = c(5, 10, 15), online = TRUE))))

algos <- append(algos, list(list(algorithm = "ML.NeuralNet",
                                 params = list(nbins = c(5, 10, 15), online = TRUE))))

## Specify the intervention we'd like to test, and also specify when we want to
## test this interventsion
intervention <- list(variable = 'A', when = c(2), what = c(1))
tau = 2

## Fit the actual OSL
osl <- OnlineSuperLearner::fit.OnlineSuperLearner(
  formulae = relevantVariables, ## Specify which are the formulae we expet
  data = data.train, ## Specify the data to train on
  algorithms = algos, ## SPecify the correct algorithms
  verbose = log, ## Logging information
  bounds = TRUE, ## Let the OSL generate the bounds based on the data it gets
  test_set_size = 5 + (3 * 3 + 3), ## The size of the minibatch test size

  initial_data_size = training_set_size / 2, ## Train the first iteration (Nl) on this part of the data
  max_iterations = max_iterations, ## Use at most max_iterations over the data
  mini_batch_size = (training_set_size / 2) / max_iterations ## Split the rememaining data into N-Nl/max_iterations equal blocks of data
)

## Sample data from it
preds <- sampledata(osl, newdata = data.test, relevantVariables, plot = TRUE)
preds


## Define kolmogorov-smirnov test
T_iter <- 10
B_iter <- 100
nbins <- 5
n_A_bins <- 2

subject <- ConditionalDensityEvaluator$new(log)
result <- subject$evaluate(
  osl,
  sim,
  T_iter, 
  B_iter,
  nbins= nbins
)

