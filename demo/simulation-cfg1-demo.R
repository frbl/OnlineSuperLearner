#' @importFrom condensier condensier_options
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach

set.seed(12345)

######################################
# Generate observations for training #
#####################################
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


# We'd like to use the following features in our estimation:
W <- RandomVariable$new(formula = Y ~ A + W, family = 'gaussian')
A <- RandomVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
Y <- RandomVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
randomVariables <- c(W, A, Y)

# Generate a dataset we will use for testing.
margin <- 100
training_set_size <- 1e3
test_set_size <- 100
sim  <- Simulator.GAD$new()
log = FALSE
data.test <- sim$simulateWAY(test_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=log)
data.train <- sim$simulateWAY(training_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=log)

# Create the bounds
bounds <- PreProcessor.generate_bounds(data.train)

# Create the measures we'd like to include in our model
# In this simulation we will include 2 lags and the latest data (non lagged)
# Define the variables in the initial dataset we'd like to use
#private$train(data.test, data.train, bounds, randomVariables, 2)

intervention <- list(variable = 'A', when = c(2), what = c(1))
tau = 2
#train(data.test, data.train, bounds, randomVariables, Y,  max_iterations = 2, llW = llW, llA = llA, llY = llY)
