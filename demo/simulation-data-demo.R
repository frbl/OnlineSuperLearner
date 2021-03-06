devtools::load_all(".")

# Turn off asking for enter
par(ask=FALSE)

library('magrittr')
library('foreach')
library("dplyr")
library('doParallel')

set.seed(1234)

## Make sure we use all cores
registerDoParallel(cores = parallel::detectCores())

log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)

## Generate a dataset we will use for testing.
training_set_size <- 1e3

initial_data_size <-  training_set_size / 2

## What is the maximum number of iterations the OSL can use while going over the data?
## Note that in this case we split the data in equal parts with this number of iterations
max_iterations <- 50

## Specify the intervention we'd like to test, and also specify when we want to
## test this intervension

interventions <- list(
  list(variable = 'A', when = c(1), what = c(1)),
  list(variable = 'A', when = c(1), what = c(0))
)


## B is the number of iterations we'll run before we hope to converge
B <- 100

## Initialize the simulator
##-------------------------
sim <- Simulator.RunningExample$new()

data.train <- sim$simulateWAY(numberOfBlocks = training_set_size)

# Write CSV
#write.csv(data.train, file = "simData.csv",row.names=FALSE)
True_Psi <- mean(data.train$YA1 - data.train$YA0)
cat(" True_Psi:", True_Psi, "\n")

# Create the relevant variables
##-----------------------------

## We'd like to use the following features in our estimation:
## TODO: When adding multiple people, include w1
## REMOVING W1 for now!
w2 <- RelevantVariable$new(formula = w2 ~ Y_lag_1 + A_lag_1 + w2_lag_1 + w3_lag_1,  family = 'gaussian')
w3 <- RelevantVariable$new(formula = w3 ~ w2 + Y_lag_1 + A_lag_1 +  w2_lag_1 + w3_lag_1,  family = 'gaussian')
A <-  RelevantVariable$new(formula = A ~ w3 + w2 + Y_lag_1 + A_lag_1 +  w2_lag_1 + w3_lag_1, family = 'binomial')
Y <-  RelevantVariable$new(formula = Y ~ A + w3 + w2, family = 'gaussian')

relevantVariables <- c(w2, w3, A, Y)

## Define a list of algorithms to use
algos <- list()
algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                 params = list(nbins = c(5, 10, 15), online = TRUE))))

algos <- append(algos, list(list(algorithm = "ML.NeuralNet",
                                 params = list(nbins = c(5), online = TRUE))))

algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                 params = list(nbins = c(15), online = TRUE))))
algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                 params = list(nbins = c(5), online = TRUE))))
algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                 algorithm_params = list(alpha = seq(0,1,0.2)),
                                 params = list(nbins = c(5), online = TRUE))))


## Fit the actual OSL
##-------------------
osl <- OnlineSuperLearner::fit.OnlineSuperLearner(
  formulae = relevantVariables, ## Specify which are the formulae we expet
  data = data.train, ## Specify the data to train on
  algorithms = algos, ## SPecify the correct algorithms
  verbose = log, ## Logging information
  bounds = TRUE, ## Let the OSL generate the bounds based on the data it gets
  test_set_size = 5 + (3 * 3 + 3), ## The size of the minibatch test size. Note that for this test set size it is super important that at least enough observations are available as
  initial_data_size = training_set_size / 2, ## Train the first iteration (Nl) on this part of the data
  max_iterations = max_iterations, ## Use at most max_iterations over the data
  mini_batch_size = 30 ## Split the remaining data into N-Nl/max_iterations equal blocks of data
)

## Create a quick overview of the training curve (the risk over time)
OutputPlotGenerator.create_training_curve(
  osl$get_historical_cv_risk,
  relevantVariables = relevantVariables,
  output = 'curve'
)


## First we create the calculator to determine the intervention effects with.
intervention_effect_caluculator = InterventionEffectCalculator$new(
  bootstrap_iterations = B,
  outcome_variable = Y$getY,
  verbose = log,
  parallel = TRUE
)

results <- lapply(interventions, function(intervention) {
  tau <- 1
  training_set_size <- nrow(data.train) / 2

  ## We need to have data that includes the summary measures for the evaluation
  ## generate them here
  data.train <- Data.Static$new(dataset = data.train)
  osl$get_summary_measure_generator$set_trajectories(data.train)
  data.train.set <- osl$get_summary_measure_generator$getNext(2)

  intervention_effects <- lapply(c(TRUE, FALSE), function(discrete) {

    ## Actually evaluate the intervention for the discrete superlearner
    intervention_effect <- intervention_effect_caluculator$evaluate_single_intervention(
      osl = osl,
      intervention = intervention,
      discrete = TRUE,
      initial_data = data.train.set$traj_1[1,],
      tau = tau
    )
  })

  result.dosl = intervention_effects[[1]]
  result.osl = intervention_effects[[2]]


  data <- list(dosl = result.dosl, osl = result.osl)
})

result.dosl = results[[1]]$dosl - results[[2]]$dosl
result.osl = results[[1]]$osl - results[[2]]$osl
result.truth = data.train$YA1 - data.train$YA0
mean(data.train$YA1 - data.train$YA0)

data <- list(truth = result.truth[1:100], dosl = result.dosl, osl = result.osl)
OutputPlotGenerator.create_convergence_plot(data = data, output = 'convergence')

