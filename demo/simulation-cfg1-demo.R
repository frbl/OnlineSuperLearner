#devtools::install_deps(dependencies = TRUE)
devtools::load_all(".")
library('magrittr')
library('doParallel')
library('foreach')
#' @importFrom condensier condensier_options
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreachs
#' 

set.seed(12345)
log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)

## Make sure we use all cores
doParallel::registerDoParallel(cores = parallel::detectCores())

## Generate observations for training
## These are used in the simulator / its scheme.
llW <- list(
  stochMech=function(numberOfBlocks) {
    rnorm(numberOfBlocks, 0, 10)
  },
  param=c(0, 0.5, -0.25, 0.1),
  rgen=identity
)

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
  rnorm(length(mu), mu, sd=0.1)}}
)


## We'd like to use the following features in our estimation:
W <- RelevantVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
A <- RelevantVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
Y <- RelevantVariable$new(formula = Y ~ A + W, family = 'gaussian')
relevantVariables <- c(W, A, Y)

## Specify the size of our data set for training and testing
training_set_size <- 1e3
test_set_size <- 100

## Create a new simulator
sim <- Simulator.GAD$new()

## What is the maximum number of iterations the OSL can use while going over the data?
## Note that in this case we split the data in equal parts with this number of iterations
max_iterations <- 10

## Generate some fake data for testing and training
data.train <- sim$simulateWAY(training_set_size, qw=llW, ga=llA, Qy=llY, verbose=log)
data.test <- sim$simulateWAY(test_set_size, qw=llW, ga=llA, Qy=llY, verbose=log)

## Define a list of algorithms to use
algos <- list()
#algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                 #params = list(nbins = c(5, 10, 15), online = TRUE))))

algos <- append(algos, list(list(algorithm = "ML.NeuralNet",
                                 params = list(nbins = c(5), online = TRUE))))

algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                 params = list(nbins = c(5), online = TRUE))))

## Fit the actual OSL
osl <- OnlineSuperLearner::fit.OnlineSuperLearner(
  formulae = relevantVariables, ## Specify which are the formulae we expet
  data = data.train, ## Specify the data to train on
  algorithms = algos, ## SPecify the correct algorithms
  verbose = log, ## Logging information
  bounds = TRUE, ## Let the OSL generate the bounds based on the data it gets
  test_set_size = 5 + (3 * 3 + 3), ## The size of the minibatch test size. Note that for this test set size it is super important that at least enough observations are available as 
  initial_data_size = training_set_size / 2, ## Train the first iteration (Nl) on this part of the data
  max_iterations = max_iterations, ## Use at most max_iterations over the data
  mini_batch_size = (training_set_size / 2) / max_iterations ## Split the remaining data into N-Nl/max_iterations equal blocks of data
)

## Specify the intervention we'd like to test, and also specify when we want to
## test this intervention
intervention <- list(variable = 'A', when = c(2), what = c(1))
## Tau is the time at which we want to test the intervention
tau <-  2
## B is the number of iterations we'll run before we hope to converge
B <- 100

## First we simulate data given the intervention. That is, we specify in our
## simulation that we want to sample data when this intervention would be
## applied. After that we take the mean at tau, and as such approximate our
## treatment effect in the true population.
cat('Approximating truth...\n')
result.approx <- foreach(i=seq(B)) %do% {
  cat('Approximating truth in iteration (under intervention): ', i, '\n')
  data.int <- sim$simulateWAY(tau, qw = llW, ga = llA, Qy = llY,
                                  intervention = intervention, verbose = FALSE)
  data.int$Y[tau]
} %>% unlist %>% mean


## The next step is to actually calculate the same intervention using the
## superlearner. We use a similar technique for this, as we try to calculate
## the mean of the intervention effects.

## We need to have data that includes the summary measures for the evaluation
## generate them here
data.train <- Data.Static$new(dataset = data.train)
osl$get_summary_measure_generator$set_trajectories(data.train)
data.train.set <- osl$get_summary_measure_generator$getNext(2)

intervention_effects <- lapply(c(TRUE, FALSE), function(discrete) {
  ## First we create the calculator to determine the intervention effects with.
  intervention_effect_caluculator = InterventionEffectCalculator$new(
    bootstrap_iterations = B, 
    outcome_variable = Y$getY,
    verbose = log,
    parallel = TRUE
  )


  ## Actually evaluate the intervention for the discrete superlearner
  intervention_effect <- intervention_effect_caluculator$evaluate_single_intervention(
    osl = osl,
    intervention = intervention, 
    discrete = TRUE, 
    initial_data = data.train.set$traj_1[1,],
    tau = tau
  ) %>% unlist %>% mean
  the_osl = ifelse(discrete, 'discrete osl', 'continuous osl')
  paste(the_osl,":", intervention_effect, '\n')
})

## Finally we run our kolmogorov smirnov test example to check whether we
## actually do a good job estimating the true conditional distributions.
## Define kolmogorov-smirnov test
T_iter <- 10
B_iter <- 100
nbins <- 5
n_A_bins <- 2

## Define the object that will be used to run the evalutation, and run the actual evaluations.
subject <- ConditionalDensityEvaluator$new(log, osl = osl, summary_measure_generator = osl$get_summary_measure_generator)
result <- subject$evaluate(
  sim,
  T_iter, 
  B_iter,
  nbins = nbins
)

## Output the evaluation.
flat_result <- result %>% unlist %>% unname
flat_result <- flat_result[!is.na(flat_result)]
perc_significant <- sum(flat_result >= 0.95) / length(flat_result) * 100 %>% round(., 2)
perc_significant <- perc_significant %>% round(., 2)
paste(perc_significant,'% significant in the KS-test')

cat('The effects of the interventions were:')
cat(paste('approx',':', result.approx)) 
for(result in intervention_effects) { cat(result) }
