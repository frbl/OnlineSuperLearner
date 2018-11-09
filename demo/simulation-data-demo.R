# if and with how much a binary treatment (antidepressant usage) attributes to changes in  depression.
#We  use  a  normalized  hypothetical  set  of  sum  scores  of  the  Inventory  of  DepressiveSymptomatology  (IDS)
#questionnaire with  a  possible  range  of  0−100.
#Anti-depressant  usage  is a  binary  variable  sampled  from  a  binomial  distribution.

#w1 gender 0/1
#w2 quality of sleep 0/1
#w3 level of activity (PAL value) between 1.4 and 2.4
#A antidepressant = 1 no antdepressant = 0
#Y IDS (Inventory of Depressive Symptomatology)
#Block time block
#Patient id patient identification
#Y_Y influence of Y on Y in the next block
#Y_A influence of Y on A in the next block
#Y_w2 influence of Y on w2 in the next blok
devtools::load_all(".")

library('magrittr')
library('foreach')
library("dplyr")
library(doParallel)

set.seed(1234)
registerDoParallel(cores = parallel::detectCores())


NOISE_SD <<- 0.1
NOISE_MEAN <<- 0

Getw3 <- function(w2){
  #if w2 = 1 (good sleep) the activity level will be higher then when w2 = 0 (bad sleep)
  if (w2 == 1) {
    min_val = 1.7
    max_val = 2.4
  } else {
    min_val = 1.4
    max_val = 2.0
  }
  return(runif(1, min = min_val, max = max_val))
}

min_max_scale <- function(x, max_val, minx, maxx){
   round((x - minx) / (maxx - minx) * max_val)
}


generateData0<-function (n,prob_w2) {

  Block <- 0
  patient_id <- seq.int(n)
  # Initialize gender (w1) man = 1 or woman = 0
  w1 <- rbinom(n, size = 1, prob = 0.5)

  # level of sleep good = 1 bad = 0
  w2 <- rbinom(n, size=1, prob = prob_w2)

  # level of activity PAL <1.4 is almost doing nothing PAL > 2.4 is very active
  #Extremely inactive Cerebral Palsy patient  <1.40
  #Sedentary  Office worker getting little or no exercise 1.40-1.69
  #Moderately active  Construction worker or person running one hour daily  1.70-1.99
  #Vigorously active  Agricultural worker (non mechanized) or person swimming two hours daily 2.00-2.40
  #Extremely active Competitive cyclist >2.40
  # bad sleep is reducing the level of activity

  Getw3Vectorized <- Vectorize(Getw3)
  w3 <- Getw3Vectorized(w2)

  A <- rbinom(n, size = 1, prob=plogis(-0.4 + 0.1 * w1 + 0.1 * w2 + 0.15 * w3 + 0.15 * w2 * w3))

  noise <- rnorm(n, mean = NOISE_MEAN, sd = NOISE_SD)

  # calculate the initial Y variable
  Y_main <- (-0.1 * w1 + 0.4 * w2 + 0.3 * w3) + noise
  Y <- 1 + A + Y_main
  # Also store the counterfactual outcomes
  YA0 <- 1 + 0 + Y_main
  YA1 <- 1 + 1 + Y_main

  minx<- min(c(Y, YA0, YA1))
  maxx<- max(c(Y, YA0, YA1))

  Y   <- min_max_scale(Y, 100, minx, maxx)
  YA0 <- min_max_scale(YA0, 100, minx, maxx)
  YA1 <- min_max_scale(YA1, 100, minx, maxx)

  data <- data.frame(Block, w1, w2, w3, A, Y, YA0, YA1, minx, maxx)
  data <- cbind(data, Patient_id = patient_id)
  return (data)
}

generateLagData <- function(simData_t0, ptn_id, to_block, prob_w2, n) {
  # If we are int the first iteration, we need to use the t0 data.
  cat('Generating data for patient', ptn_id,'\n')
  simData_t<-data.frame(Block = numeric(),
                        w1 = integer(),
                        w2 = integer(),
                        w3 = numeric(),
                        A  = integer(),
                        Y  = numeric(),
                        YA0 =numeric(),
                        YA1 = numeric(),
                        Patient_id = integer())
  row_dag  <- simData_t0 %>%
    filter(Patient_id == ptn_id) %>%
    data.frame(.)
  
  length_A_1 <- 0
  
  for(j in 1:to_block) {

    row_dag$Block <- row_dag$Block + 1
    prev_A <- row_dag$A
    prev_Y <- row_dag$Y

    ## calculate w2 depending on Y and previous W2
    ## if Y> 50 increase if Y<50 decrease probability
    delta_prob_w2 <- ifelse(prev_Y < 50, (prev_Y+1)/250, (prev_Y+1)/500)
    noise <- rnorm(n, mean = NOISE_MEAN, sd = NOISE_SD)
    prob_w2_max_1 <- prob_w2 + delta_prob_w2 + noise
    prob_w2_new <- ifelse(prob_w2_max_1 >0.99,1,prob_w2_max_1)
    
    row_dag$w2 <- rbinom(n, size = 1, prob = prob_w2_new)

    ## calculate w3 depending on w2
    ## categorize using the PAL scale
    ## function is neat, vectorization as in the first function is only needed when vectors are used
    row_dag$w3 <- Getw3(row_dag$w2)
    noise <- rnorm(n, mean = NOISE_MEAN, sd = NOISE_SD)
    row_dag$w3 <- row_dag$w3 + noise/10
    ## The use of A
    ## When A is 1 the chances increase that A will become 1 again
    ## after a period of A = 1 the chances increase that A will become 0 
    
    noise <- rnorm(n, mean = NOISE_MEAN, sd = NOISE_SD)
    if (prev_A == 1){
       if (length_A_1 < 10) {
       row_dag$A <- 1
       length_A_1 <- length_A_1+1
       } else{
         A_prob <- 0.1 * row_dag$w1+ 0.1 * row_dag$w2 + 0.15 * row_dag$w3 + 0.15 * row_dag$w2 * row_dag$w3 + prev_Y/100+noise
         ##print(A_prob)
         row_dag$A <- rbinom(n, size = 1, prob = plogis(A_prob))
         length_A_1 <- 0
       }
    } else { 
      A_prob <- -0.3 + 0.1 * row_dag$w1+0.1 * row_dag$w2 + 0.15 * row_dag$w3 + 0.15 * row_dag$w2 * row_dag$w3 - prev_Y + noise
      row_dag$A <- rbinom(n, size = 1, prob = plogis(A_prob))
    }
   
    
    ##n=1 when single patient_id is used
    noise <- rnorm(n, mean = NOISE_MEAN, sd = NOISE_SD)

    ##counter factual
    ##the longer A is used the higher Y becomes
    ##
    Y_main <- (-0.1 * row_dag$w1 + 0.4 * row_dag$w2 + 0.3 * row_dag$w3) +length_A_1/10+prev_Y/100 + noise
    row_dag$Y   <- 1 + row_dag$A + Y_main
    row_dag$YA0 <- 1 + 0 + Y_main
    row_dag$YA1 <- 1 + 1 + Y_main
    simData_t <- rbind(simData_t, row_dag)
 }


  ## Perform the min-max scaling at the end so we have all values,
  ## and thus can easily rescale the values
  minx <- min(c(simData_t$Y, simData_t$YA0, simData_t$YA1))
  maxx <- max(c(simData_t$Y, simData_t$YA0, simData_t$YA1))

  simData_t$Y <- min_max_scale(simData_t$Y, 100, minx, maxx)
  simData_t$YA0 <- min_max_scale(simData_t$YA0, 100, minx, maxx)
  simData_t$YA1 <- min_max_scale(simData_t$YA1, 100, minx, maxx)
  return (simData_t)
}

calc_blocks<-function(n,to_block_val,prob_w2_val, simData_t0){
  simData_t_df <- data.frame()
  for (ptn_id_value in 1:n) {
    temp <- generateLagData(simData_t0 = simData_t0,
                                    ptn_id = ptn_id_value,
                                    to_block = to_block_val,
                                    prob_w2 =prob_w2_val,
                                    n = 1)
    simData_t_df <- rbind(simData_t_df, temp)
  }

  return(simData_t_df)
}

# call functions
## Calculate block zero
## N is number of participants
n <- 1
## probability_w2 is the probability of good sleep
probability_w2 <-0.65
## The number of blocks we'd like
nblocks <- 180

# Calculate the first block
simData_t0 <- generateData0(n, probability_w2)

# Calculate the lagged blocks blocks
simData_t_df <- calc_blocks(n, nblocks, probability_w2, simData_t0)

# Combine block zero and the following blocks
data.train <- rbind(simData_t0,simData_t_df)

# Write CSV
#write.csv(simData_t_df, file = "simData.csv",row.names=FALSE)

True_Psi <- mean(simData_t_df$YA1 - simData_t_df$YA0)
cat(" True_Psi:", True_Psi, "\n")

## We'd like to use the following features in our estimation:
# TODO: When adding multiple people, include w1
# REMOVING W1 for now!
w2 <- RelevantVariable$new(formula = w2 ~ Y_lag_1 + A_lag_1 + w2_lag_1 + w3_lag_1,  family = 'gaussian')
w3 <- RelevantVariable$new(formula = w3 ~ w2 + Y_lag_1 + A_lag_1 +  w2_lag_1 + w3_lag_1,  family = 'gaussian')
A <- RelevantVariable$new(formula = A ~ w3 + w2 + Y_lag_1 + A_lag_1 +  w2_lag_1 + w3_lag_1, family = 'binomial')
Y <- RelevantVariable$new(formula = Y ~ A + w3 + w2 + Y_lag_1 + A_lag_1 +  w2_lag_1 + w3_lag_1, family = 'gaussian')

relevantVariables <- c(w2, w3, A, Y)

log <- R.utils::Arguments$getVerbose(-8, timestamp=TRUE)

## What is the maximum number of iterations the OSL can use while going over the data?
## Note that in this case we split the data in equal parts with this number of iterations
max_iterations <- 10

## Define a list of algorithms to use
algos <- list()
#algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                 #params = list(nbins = c(5, 10, 15), online = TRUE))))

#algos <- append(algos, list(list(algorithm = "ML.NeuralNet",
                                 #params = list(nbins = c(5), online = TRUE))))

algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                 params = list(nbins = c(15), online = TRUE))))
algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                 params = list(nbins = c(5), online = TRUE))))
algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                 algorithm_params = list(alpha = seq(0,1,0.2)),
                                 params = list(nbins = c(5), online = TRUE))))

## Specify the intervention we'd like to test, and also specify when we want to
## test this intervension
intervention <- list(variable = 'A', when = c(1), what = c(1))
tau <- 2
training_set_size <- nrow(data.train) / 2

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
  mini_batch_size = 30 ## Split the remaining data into N-Nl/max_iterations equal blocks of data
)

OutputPlotGenerator.create_training_curve(osl$get_historical_cv_risk, 
                                          relevantVariables = relevantVariables,
                                          output = 'curve')

if (FALSE) {
  
## Specify the intervention we'd like to test, and also specify when we want to
## test this intervention
intervention <- list(variable = 'A', when = c(1), what = c(1))
## Tau is the time at which we want to test the intervention
tau <- 1
## B is the number of iterations we'll run before we hope to converge
B <- 1000

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
discrete <- TRUE

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
  )
})

result.dosl = intervention_effects[[1]]
result.osl = intervention_effects[[2]]

data <- list(truth = rep(33, length(dosl), dosl = result.dosl, osl = result.osl)
OutputPlotGenerator.create_convergence_plot(data = data, output = 'convergence')


## actually do a good job estimating the true conditional distributions.
## Finally we run our kolmogorov smirnov test example to check whether we
## Define kolmogorov-smirnov test
#T_iter <- 10
#B_iter <- 100
#nbins <- 5
#n_A_bins <- 2

### Define the object that will be used to run the evalutation, and run the actual evaluations.
#subject <- ConditionalDensityEvaluator$new(log, osl = osl, summary_measure_generator = osl$get_summary_measure_generator)
#result <- subject$evaluate(
  #sim,
  #T_iter, 
  #B_iter,
  #nbins = nbins
#)

## Output the evaluation.
flat_result <- result %>% unlist %>% unname
flat_result <- flat_result[!is.na(flat_result)]
perc_significant <- sum(flat_result >= 0.95) / length(flat_result) * 100 %>% round(., 2)
perc_significant <- perc_significant %>% round(., 2)
paste(perc_significant,'% significant in the KS-test')

cat('The effects of the interventions were:')
cat(paste('approx',':', result.approx)) 
for(result in intervention_effects) { cat(result) }

}
