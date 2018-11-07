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


library("dplyr")

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
    Y_main <- (-0.1 * row_dag$w1 + 0.4 * row_dag$w2 + 0.3 * row_dag$w3) + noise
    row_dag$Y   <- 1 + row_dag$A + Y_main+length_A_1/10+prev_Y/100
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

calc_blocks<-function(n,to_block_val,prob_w2_val){
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
set.seed(1234)
library(doParallel)
registerDoParallel(cores = parallel::detectCores())

## Calculate block zero
## N is number of participants
n <- 10
## probability_w2 is the probability of good sleep
probability_w2 <-0.65
## The number of blocks we'd like
nblocks <- 180

# Calculate the first block
simData_t0 <- generateData0(n, probability_w2)

# Calculate the lagged blocks blocks
simData_t_df <- calc_blocks(n, nblocks, probability_w2)

# Combine block zero and the following blocks
simData_t_df <- rbind(simData_t0,simData_t_df)

# Write CSV
#write.csv(simData_t_df, file = "simData.csv",row.names=FALSE)

True_Psi <- mean(simData_t_df$YA1 - simData_t_df$YA0)
cat(" True_Psi:", True_Psi, "\n")

