## ----install devtools, echo=FALSE, results='hide'------------------------
if (!("devtools" %in% rownames(installed.packages()))) { install.packages('devtools') }

## ----setup the package, echo=FALSE, results='hide'-----------------------
use_local <- FALSE

if ("package:OnlineSuperLearner" %in% search()) { detach("package:OnlineSuperLearner", unload=TRUE) }
if ("OnlineSuperLearner" %in% rownames(installed.packages())) { remove.packages("OnlineSuperLearner") }
if (use_local) {
  install.packages('.', repos = NULL, type="source")
} else {
  devtools::install_github('frbl/onlinesuperlearner')
}
library('OnlineSuperLearner')

## ----inspect the setup---------------------------------------------------
getNamespaceExports("OnlineSuperLearner")

## ----initialization------------------------------------------------------
  library('magrittr')
  # Set the seed
  set.seed(12345)

  # Set some functions, for readability
  expit = plogis
  logit = qlogis

  # Do logging?
  log <- FALSE

  # How many cores would we like to use?
  cores = parallel::detectCores()

  # Number of items we have in our training set
  training_set_size <- 200

  # Number of items we have in our testset
  test_set_size <- 2

  # Number of iterations we want to use (this is for the online training part)
  max_iterations <- 3

  # Size of the mini batch
  mini_batch_size <- 3

  # The calculator for estimating the risk. This is just used for testing.
  cv_risk_calculator <- CrossValidationRiskCalculator$new()

## ----intervention initialization-----------------------------------------
  # Number of iterations for approximation of the true parameter of interest
  B <- 1e2

  # The intervention we are interested in (intervene at time 2, give intervention 1, on variable A)
  intervention  <- list(when = c(2), what = c(1), variable = 'A')

  # The time of the outcome we are interested in
  tau <- 2

## ----initialize simulation study-----------------------------------------
  complicated_treatment <- FALSE

  # Our covariate definition
  llW <- list(
    stochMech=function(numberOfBlocks) {
      rnorm(numberOfBlocks, 0, 10)
    },
    param=c(0, 0.5, -0.25, 0.1),
    rgen=identity
  )

  # The treatment mechanism
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

  # The outcome variable
  if (complicated_treatment) {
      rgenfunction = function(AW){
        aa <- AW[, "A"]
        ww <- AW[, grep("[^A]", colnames(AW))]
        mu <- aa*(0.4-0.2*sin(ww)+0.05*ww) + (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
        rnorm(length(mu), mu, sd=0.1)
      } 
  } else {
        rgenfunction = function(AW){
        aa <- AW[, "A"]
        mu <- 19 + aa*(0.9) + (1-aa)*(0.3)
        rnorm(length(mu), mu, sd=0.1)
      }
  }
  llY <- list(rgen = rgenfunction)

## ----create the simulator and approximate the parameter of interest------
  # Create the simulator
  simulator  <- Simulator.GAD$new()

  # Approximate the truth under the treatment
  result.approx <- parallel::mclapply(seq(B), function(bb) {
    when <- max(intervention$when)
    data.int <- simulator$simulateWAY(
      tau,
      qw = llW,
      ga = llA,
      Qy = llY,
      intervention = intervention,
      verbose = log
    )
    data.int$Y[tau]
  }, mc.cores = cores) %>%
    unlist

  # Calculate the approximation of the true parameter of interest
  psi.approx <- mean(result.approx)

  print(psi.approx)

## ----simulate a data set-------------------------------------------------
  data.train <- simulator$simulateWAY(training_set_size + B + 100, qw=llW, ga=llA, Qy=llY, verbose=log)
  data.test <- simulator$simulateWAY(1000, qw=llW, ga=llA, Qy=llY, verbose=log)

## ----initialize the list of algorithms to use----------------------------
  algos <- list()

  algos <- append(algos, list(list(algorithm = "ML.NeuralNet",
                                  params = list(nbins = c(5), online = TRUE))))

  algos <- append(algos, list(list(algorithm = "ML.SpeedGLMSGD",
                                  params = list(nbins = c(5), online = TRUE))))

## ----specify the relevant variables--------------------------------------
  W <- RelevantVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  A <- RelevantVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RelevantVariable$new(formula = Y ~ A + W, family = 'gaussian')

  variable_of_interest <- Y
  relevantVariables <- c(W, A, Y)

## ----fit the online superlearner-----------------------------------------
osl  <- fit.OnlineSuperLearner(
  formulae = relevantVariables,
  data = data.train,
  algorithms = algos, 
  verbose = log,

  test_set_size = test_set_size,
  initial_data_size = training_set_size / 2,
  max_iterations = max_iterations,
  mini_batch_size = (training_set_size / 2) / max_iterations
)

## ----calculate the effect of the intervention using OLS------------------
  # Should we run in parallel?
  do_parallel <- FALSE

  # Would we like to have the results of the discrete (TRUE) or continuous (FALSE) osl?
  discrete = TRUE

  interventionEffectCalculator <- InterventionEffectCalculator$new(
    bootstrap_iterations = B,
    outcome_variable = 'Y',
    parallel = do_parallel
  )

  ## Generate a block from the initial data
  osl$get_summary_measure_generator$set_trajectories(data = Data.Static$new(dataset = data.test))
  data_test_full <- osl$get_summary_measure_generator$getNext(1)$traj_1

  result <- interventionEffectCalculator$evaluate_single_intervention(
    osl = osl,
    initial_data = data_test_full,
    intervention = intervention,
    tau = tau,
    discrete = discrete
  )

  result %<>% unlist

## ----inspect the differences of the approximation and the estimation-----
  # Calculate psi
  psi.estimation <- mean(result)

  # Plot the convergence
  y1 <- cumsum(result.approx)/seq(along=result.approx)
  y2 <- cumsum(result)/seq(along=result)

  plot(y1, ylim=range(c(y1,y2)))
  par(new=TRUE)
  plot(y2, ylim=range(c(y1,y2)), col="red", axes = FALSE, xlab = "", ylab = "")

  # Print the outcome
  outcome <- paste(
    'We have approximated psi as', psi.approx, 
    'our estimate is', psi.estimation, 
    'which is a difference of:', abs(psi.approx - psi.estimation)
  )
  print(outcome)

## ----predicting and sampling using the osl-------------------------------
  # Predict some variables for our Y outcome variable. The newdata can be a data.table:
  predict(osl, data.test, Y = Y)

  # Apart from predicting, we can also sample new values
  sampledata(osl, data.test, Y = Y)

