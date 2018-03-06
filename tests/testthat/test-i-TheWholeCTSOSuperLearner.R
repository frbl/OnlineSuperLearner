context('Integration test: Test the whole SuperLearner routine with the continous super learner')
test_that("it should estimate the true treatment", {
  if(Sys.getenv('CI') == "travis") skip('Not running this spec on Travis!')
  # This very basic example shows how well the intervention estimation works. The procedure is as follows. We
  # have 3 variables, W A and Y, of which W is cts, A is binary and Y is gaussian. We will generate a number of
  # samples from this distribution with which the estimators are trained. Then we will simulate an interverntion
  # on the true distribution, and check the outcome under the intervention. This is compared with the outcome as
  # estimated using OSL.

  ## INITIALIZATION
  # Generate the mehanisms
  # we generate number of blocks observations
  options(warn=-1)
  set.seed(12345)
  doParallel::registerDoParallel(cores = parallel::detectCores())

  # Number of cores available
  cores = detectCores()

  # Create the simulator
  simulator  <- Simulator.GAD$new()

  # Number of items we have in our testset
  training_set_size <- 200

  # Number of iterations we want to use
  max_iterations = 3

  # The calculator for estimating the risk
  cv_risk_calculator <- CrossValidationRiskCalculator$new()

  # Do logging?
  log <- FALSE
  #log <- Arguments$getVerbose(-1, timestamp=TRUE)

  algos <- list()

  # Number of iterations for approximation of the true parameter of interest
  B <- 1e2

  # The intervention we are interested in
  interventions  <- list(intervention = list(when = c(2), what = c(1), variable ='A'))

  # The time of the outcome
  tau = 2

  #algos <- append(algos, list(list(algorithm = 'ML.XGBoost',
                          ##algorithm_params = list(alpha = 0),
                          #params = list(nbins = c(3,4,5), online = FALSE))))
  algos <- append(algos, list(list(algorithm = 'condensier::speedglmR6',
                          #algorithm_params = list(),
                          params = list(nbins = c(3,4,5), online = FALSE))))


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
    mu <- 100 + aa*(0.4-0.2*sin(ww)+0.05*ww) +
      (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
    rnorm(length(mu), mu, sd=0.1)}})


  ##################
  # Approximation  #
  ##################
  # 'psi.approx' is a Monte-Carlo approximation of the parameter of interest.
  # This is a little slow, because 'simulateWAY' is designed to simulate quickly a long time series,
  # as opposed to many short time series.
  psi.approx <- mclapply(seq(B), function(bb) {
    when <- max(interventions$intervention$when)
    data.int <- simulator$simulateWAY(tau, qw = llW, ga = llA, Qy = llY,
                                intervention = interventions$intervention, verbose = FALSE)
    data.int$Y[tau]
  }, mc.cores = cores) %>%
    unlist %>%
    mean


  ##############
  # Estimation #
  ##############
  data <- simulator$simulateWAY(training_set_size + B, qw=llW, ga=llA, Qy=llY, verbose=log)
  data.train <- Data.Static$new(dataset = data)

  # We use the following covariates in our estimators
  W <- RelevantVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  A <- RelevantVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RelevantVariable$new(formula = Y ~ A + W, family = 'gaussian')
  relevantVariables <- c(W, A, Y)

  # Generate some bounds to use for the data (scale it between 0 and 1)
  bounds <- PreProcessor.generate_bounds(data)
  pre_processor <- PreProcessor$new(bounds)

  smg_factory <- SMGFactory$new()
  summaryMeasureGenerator <- smg_factory$fabricate(relevantVariables, pre_processor = pre_processor)

  osl <- OnlineSuperLearner$new(algos, summaryMeasureGenerator = summaryMeasureGenerator,
                                relevant_variables = relevantVariables,
                                verbose = log, 
                                should_fit_osl = TRUE,
                                should_fit_dosl = FALSE,
                                pre_processor = pre_processor)
  hide_warning_replace_weights_osl(
    risk <- osl$fit(data.train, initial_data_size = training_set_size / 2,
                          max_iterations = max_iterations,
                          mini_batch_size = (training_set_size / 2) / max_iterations)
  )

  summaryMeasureGenerator$reset()
  datas <- summaryMeasureGenerator$getNext(n = 1)[[1]]

  #result <- mclapply(seq(B), function(i) {
   #osl$sample_iteratively(data = datas[i,],
                          #intervention = intervention,
                          #variable_of_interest = Y,
                          #tau = tau)
  #}, mc.cores=cores)
  intervention_effect_caluculator = InterventionEffectCalculator$new(bootstrap_iterations = B, 
                                                                    outcome_variable = Y$getY,
                                                                    verbose = FALSE,
                                                                    parallel = FALSE)


  result <- intervention_effect_caluculator$perform_initial_estimation(
    osl = osl,
    interventions = interventions, 
    discrete = FALSE, 
    initial_data = datas[1,],
    tau = tau
  )$intervention

  psi.estimation <- mean(result)

  print(paste('Approximation:', psi.approx, 'estimation:', psi.estimation, 'difference:', abs(psi.approx - psi.estimation)))
  expect_lt((abs(psi.approx - psi.estimation)),  0.2)
})

