context('Integration test: Test the whole SuperLearner routine')

test_that("it should estimate the true treatment", {
  #if(Sys.getenv('CI') == "") skip('Only running this test on Circle. It takes very long.')
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

  # Number of cores available
  cores = 1 #detectCores()

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
  algos <- list()

  # Number of iterations for approximation of the true parameter of interest
  B <- 1e2

  # The intervention we are interested in
  interventions  <- list(intervention = list(when = c(2), what = c(1), variable ='A'))

  # The time of the outcome
  tau = 2

  #algos <- list(list(description='ML.H2O.randomForest-1tree',
                          #algorithm = 'ML.H2O.randomForest',
                          #params = list(ntrees = 1)))

  #algos <- append(algos, list(list(description='ML.H2O.randomForest-50trees',
                          #algorithm = 'ML.H2O.randomForest',
                          #params = list(ntrees = 50))))

  #algos <- append(algos, list(list(description='ML.H2O.gbm',
                          #algorithm = 'ML.H2O.gbm')))

  #algos <- append(algos, list(list(algorithm = 'ML.XGBoost',
                          #algorithm_params = list(alpha = 0),
                          #params = list(nbins = c(6,40, 50), online = FALSE))))

  #algos <- append(algos, list(list(algorithm = 'ML.H2O.gbm',
                          #algorithm_params = list(ntrees=c(10,20), min_rows=1),
                          #params = list(nbins = c(6), online = TRUE))))

  #algos <- append(algos, list(list(algorithm = 'ML.H2O.randomForest',
                          #algorithm_params = list(ntrees=c(10,20)),
                          #params = list(nbins = c(6), online = TRUE))))

  algos <- append(algos, list(list(algorithm = 'condensier::speedglmR6',
                          #algorithm_params = list(),
                          params = list(nbins = c(3,40, 50, 100), online = FALSE))))

  #algos <- append(algos, list(list(algorithm = 'condensier::glmR6',
                          ##algorithm_params = list(),
                          #params = list(nbins = c(16, 20, 24, 30, 34, 40), online = FALSE))))

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
    mu <- 500 + aa * 5 + (1-aa) * 0.2
    rnorm(length(mu), mu, sd=0.01)}})


  ##################
  # Approximation  #
  ##################
  # 'psi.approx' is a Monte-Carlo approximation of the parameter of interest.
  # This is a little slow, because 'simulateWAY' is designed to simulate quickly a long time series,
  # as opposed to many short time series.
  psi <- mclapply(seq(B), function(bb) {
    when <- max(interventions$intervention$when)
    data.int <- simulator$simulateWAY(tau, qw = llW, ga = llA, Qy = llY,
                                intervention = interventions$intervention, verbose = FALSE)
    data.int$Y[tau]
  }, mc.cores = cores) %>% unlist
  
  psi.approx <- mean(psi)


  ##############
  # Estimation #
  ##############
  data <- simulator$simulateWAY(training_set_size + B + 1000, qw=llW, ga=llA, Qy=llY, verbose=log)
  data.train <- Data.Static$new(dataset = data)

  # We use the following covariates in our estimators
  W <- RandomVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  A <- RandomVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RandomVariable$new(formula = Y ~ A_lag_1 + Y_lag_1 + A + W, family = 'gaussian')
  randomVariables <- c(W, A, Y)

  # Generate some bounds to use for the data (scale it between 0 and 1)
  bounds <- PreProcessor.generate_bounds(data)
  pre_processor <- PreProcessor$new(bounds)

  smg_factory <- SMGFactory$new()
  summaryMeasureGenerator <- smg_factory$fabricate(randomVariables, pre_processor = pre_processor)

  osl <- OnlineSuperLearner$new(algos, summaryMeasureGenerator = summaryMeasureGenerator, verbose = log, 
                                pre_processor = pre_processor)


  hide_warning_replace_weights_osl(
    risk <- osl$fit(data.train, randomVariables = randomVariables,
                          initial_data_size = training_set_size / 2,
                          max_iterations = max_iterations,
                          mini_batch_size = (training_set_size / 2) / max_iterations)
  )


  summaryMeasureGenerator$reset()
  datas <- summaryMeasureGenerator$getNext(n = 1000)

  intervention_effect_caluculator = InterventionEffectCalculator$new(bootstrap_iterations = B, 
                                                                    randomVariables = randomVariables, 
                                                                    outcome_variable = Y$getY,
                                                                    verbose = FALSE,
                                                                    parallel = FALSE)

  result.all <- intervention_effect_caluculator$calculate_intervention_effect(
    osl = osl,
    interventions = interventions, 
    discrete = TRUE, 
    initial_data = datas[1,],
    tau = tau
  )$intervention
  result <- result.all %>% unlist %>% mean

  data <- list(truth = psi, dosl = result.all)

  OutputPlotGenerator.create_convergence_plot(
    data = data,
    output = paste('test_convergence_configuration',sep='_')
  )

  #datas$A <- 1
  #osl$predict(data = copy(datas), randomVariables, plot= TRUE, sample=TRUE)$denormalized$dosl.estimator$Y%>% mean
  #osl$predict(data = copy(datas), randomVariables, plot= TRUE, sample=FALSE)
  psi.estimation <- mean(result)
  print(paste('Approximation:', psi.approx, 'estimation:', psi.estimation, 'difference:', abs(psi.approx - psi.estimation)))
  expect_lt(abs(psi.approx - psi.estimation), 0.1)

})

