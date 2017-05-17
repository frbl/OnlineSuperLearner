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
  tmlenet_options(parfit=FALSE)
  options(warn=-1)
  set.seed(12345)

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
  algos <- list()

  # Number of iterations for approximation of the true parameter of interest
  B <- 1e2

  # The intervention we are interested in
  intervention  <- list(when = c(2), what = c(0), variable ='A')

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
                          #params = list(nbins = c(6,40), online = TRUE))))

  #algos <- append(algos, list(list(algorithm = 'ML.H2O.gbm',
                          #algorithm_params = list(ntrees=c(10,20), min_rows=1),
                          #params = list(nbins = c(6), online = TRUE))))

  #algos <- append(algos, list(list(algorithm = 'ML.H2O.randomForest',
                          #algorithm_params = list(ntrees=c(10,20)),
                          #params = list(nbins = c(6), online = TRUE))))

  algos <- append(algos, list(list(algorithm = 'tmlenet::speedglmR6',
                          #algorithm_params = list(),
                          params = list(nbins = c(3,4, 5), online = FALSE))))

  #algos <- append(algos, list(list(algorithm = 'tmlenet::glmR6',
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
    mu <- aa*(0.4-0.2*sin(ww)+0.05*ww) +
      (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
    rnorm(length(mu), mu, sd=0.1)}})


  ##################
  # Approximation  #
  ##################
  # 'psi.approx' is a Monte-Carlo approximation of the parameter of interest.
  # This is a little slow, because 'simulateWAY' is designed to simulate quickly a long time series,
  # as opposed to many short time series.
  if (FALSE) {
    # Calculated based on 1e4
    psi.approx <- 0.2991661 #with intervention = 0
    psi.approx <- 0.9021478 #with intervention = 1
  } else {
    psi.approx <- mclapply(seq(B), function(bb) {
      when <- max(intervention$when)
      data.int <- simulator$simulateWAY(tau, qw = llW, ga = llA, Qy = llY,
                                  intervention = intervention, verbose = FALSE)
      data.int$Y[tau]
    }, mc.cores = cores) %>%
      unlist %>%
      mean
  }

  print(psi.approx)

  ##############
  # Estimation #
  ##############
  data.train <- simulator$simulateWAY(training_set_size + B, qw=llW, ga=llA, Qy=llY, verbose=log) %>%
    Data.Static$new(dataset = .)

  # We use the following covariates in our estimators
  W <- RandomVariable$new(formula = Y ~ A + W, family = 'gaussian')
  A <- RandomVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
  Y <- RandomVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
  randomVariables <- c(W, A, Y)

  # Generate some bounds to use for the data (scale it between 0 and 1)
  bounds <- list()
  for(name in colnames(data)) {
    min_bound = min(data[, name, with=FALSE] )
    max_bound = max(data[, name, with=FALSE] )
    bounds <- append(bounds, list(list(max_bound = max_bound, min_bound = min_bound)))
  }
  names(bounds) <- colnames(data)

  smg_factory <- SMGFactory$new()
  summaryMeasureGenerator <- smg_factory$fabricate(randomVariables, bounds = bounds)

  osl <- OnlineSuperLearner$new(algos, summaryMeasureGenerator = summaryMeasureGenerator, verbose = log)
  risk <- osl$fit(data.train, randomVariables = randomVariables,
                        initial_data_size = training_set_size / 2,
                        max_iterations = max_iterations,
                        mini_batch_size = (training_set_size / 2) / max_iterations)

  summaryMeasureGenerator$reset
  datas <- summaryMeasureGenerator$getNext(n = B)
  result <- mclapply(seq(B), function(i) {
   osl$sample_iteratively(data = datas[i,],
                          randomVariables = randomVariables,
                          intervention = intervention,
                          tau = tau)
  }, mc.cores=cores) %>%
    lapply(., function(x) { tail(x, 1)$Y }) %>%
    unlist

  #plot(cumsum(result)/seq(along=result))

  psi.estimation <- mean(result)

  print(psi.estimation)
  #print(abs(psi.approx - psi.estimation))

  expect_true((abs(psi.approx - psi.estimation)) < 0.1)

})

