library(mockery)
run_slow_specs <- FALSE

context("ConditionalDensityEvaluator.R")
described.class <- ConditionalDensityEvaluator
glob_osl_mock <- mock('osl')
class(glob_osl_mock) <- 'OnlineSuperLearner'

glob_smg_mock <- mock('summary_measure_generator')
class(glob_smg_mock) <- 'SummaryMeasureGenerator'

glob_simulator_mock <- mock('simulator')
class(glob_simulator_mock) <- 'Simulator.GAD'
glob_T_iter <- 10
glob_B_iter <- 10


glob_llW <- list(stochMech=function(numberOfBlocks) {
    rnorm(numberOfBlocks, 0, 1)
  },
  param=c(1),
  rgen=identity
)

glob_llA <- list(
  stochMech=function(ww) {
    rbinom(length(ww), 1, expit(ww))
  },
  param=c(0.5),
  rgen=function(xx, delta=0.05){
    probability <- delta+(1-2*delta)*expit(xx)
    rbinom(length(xx), 1, probability)
  }
)

glob_llY <- list(rgen={function(AW){
    aa <- AW[, "A"]
    ww <- AW[, grep("[^A]", colnames(AW))]
    mu <- aa*(0.9) + (1-aa)*(0.3) + 0.6 * ww + rnorm(length(aa), 0, 0.1)
    mu
  }}
)

glob_simulator  <- Simulator.GAD$new(qw = glob_llW,
                                     ga = glob_llA,
                                     Qy = glob_llY)


context(" initialize")
##=====================================================================
test_that("it should initialize without problems", {
  expect_error(described.class$new(osl = glob_osl_mock, 
                                 summary_measure_generator = glob_smg_mock), NA)
  subject <- described.class$new(osl = glob_osl_mock, 
                                 summary_measure_generator = glob_smg_mock)
  expect_is(subject, 'ConditionalDensityEvaluator')
})

context(" evaluate")
##=====================================================================
test_that("it should throw if the provided T_iter is not an int", {
  subject <- described.class$new(osl = glob_osl_mock, 
                                 summary_measure_generator = glob_smg_mock)
  T_iter <- print
  expect_error(subject$evaluate(glob_osl_mock, glob_simulator_mock, T_iter, glob_B_iter),
               "Argument 'T_iter' is not a vector: function", fixed = TRUE)
})

test_that("it should throw if the provided B_iter is not an int", {
  subject <- described.class$new(osl = glob_osl_mock, 
                                 summary_measure_generator = glob_smg_mock)
  B_iter <- print
  expect_error(subject$evaluate(glob_simulator_mock, glob_T_iter, B_iter),
               "Argument 'B_iter' is not a vector: function", fixed = TRUE)
})

#test_that("it should implement a test from which we know the correct distribution, and we know it is the same as the simulator", {
  ##if(Sys.getenv('CI') != "" || !run_slow_specs) skip('Takes to long now')
  #set.seed(1234)

  ### Create a simulator
  #nobs <- 10
  #p_0 <<- function(nobs) {
    #W <- rnorm(nobs, 0, 1)
    #A <- rbinom(nobs, 1, 0.8)
    #Y <- rnorm(nobs, 1, 1)
    #data.table(W =W, A= A, Y=Y)
  #}

  #mock_simulator <- list(simulateWAY = function(numberOfTrajectories) {
    #p_0(nobs = numberOfTrajectories)
  #})

  #sampledata.MockOsl <<- function(object, newdata, Y = NULL, nobs = 1, ...) {
    #res <- p_0(nobs = nobs)
    #list(osl.estimator = res$Y,
         #dosl.estimator = res$Y)
  #}
  #mock_osl <- mock('osl')
  #class(mock_osl) <- 'OnlineSuperLearner'

  #mock_smg <- mock('summary_measure_generator')
  #class(mock_smg) <- 'SummaryMeasureGenerator'

  #log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)
  #subject <- described.class$new(log, osl = mock_osl,
                                 #summary_measure_generator = mock_smg)
  #doParallel::registerDoParallel(cores = parallel::detectCores())

  #T_iter <- 10
  #B_iter <- 10000
  #nbins <- 5
  #n_A_bins <- 2

  #result <- subject$evaluate(
    #mock_simulator,
    #T_iter, 
    #B_iter,
    #nbins= nbins
  #)

  #result %<>% unlist %>% unname
  #total_significant <- (result > 0.05) %>% as.numeric %>% sum
  #percentage_significant <- total_significant / (T_iter * nbins * n_A_bins)
  #expect_gte(percentage_significant, 0.95)
#})

test_that("it should show when the two provided distributions are not the same", {
  set.seed(1234)
  log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)
  doParallel::registerDoParallel(cores = parallel::detectCores())

  ## Create a simulator
  nobs <- 10
  p_0 <<- function(nobs, newdata = NULL) {
    if (is.null(newdata)) {
      W <- rnorm(nobs, 0, 1)
      A <- rbinom(nobs, 1, 0.8)
    } else {
      W <- newdata$W
      A <- newdata$A
    }
    Y <- rnorm(nobs, A, 0.1)
    data.table(W = W, A = A, Y=Y)
  }

  p_1 <<- function(nobs, newdata = NULL) {
    W <- c(rnorm(nobs/2, -0.5, 1), rnorm(nobs/2, 0.5, 1))
    A <- c(rbinom(nobs, 1, 0.5))
    Y <- c(rnorm(nobs/2, A + 0.3, 0.01), rnorm(nobs/2, A-0.3, 0.01))
    data.table(W = W, A = A, Y=Y)
  }

  mock_simulator <- list(simulateWAY = function(numberOfTrajectories) {
    p_0(nobs = numberOfTrajectories)
  })

  sampledata.MockOsl <<- function(object, newdata, Y = NULL, nobs = 1, ...) {
    res <- p_1(nobs = nobs, newdata)
    list(osl.estimator = res,
         dosl.estimator = res)
  }

  mock_osl <- mock('osl')
  class(mock_osl) <- 'MockOsl'

  mock_smg <- list(
    set_trajectories = function(data) { },
    get_minimal_measurements_needed = 1234,
    getNext = function(n) {
      list(p_0(n))
    } 
  )

  class(mock_smg) <- 'SummaryMeasureGenerator'

  T_iter <- 10
  B_iter <- 10000
  nbins <- 5
  n_A_bins <- 2

  subject <- described.class$new(log, osl = mock_osl, summary_measure_generator = mock_smg)
  result <- subject$evaluate(
    mock_simulator,
    T_iter, 
    B_iter,
    nbins = nbins
  )

  result %<>% unlist %>% unname
  total_insignificant <- (result <= 0.05) %>% as.numeric %>% sum
  percentage_insignificant <- total_insignificant / (T_iter * nbins * n_A_bins)
  expect_gte(percentage_insignificant, 0.95)
})

#test_that("it should implement a test from which we know the correct conditional distribution, and we know it is the same as the simulator", {
  #set.seed(1234)
  #log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)
  #subject <- described.class$new(log)
  #doParallel::registerDoParallel(cores = parallel::detectCores())

  ### Create a simulator
  #nobs <- 10
  #p_0 <<- function(nobs, newdata = NULL) {
    #if (is.null(newdata)) {
      #W <- rnorm(nobs, 0, 1)
      #A <- rbinom(nobs, 1, 0.8)
    #} else {
      #W <- newdata$W
      #A <- newdata$A
    #}
    #Y <- rnorm(nobs, A, 0.1)
    #data.table(W = W, A = A, Y=Y)
  #}

  #mock_simulator <- list(simulateWAY = function(numberOfTrajectories) {
    #p_0(nobs = numberOfTrajectories)
  #})

  #sampledata.MockOsl <<- function(object, newdata, Y = NULL, nobs = 1, ...) {
    #res <- p_0(nobs = nobs, newdata)
    #list(osl.estimator = res$Y,
         #dosl.estimator = res$Y)
  #}
  #mock_osl <- mock('osl')
  #class(mock_osl) <- 'MockOsl'

  #T_iter <- 10
  #B_iter <- 10000
  #nbins <- 5
  #n_A_bins <- 2

  #result <- subject$evaluate(
    #mock_osl,
    #mock_simulator,
    #T_iter, 
    #B_iter,
    #nbins= nbins
  #)

  #result %<>% unlist %>% unname
  #total_significant <- (result > 0.05) %>% as.numeric %>% sum
  #percentage_significant <- total_significant / (T_iter * nbins * n_A_bins)
  #expect_gte(percentage_significant, 0.95)
#})

#test_that("it should evaluate the learner", {
  #if(Sys.getenv('CI') != "" || !run_slow_specs) skip('Takes to long now')
  ### General settings
  #doParallel::registerDoParallel(cores = parallel::detectCores())
  #log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)
  #nb_iter <- 10
  #training_set_size <- 1000
  #data <- glob_simulator$simulateWAY(numberOfTrajectories = training_set_size)
  #subject <- described.class$new(log)

  #algos <- list()

  ##algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                  ##algorithm_params = list(alpha = c(0.3, 0.4, 0.5)),
                                  ##params = list(nbins = c(30, 40, 50), online = TRUE))))

  #algos <- append(algos, list(list(algorithm = "condensier::speedglmR6",
                                  #params = list(nbins = c(5, 10, 15, 20), online = FALSE))))


  #relevant_variables <- list(RelevantVariable$new(formula = Y ~ A + W, family = 'gaussian'))

  ### Fit the online SuperLearner
  #osl <- OnlineSuperLearner::fit.OnlineSuperLearner(
    #formulae = relevant_variables,
    #data = data,
    #algorithms = algos, 
    #bounds = TRUE,
    #verbose = FALSE,
    #test_set_size = 5 + (3 * 3 + 3),

    #initial_data_size = training_set_size / 2,
    #max_iterations = nb_iter,
    #mini_batch_size = (training_set_size / 2) / nb_iter
  #)

  #T_iter <- 10
  #B_iter <- 1000
  #nbins <- 5

  #result <- subject$evaluate(osl, glob_simulator, T_iter, B_iter, nbins= nbins)
  #print(result)
  #expect_is(result, 'list')
  #expect_length(result, T_iter)
  #expect_length(result[[1]], nbins)

  ## Check the middle result, it should have 2 outcomes, one for A=0, one for A=1
  #expect_length(result[[1]][[nbins/2]], 2)
#})

