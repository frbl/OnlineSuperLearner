library(mockery)

context("ConditionalDensityEvaluator.R")
described.class <- ConditionalDensityEvaluator
glob_osl_mock <- mock('osl')
class(glob_osl_mock) <- 'OnlineSuperLearner'

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
    mu <- aa*(0.9) + (1-aa)*(0.3) + rnorm(length(aa), 0, 1)
    mu <- pmax(pmin(mu, 1),0)
    rbinom(length(aa), 1, mu)
  }}
)

glob_simulator  <- Simulator.GAD$new(qw = glob_llW,
                                     ga = glob_llA,
                                     Qy = glob_llY)


context(" initialize")
##=====================================================================
test_that("it should initialize without problems", {
  #browser()
  expect_error(described.class$new(), NA)
  subject <- described.class$new()
  expect_is(subject, 'ConditionalDensityEvaluator')
})

context(" evaluate")
##=====================================================================
test_that("it should throw if the provided T_iter is not an int", {
  subject <- described.class$new()
  T_iter <- print
  expect_error(subject$evaluate(glob_osl_mock, glob_simulator_mock, T_iter, glob_B_iter),
               "Argument 'T_iter' is not a vector: function", fixed = TRUE)
})

test_that("it should throw if the provided B_iter is not an int", {
  subject <- described.class$new()
  B_iter <- print
  expect_error(subject$evaluate(glob_osl_mock, glob_simulator_mock, glob_T_iter, B_iter),
               "Argument 'B_iter' is not a vector: function", fixed = TRUE)
})

test_that("it should evaluate the learner", {
  ## General settings
  doParallel::registerDoParallel(cores = parallel::detectCores())
  log <- R.utils::Arguments$getVerbose(-1, timestamp=TRUE)
  nb_iter <- 10
  training_set_size <- 1000
  data <- glob_simulator$simulateWAY(numberOfBlocks = training_set_size)
  subject <- described.class$new()

  algos <- list()

  algos <- append(algos, list(list(algorithm = "ML.XGBoost",
                                  algorithm_params = list(alpha = c(0.3, 0.4, 0.5)),
                                  params = list(nbins = c(30, 40, 50), online = TRUE))))

  algos <- append(algos, list(list(algorithm = "condensier::speedglmR6",
                                  params = list(nbins = c(5, 10, 15), online = FALSE))))


  relevant_variables <- list(RelevantVariable$new(formula = Y ~ A + W, family = 'gaussian'))

  ## Fit the online SuperLearner
  osl <- OnlineSuperLearner::fit.OnlineSuperLearner(
    formulae = relevant_variables,
    data = data,
    algorithms = algos, 
    verbose = log,
    test_set_size = 5 + (3 * 3 + 3),

    initial_data_size = training_set_size / 2,
    max_iterations = nb_iter,
    mini_batch_size = (training_set_size / 2) / nb_iter
  )

  result <- subject$evaluate(osl, glob_simulator, 100, 10000)
})

