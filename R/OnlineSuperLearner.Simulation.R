#' OnlineSuperLearner.Simulation runs different simulations on the superlearner object.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include RandomVariable.R
#' @include SMG.Latest.Entry.R
#' @include SMG.Lag.R
#' @include SMG.Transformation.R
#' @export
OnlineSuperLearner.Simulation <- R6Class("OnlineSuperLearner.Simulation",
  private =
    list(
        sim = NULL,
        training_set_size = NULL,
        test_set_size = NULL,
        log = NULL,
        SL.library.definition = NULL
        ),
  public =
    list(
        initialize = function() {
          options(warn=1)
          options(error = browser)
          private$sim  <- Simulator.GAD$new()
          private$training_set_size <- 1e5
          private$test_set_size <- 100
          private$log <- Arguments$getVerbose(-8, timestamp=TRUE)
          #algos <- list(list(description='ML.H2O.randomForest-1tree',
                                  #algorithm = 'ML.H2O.randomForest',
                                  #params = list(ntrees = 1)))

          #algos <- append(algos, list(list(description='ML.H2O.randomForest-50trees',
                                  #algorithm = 'ML.H2O.randomForest',
                                  #params = list(ntrees = 50))))

          #algos <- append(algos, list(list(description='ML.H2O.gbm',
                                  #algorithm = 'ML.H2O.gbm')))
          algos <- list()

          algos <- append(algos, list(list(algorithm = 'tmlenet::speedglmR6',
                                  #algorithm_params = list(),
                                  params = list(nbins = c(6,40)))))

          algos <- append(algos, list(list(algorithm = 'ML.XGBoost',
                                  algorithm_params = list(alpha = 0),
                                  params = list(nbins = c(6,40)))))


          #algos <- append(algos, list(list(algorithm = 'tmlenet::glmR6',
                                  ##algorithm_params = list(),
                                  #params = list(nbins = c(16)))))

          private$SL.library.definition <- algos

          # Run the simulations
          self$basicRegression()
          self$basicRegressionWithLags()
          self$basicClassification()
          self$basicClassificationWithLags()
          #self$verySimpleSimulation()
        },

        basicClassification = function() {
          set.seed(12345)
        },

        basicClassificationWithLags = function() {
          set.seed(12345)
        },

        basicRegression = function() {
          set.seed(12345)
        },

        basicRegressionWithLagsAndJustWAY = function() {
          set.seed(12345)

          ######################################
          # Generate observations for training #
          #####################################
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
            mu <- aa*(0.9) + (1-aa)*(0.3)
            rnorm(length(mu), mu, sd=0.1)}})
          

          # Create the measures we'd like to include in our model
          # In this simulation we will include 2 lags and the latest data (non lagged)
          # Define the variables in the initial dataset we'd like to use
          Y = "Y"
          W = "W"
          A = "A"
          SMG.list <- list()
          SMG.list <- c(SMG.list, SMG.Lag$new(lags = 2, colnames.to.lag = (c(A, W, Y))))
          SMG.list <- c(SMG.list, SMG.Latest.Entry$new(colnames.to.use = (c(A, W, Y))))
          summaryMeasureGenerator = SummaryMeasureGenerator$new(SMG.list = SMG.list, verbose = private$log) 


          # We'd like to use the following features in our estimation:
          Y.eq <- Y ~ A + W
          A.eq <- A ~ W + Y_lag_1 + A_lag_1 + W_lag_1
          W.eq <- W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2
          W <- RandomVariable$new(formula = W.eq, family = 'gaussian')
          A <- RandomVariable$new(formula = A.eq, family = 'binomial')
          Y <- RandomVariable$new(formula = Y.eq, family = 'gaussian')

          # Generate a dataset we will use for testing.
          private$sim$simulateWAY(200, qw=llW, ga=llA, Qy=llY, verbose=private$log) %>%
            Data.Static$new(dataset = .) %>%
            summaryMeasureGenerator$setData(.)

          data.test <- summaryMeasureGenerator$getNextN(80)

          # Generate a dataset, from the same statistical model, which we will use to train our model
          data.train <-
            private$sim$simulateWAY(private$training_set_size, qw=llW, ga=llA, Qy=llY, verbose=private$log) %>%
            Data.Static$new(dataset = .)

          # Now run several iterations on the data
          #performances <- mclapply(seq(5,201,20), function(i) {

          i = 20
          data.train$reset()


          private$log && cat(private$log, 'Initializing OSL')
          osl <- OnlineSuperLearner$new(private$SL.library.definition,
                                        summaryMeasureGenerator = summaryMeasureGenerator,
                                        verbose = private$log)

          private$log && cat(private$log, 'Running OSL')
          estimators <- osl$fit(data.train, Y = Y, A = A, W = W,
                                initial_data_size = 20000, max_iterations = i,
                                mini_batch_size = 10)

          predictions <- osl$predict(data = copy(data.test), c(W,A,Y), discrete=TRUE) 

          performance <- osl$evaluateModels(data = copy(data.test), randomVariables = c(W, A, Y)) %>%
            c(iterations = i, performance = .) %T>%
            print
          #})
          #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
          #print

          #plot(x=performances$iterations, y=performances$performance)
          #performances
          browser()
          intervention <- list(variable = 'A', when = c(5, 7), what = c(1,0))
          result <- osl$sample_iteratively(data = data.test[1,], randomVariables = c(W,A,Y), intervention = intervention)
          performance
          result
        },

        basicRegressionWithLags = function() {
          set.seed(12345)

          ######################################
          # Generate observations for training #
          #####################################
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
            mu <- aa*(0.9) + (1-aa)*(0.3)
            rnorm(length(mu), mu, sd=0.1)}})


          # Create the measures we'd like to include in our model
          # In this simulation we will include 2 lags and the latest data (non lagged)
          # Define the variables in the initial dataset we'd like to use
          W = "W"
          W1 = "W1"
          W2 = "W2"
          A = "A"
          Y = "Y"
          SMG.list <- list()
          SMG.list <- c(SMG.list, SMG.Lag$new(lags = 2, colnames.to.lag = (c(A, W, W1, Y))))
          SMG.list <- c(SMG.list, SMG.Latest.Entry$new(colnames.to.use = (c(A, W, W1, W2, Y))))
          summaryMeasureGenerator = SummaryMeasureGenerator$new(SMG.list = SMG.list, verbose = private$log)


          # We'd like to use the following features in our estimation:
          W  <- RandomVariable$new(family = 'gaussian', formula = W  ~ Y_lag_1 + W1_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2)
          W1 <- RandomVariable$new(family = 'gaussian', formula = W1 ~ W_lag_1)
          W2 <- RandomVariable$new(family = 'gaussian', formula = W2 ~ Y_lag_1)
          A  <- RandomVariable$new(family = 'binomial', formula = A  ~ W + Y_lag_1 + A_lag_1 + W_lag_1)
          Y  <- RandomVariable$new(family = 'gaussian', formula = Y  ~ A + W)

          randomVariables <- c(W, W1, W2, A, Y)

          # We add a margin so we don't have to worry about the presence of enough history
          margin <- 100

          # Generate a dataset we will use for testing.
          dataset <- private$sim$simulateWAY(private$test_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=private$log)

          # Generate some unrelated columns
          dataset[, W1 := rnorm(private$test_set_size + margin, 10, 1)]
          dataset[, W2 := rbinom(private$test_set_size + margin, 1, 0.5)]

          Data.Static$new(dataset = dataset) %>%
            summaryMeasureGenerator$setData(.)

          data.test <- summaryMeasureGenerator$getNextN(private$test_set_size)

          # Generate a dataset, from the same statistical model, which we will use to train our model
          dataset <- private$sim$simulateWAY(private$training_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=private$log)

          # Generate some unrelated columns
          dataset[, W1 := rnorm(private$training_set_size + margin, 10, 1)]
          dataset[, W2 := rbinom(private$training_set_size + margin, 1, 0.5)]

          data.train <- Data.Static$new(dataset = dataset)

          # Now run several iterations on the data
          #performances <- mclapply(seq(5,201,20), function(i) {

          i = 20
          data.train$reset()


          private$log && cat(private$log, 'Initializing OSL')
          osl <- OnlineSuperLearner$new(private$SL.library.definition,
                                        summaryMeasureGenerator = summaryMeasureGenerator,
                                        verbose = private$log)

          private$log && cat(private$log, 'Running OSL')

          # Divide by two here just so the initial size is a lot larger then each iteration, not really important
          estimators <- osl$fit(data.train, randomVariables = randomVariables,
                                initial_data_size = private$training_set_size / 2, max_iterations = i,
                                mini_batch_size = (private$training_set_size / 2) / i)


          private$log && cat(private$log, 'Predicting using all estimators')
          predictions <- osl$predict(data = copy(data.test), c(W,A,Y))

          performance <- osl$evaluateModels(data = copy(data.test), randomVariables = randomVariables) %>%
            c(iterations = i, performance = .) %T>%
            print
          #})
          #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
          #print

          #plot(x=performances$iterations, y=performances$performance)
          #performances
          browser()
          intervention <- list(variable = 'A', when = c(5, 7), what = c(1,0))
          result <- osl$sample_iteratively(data = data.test[1,], randomVariables = randomVariables, intervention = intervention)
          performance
          result
        }
  )
)


