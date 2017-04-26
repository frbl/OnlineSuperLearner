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
                                  params = list(nbins = 6))))


          algos <- append(algos, list(list(algorithm = 'tmlenet::glmR6',
                                  #algorithm_params = list(),
                                  params = list(nbins = c(16, 120))))) 

          private$SL.library.definition <- algos

          # Run the simulations
          self$basicRegression()
          self$basicRegressionWithLags()
          self$basicClassification()
          self$basicClassificationWithLags()
          #self$verySimpleSimulation()
        },

        verySimpleSimulation = function() {
          generate_data <- function(nobs, mean_treated = 0.9, mean_not_treated = 0.3) {
            # W is completely unreleated
            W <- rnorm(nobs, 0, 1)
            WU <- rnorm(nobs, 3, 5)

            # A is just a random binary number
            A <- rbinom(n=nobs,size=1, prob=0.5)

            mean <- A*(mean_treated) + (1-A)*(mean_not_treated)
            mean <- mean + 0.3*W + 0.1 * WU
            Y <- rnorm(nobs,mean,1)
            Y[Y < 0] <- 0
            #Y <- Y + rnorm(nobs,0,max(c(mean_treated, mean_not_treated)))
            plot(density(Y))

            data.table(W=W, A=A, Y=Y)
          }

          # Define the variables in the initial dataset we'd like to use
          Y = "Y"
          W = "W"
          A = "A"
          # In this simulation we will include 2 lags and the latest data (non lagged)
          SMG.list <- list()
          SMG.list <- c(SMG.list, SMG.Lag$new(lags = 1, colnames.to.lag = (c(A, W, Y))))
          SMG.list <- c(SMG.list, SMG.Latest.Entry$new(colnames.to.use = (c(A, W, Y))))
          summaryMeasureGenerator = SummaryMeasureGenerator$new(SMG.list = SMG.list, verbose = private$log) 

          # We'd like to use the following features in our estimation:
          Y.eq <- Y ~ A + W
          A.eq <- A ~ W# + Y_lag_1 + A_lag_1
          W.eq <- W ~ Y_lag_1# + A_lag_1 +  W_lag_1 + Y_lag_2
          W <- RandomVariable$new(formula = W.eq, family = 'gaussian')
          A <- RandomVariable$new(formula = A.eq, family = 'binomial')
          Y <- RandomVariable$new(formula = Y.eq, family = 'gaussian')


          # Generate a dataset we will use
          generate_data(private$test_set_size + 100) %>%
            Data.Static$new(dataset = .) %>%
            summaryMeasureGenerator$setData(.)

          data.test <- summaryMeasureGenerator$getNextN(private$test_set_size)

          # Generate a dataset, from the same statistical model, which we will use to train our model
          data.train <- generate_data(private$training_set_size) %>%
            Data.Static$new(dataset = .)

          # Now run several iterations on the data
          #performances <- mclapply(seq(5,201,20), function(i) {


          i = 1000
          data.train$reset()

          osl <- OnlineSuperLearner$new(private$SL.library.definition,
                                        summaryMeasureGenerator = summaryMeasureGenerator,
                                        verbose = private$log)

          estimators <- osl$run(data.train, Y = Y, A = A, W = W,
                                initial_data_size = 2000, max_iterations = i,
                                mini_batch_size = 3)

          osl$evaluateModels(data = copy(data.test), randomVariables = c(W, A, Y)) %>%
            c(iterations = i, performance_or_error = .) %>%
            print
          #})
          #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
          #print

          #plot(x=performances$iterations, y=performances$performance)
          #performances
          #intervention <- list(variable = 'A', when = c(5, 7), what = c(1,0))
          #result <- osl$sample_iteratively(data = data.test[1,], randomVariables = c(W,A,Y), intervention = intervention)
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
          private$sim$simulateWAY(25000, qw=llW, ga=llA, Qy=llY, verbose=private$log) %>%
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
          estimators <- osl$run(data.train, Y = Y, A = A, W = W,
                                initial_data_size = 20000, max_iterations = i,
                                mini_batch_size = 10)

          predictions <-osl$predict(data = copy(data.test), c(W,A,Y), discrete=TRUE) 
          performance <- osl$evaluateModels(data = copy(data.test), randomVariables = c(W, A, Y)) %>%
            c(iterations = i, performance = .) %T>%
            print
          #})
          #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
          #print

          #plot(x=performances$iterations, y=performances$performance)
          #performances
          intervention <- list(variable = 'A', when = c(5, 7), what = c(1,0))
          result <- osl$sample_iteratively(data = data.test[1,], randomVariables = c(W,A,Y), intervention = intervention)
          browser()
        }
  )
)


