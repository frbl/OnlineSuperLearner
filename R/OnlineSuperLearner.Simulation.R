#' OnlineSuperLearner.Simulation runs different simulations on the superlearner object.
#'
#' @docType class
#' @importFrom R6 R6Class
OnlineSuperLearner.Simulation <-
  R6Class (
           "OnlineSuperLearner.Simulation",
           private =
             list(
                  sim = NULL,
                  nobs = NULL,
                  SL.Library = NULL
                  ),
           public =
             list(
                  initialize = function() {
                    private$sim  <- Simulator.GAD$new()
                    private$nobs <- 1e5
                    private$SL.Library = c('ML.Local.lm',
                                           'ML.H2O.glm',
                                           'ML.H2O.gbm')

                    # Run the simulations
                    self$basicRegression()
                    self$basicRegressionWithLags()
                    self$basicClassification()
                    self$basicClassificationWithLags()
                  },

                  basicClassification = function() {
                    set.seed(12345)
                    log <- FALSE
                  },

                  basicClassificationWithLags = function() {
                    set.seed(12345)
                    log <- FALSE
                  },

                  basicRegression = function() {
                    set.seed(12345)
                    log <- FALSE
                  },

                  basicRegressionWithLags = function() {
                    set.seed(12345)
                    log <- FALSE

                    ######################################
                    # Generate observations for training #
                    #####################################
                    llW <- list(stochMech=rnorm,
                                param=c(0, 0.5, -0.25, 0.1),
                                rgen=identity)

                    llA <- list (stochMech=function(ww) {
                                   rbinom(length(ww), 1, expit(ww))
                                },
                                param=c(-0.1, 0.1, 0.25),
                                rgen=function(xx, delta=0.05){
                                  rbinom(length(xx), 1, delta+(1-2*delta)*expit(xx))
                                })

                    llY <- list(stochMech=function(aa, ww){
                                  aa*ww+(1-aa)*(-ww)
                                },
                                param=c(0.1, 0.1, 0.1, 0.05, -0.01),
                                rgen=identity)


                    # Define the variables in the initial dataset we'd like to use
                    Y = "Y1"
                    W = c("W1")
                    A = c("V2")

                    # Create the measures we'd like to include in our model
                    # In this simulation we will include 2 lags and the latest data (non lagged)
                    SMG.list <- list()
                    SMG.list <- c(SMG.list, SMG.Lag$new(lags = 2, colnames.to.lag = (c(A, W, Y))))
                    SMG.list <- c(SMG.list, SMG.Latest.Entry$new(colnames.to.use = (c(A, W, Y))))

                    summaryMeasureGenerator = SummaryMeasureGenerator$new(SMG.list = SMG.list)

                    # We'd like to use the following features in our estimation:
                    Y = "Y1"
                    W = c("Y1_lag_1","Y1_lag_2","W1", "W1_lag_1", "W1_lag_2", "V2_lag_1", "V2_lag_2")
                    A = c("V2")

                    # Generate a dataset we will use for testing.
                    # TODO: This step is really slow, because of the getNextN(800)
                    private$sim$simulateWAY(1000, qw=llW, ga=llA, Qy=llY, verbose=log) %>%
                      Data.Static$new(dataset = .) %>%
                      summaryMeasureGenerator$setData(.)

                    data.test <- summaryMeasureGenerator$getNextN(800)

                    # Generate a dataset, from the same statistical model, which we will use to train our model
                    data.train <-
                      private$sim$simulateWAY(private$nobs, qw=llW, ga=llA, Qy=llY, verbose=log) %>%
                      Data.Static$new(dataset = .)

                    # Now run several iterations on the data
                    performances <- lapply(seq(5,201,20), function(i) {
                      data.train$reset()

                      osl <- OnlineSuperLearner$new(private$SL.Library,
                                                    summaryMeasureGenerator = summaryMeasureGenerator,
                                                    verbose = log)

                      estimators <- osl$run(data.train,
                                            Y = Y,
                                            A = A,
                                            W = W,
                                            initial.data.size = 30, max.iterations = i)

                      osl$evaluateModels(data = copy(data.test),
                                        W = W, A = A, Y = Y) %>% c(iterations = i, performance = .)
                    })
                    performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
                      print

                    plot(x=performances$iterations, y=performances$performance)
                    performances
                  }
                  )

           )

