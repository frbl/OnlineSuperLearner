#' OnlineSuperLearner.Simulation runs different simulations on the superlearner object.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @export
OnlineSuperLearner.Simulation <-
  R6Class (
           "OnlineSuperLearner.Simulation",
           private =
             list(
                  sim = NULL,
                  nobs = NULL,
                  SL.library.definition = NULL
                  ),
           public =
             list(
                  initialize = function() {
                    private$sim  <- Simulator.GAD$new()
                    private$nobs <- 1e5
                    #algos <- list(list(description='ML.H2O.randomForest-1tree',
                                           #algorithm = 'ML.H2O.randomForest',
                                           #params = list(ntrees = 1)))

                    #algos <- append(algos, list(list(description='ML.H2O.randomForest-50trees',
                                           #algorithm = 'ML.H2O.randomForest',
                                           #params = list(ntrees = 50))))

                    #algos <- append(algos, list(list(description='ML.H2O.gbm',
                                           #algorithm = 'ML.H2O.gbm')))

                    algos <- list(list(description='MLD.Density.Estimation-50bins',
                                           algorithm = 'MLD.Density.Estimation',
                                           params = list(nbins = 3)))

                    #algos <- append(algos, list(list(description='MLD.Density.Estimation-100bins',
                                           #algorithm = 'MLD.Density.Estimation',
                                           #params = list(nbins = 100))))

                    private$SL.library.definition <- algos

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
                    log <- Arguments$getVerbose(-8, timestamp=TRUE)

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

                    llY <- list(rgen={function(AW){
                      aa <- AW[, "A"]
                      ww <- AW[, grep("[^A]", colnames(AW))]
                      mu <- aa*(0.4-0.2*sin(ww)+0.05*ww) +
                        (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
                      rnorm(length(mu), mu, sd=0.1)}})
                    

                    # Define the variables in the initial dataset we'd like to use
                    Y = "Y"
                    W = c("W")
                    A = c("A")

                    # Create the measures we'd like to include in our model
                    # In this simulation we will include 2 lags and the latest data (non lagged)
                    SMG.list <- list()
                    SMG.list <- c(SMG.list, SMG.Lag$new(lags = 2, colnames.to.lag = (c(A, W, Y))))
                    SMG.list <- c(SMG.list, SMG.Latest.Entry$new(colnames.to.use = (c(A, W, Y))))

                    summaryMeasureGenerator = SummaryMeasureGenerator$new(SMG.list = SMG.list, verbose = log) 
                    # We'd like to use the following features in our estimation:
                    Y = "Y"
                    W = c("Y_lag_1","Y_lag_2","W", "W_lag_1", "W_lag_2", "A_lag_1", "A_lag_2")
                    A = c("A")


                    ## The predict should be ran for each dist separately
                    # Once Y ~ its dependencies
                    # Once a ~ its dependencies
                    # Once W ~ its dependencies
                    #Y.eq <- Y ~ A + W +  Y_lag_1 + A_lag_1 + W_lag_1 + Y_lag_2 + A_lag_2 + W_lag_2
                    #A.eq <- A ~ W + Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2 + A_lag_2 + W_lag_2
                    #W.eq <- W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2 + A_lag_2 + W_lag_2
                    Y.eq <- Y ~ A + W
                    A.eq <- A ~ W
                    W.eq <- W ~ W_lag_1

                    # Generate a dataset we will use for testing.
                    # TODO: This step is really slow, because of the getNextN(800)
                    private$sim$simulateWAYOneTrajectory(1000, qw=llW, ga=llA, Qy=llY, verbose=log) %>%
                      Data.Static$new(dataset = .) %>%
                      summaryMeasureGenerator$setData(.)

                    data.test <- summaryMeasureGenerator$getNextN(80)

                    # Generate a dataset, from the same statistical model, which we will use to train our model
                    data.train <-
                      private$sim$simulateWAYOneTrajectory(private$nobs, qw=llW, ga=llA, Qy=llY, verbose=log) %>%
                      Data.Static$new(dataset = .)

                    # Now run several iterations on the data
                    #performances <- mclapply(seq(5,201,20), function(i) {

                    i = 20000
                    data.train$reset()

                    osl <- OnlineSuperLearner$new(private$SL.library.definition,
                                                  summaryMeasureGenerator = summaryMeasureGenerator,
                                                  verbose = log)

                    estimators <- osl$run(data.train,
                                          Y = Y,
                                          A = A,
                                          W = W,
                                          Y.eq = Y.eq,
                                          A.eq = A.eq,
                                          W.eq = W.eq,
                                          initial.data.size = 20, max.iterations = i,
                                          mini.batch.size = 1000)

                    osl$evaluateModels(data = copy(data.test), W = W, A = A, Y = Y) %>%
                      c(iterations = i, performance = .) %>%
                      print
                    #})
                    #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
                    #print

                    #plot(x=performances$iterations, y=performances$performance)
                    #performances
                  }
                  )

           )

