#' OnlineSuperLearner.Simulation runs different simulations on the superlearner object.
#' You can run an example using:
#' suppressWarnings(devtools::load_all()); OnlineSuperLearner.Simulation$new()
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom condensier condensier_options
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @include RandomVariable.R
#' @include SMGFactory.R
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
        SL.library.definition = NULL,
        cv_risk_calculator = NULL,

        train = function(data.test, data.train, bounds, randomVariables, max_iterations, llW, llA, llY) {
          outcome.variables <- sapply(randomVariables, function(rv) rv$getY)
          smg_factory <- SMGFactory$new()

          pre_processor <- PreProcessor$new(bounds = bounds)
          summaryMeasureGenerator <- smg_factory$fabricate(randomVariables, pre_processor = pre_processor)

          margin <- 100
          Data.Static$new(dataset = data.test) %>%
            summaryMeasureGenerator$setData(.)
          data.test <- summaryMeasureGenerator$getNext(private$test_set_size)

          data.train <- Data.Static$new(dataset = data.train)

          private$log && cat(private$log, 'Initializing OSL')
          osl <- OnlineSuperLearner$new(private$SL.library.definition,
                                        summaryMeasureGenerator = summaryMeasureGenerator,
                                        pre_processor = pre_processor,
                                        verbose = private$log)

          private$log && cat(private$log, 'Running OSL')

          mini_batch_size <- (private$training_set_size / 2) / max_iterations
          mini_batch_size <- ifelse(is.na(mini_batch_size) || is.infinite(mini_batch_size), 1, mini_batch_size)
          print(mini_batch_size)
          # Divide by two here just so the initial size is a lot larger then each iteration, not really important
          risk <- osl$fit(data.train, randomVariables = randomVariables,
                                initial_data_size = private$training_set_size / 2,
                                max_iterations = max_iterations,
                                mini_batch_size = mini_batch_size) %T>%
            print


          private$log && cat(private$log, 'Predicting using all estimators')

          # Calculate prediction quality
          observed.outcome <- data.test[, outcome.variables, with=FALSE]
          predicted.outcome <- osl$predict(data = copy(data.test), randomVariables)

          performance <- private$cv_risk_calculator$calculate_evaluation(predicted.outcome = predicted.outcome,
                                                          observed.outcome = observed.outcome,
                                                          randomVariables = randomVariables) %>%
            c(iterations = max_iterations, performance = .) %T>%
            print

          #})
          #performances <- do.call(rbind, lapply(performances, data.frame)) %T>%
          #print

          #plot(x=performances$iterations, y=performances$performance)
          #performances
          intervention <- list(variable = 'A', when = c(2), what = c(0))
          tau = 2
          B <- 100

          pre <- options('warn')$warn
          options(warn=-1)
          doParallel::registerDoParallel(cores = 8)

          data.aisset <- copy(data.test)
          result <- osl$predict(data = data.aisset,
                        randomVariables = randomVariables,
                        all_estimators = FALSE,
                        discrete = TRUE,
                        continuous = FALSE,
                        sample = TRUE,
                        plot = TRUE)[[1]]

          result.approx <- foreach(i=seq(B), .combine=rbind) %dopar% {
            data.int <- private$sim$simulateWAY(tau, qw = llW, ga = llA, Qy = llY,
                                              intervention = intervention, verbose = FALSE)
            data.int$Y[tau]
          }
          


          #pdf('/tmp/osl/test.pdf')
          #plot(density(data.aisset$Y))
          #lines(density(data.aisset[data.aisset$A > 0.5]$Y), col='red')
          #lines(density(data.aisset[data.aisset$A < 0.5]$Y), col='blue')
          #dev.off()

          #pdf('/tmp/osl/test2.pdf')
          #plot(density(result$Y))
          #lines(density(data.aisset[result$A > 0.5]$Y), col='red')
          #lines(density(data.aisset[result$A < 0.5]$Y), col='blue')
          #dev.off()



          # Note that this won't work when we have an H2O estimator in the set. The parallelization will fail.
          result <- foreach(i=seq(B), .combine=rbind) %dopar% {
            osl$sample_iteratively(data = data.test[i,],
                                   randomVariables = randomVariables,
                                   intervention = intervention,
                                   tau = tau)[tau, 'Y']
          } %>%
            unlist


          options(warn=pre)

          result.mean <- result %>%
            mean

          result.approx.mean <- result.approx %>%
            mean

          print(paste('The difference between the estimate and approximation is: ', abs(result.mean - result.approx.mean)))

          result

          # Plot the convergence
          y1 <- cumsum(result.approx)/seq(along=result.approx)
          y2 <- cumsum(result)/seq(along=result)

          pdf('/tmp/osl/difference.pdf')
          plot(y1, ylim=range(c(y1, y2)))
          par(new=TRUE)
          plot(y2, ylim=range(c(y1, y2)), col="red", axes = FALSE, xlab = "", ylab = "")
          dev.off()

          browser()

          osl$info

          lapply(performance, function(x) {lapply(x,mean)})
        }
        ),
  public =
    list(
        initialize = function() {
          condensier_options(parfit=FALSE)
          options(warn=1)
          private$sim  <- Simulator.GAD$new()
          private$training_set_size <- 1e3
          private$cv_risk_calculator <- CrossValidationRiskCalculator$new()
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


          #algos <- append(algos, list(list(algorithm = 'ML.XGBoost',
                                  #algorithm_params = list(alpha = 0),
                                  #params = list(nbins = c(16,40), online = TRUE))))

          #algos <- append(algos, list(list(algorithm = 'ML.H2O.gbm',
                                  #algorithm_params = list(ntrees=c(10,20), min_rows=1),
                                  #params = list(nbins = c(6), online = TRUE))))

          #algos <- append(algos, list(list(algorithm = 'ML.H2O.randomForest',
                                  #algorithm_params = list(ntrees=c(10,20)),
                                  #params = list(nbins = c(16), online = TRUE))))

          algos <- append(algos, list(list(algorithm = 'condensier::speedglmR6',
                                  #algorithm_params = list(),
                                  params = list(nbins = c(39, 40), online = FALSE))))

          #algos <- append(algos, list(list(algorithm = 'condensier::glmR6',
                                  ##algorithm_params = list(),
                                  #params = list(nbins = c(16, 20, 24, 30, 34, 40), online = FALSE))))

          private$SL.library.definition <- algos

          # Run the simulations
          self$basicRegressionWithLags()
          #self$verySimpleSimulation()
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


          # We'd like to use the following features in our estimation:
          W <- RandomVariable$new(formula = Y ~ A + W, family = 'gaussian')
          A <- RandomVariable$new(formula = A ~ W + Y_lag_1 + A_lag_1 + W_lag_1, family = 'binomial')
          Y <- RandomVariable$new(formula = W ~ Y_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2, family = 'gaussian')
          randomVariables <- c(W, A, Y)

          # Generate a dataset we will use for testing.
          margin <- 100
          data.test <- private$sim$simulateWAY(private$test_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=private$log)
          data.train <- private$sim$simulateWAY(private$training_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=private$log)

          # Create the bounds
          bounds <- PreProcessor.generate_bounds(data.train)

          # Create the measures we'd like to include in our model
          # In this simulation we will include 2 lags and the latest data (non lagged)
          # Define the variables in the initial dataset we'd like to use
          private$train(data.test, data.train, bounds, randomVariables, 2)
          private$train(data.test, data.train, bounds, randomVariables, max_iterations = 2,
                        llW = llW,
                        llA = llA, 
                        llY = llY)
        },

        basicRegressionWithLags = function() {
          set.seed(12345)

          ######################################
          # Generate observations for training #
          #####################################
          #llW <- list(stochMech=function(numberOfBlocks) {
                        #rnorm(numberOfBlocks, 0, 10)
                      #},
                      #param=c(0, 0.5, -0.25, 0.1),
                      #rgen=identity)

          llW <- list(list(stochMech = function(numberOfBlocks) {
                        rnorm(numberOfBlocks, 0, 10)
                      },
                      param=c(0, 0.5, -0.25, 0.1),
                      rgen=identity),
                    list(stochMech = function(numberOfBlocks) {
                        runif(numberOfBlocks, 0, 1)
                      },
                      param = c(0, 0.5),
                      rgen = identity),
                    list(stochMech = function(numberOfBlocks) {
                        runif(numberOfBlocks, 0, 1)
                      },
                      param = c(0, 0.5),
                      rgen = identity)
                    )

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
            rnorm(length(mu), mu, sd=0.0001)}})

          # We'd like to use the following features in our estimation:
          W  <- RandomVariable$new(family = 'gaussian', formula = W  ~ Y_lag_1 + W2_lag_1 + A_lag_1 +  W_lag_1 + Y_lag_2)
          W2 <- RandomVariable$new(family = 'gaussian', formula = W2 ~ W_lag_1)
          W3 <- RandomVariable$new(family = 'gaussian', formula = W3 ~ Y_lag_1)
          A  <- RandomVariable$new(family = 'binomial', formula = A  ~ W + Y_lag_1 + A_lag_1 + W_lag_1)
          Y  <- RandomVariable$new(family = 'gaussian', formula = Y  ~ A + W + Y_lag_1 + A_lag_1 + W_lag_1)
          randomVariables <- c(W, W2, W3, A, Y)

          # Create the measures we'd like to include in our model
          # In this simulation we will include 2 lags and the latest data (non lagged)
          # Define the variables in the initial dataset we'd like to use

          # Generate a dataset we will use for testing.
          # We add a margin so we don't have to worry about the presence of enough history
          margin <- 100
          data.test <- private$sim$simulateWAY(private$test_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=private$log)
          data.train <- private$sim$simulateWAY(private$training_set_size + margin, qw=llW, ga=llA, Qy=llY, verbose=private$log)

          # Create the bounds
          bounds <- PreProcessor.generate_bounds(data.train)

          # Create the measures we'd like to include in our model
          # In this simulation we will include 2 lags and the latest data (non lagged)
          # Define the variables in the initial dataset we'd like to use
          private$train(data.test, data.train, bounds, randomVariables, max_iterations = 20, llW, llA, llY)
        }
  )
)

