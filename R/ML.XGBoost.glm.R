#' General Linear Model machine learning model, based on the XGBoost library
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom xgboost xgb.dump xgb.train xgb.DMatrix getinfo
#' @import xgboost
#' @field model the most recent / best model fitted.
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{This method is used to create object of this class. }
#'
#'   \item{\code{fit(data, X, Y)}}{Method to fit the linear model.}
#'}
ML.XGBoost.glm <-
  R6Class (
           "ML.XGBoost.glm",
           inherit = ML.XGBoost,
           private =
             list(
                  model.name = NULL,
                  lastdata = NULL,
                  param = NULL,
                  rounds = NULL,
                  verbosity = 0
                  ),
           public =
             list(
                  initialize = function() {
                    private$rounds <- 20
                    private$param <- list(objective = 'binary:logistic',
                                          booster = "gblinear",
                                          nthread = 8,
                                          alpha = 0,
                                          lambda = 0)
                  },


                  fit = function (data, X, Y) {
                    # If we have not yet fit a model, we are using the first n observations as the training set,
                    # and use the last observation as test set.  If we have fitted a model before, we use the set
                    # we previously used as a test set as the new training set to update the current model using
                    # this set.
                    if(is.null(self$model)){
                      test <- tail(data, 1)
                      train <- head(data, nrow(data)-1)
                    } else {
                      test <- data
                      train <- private$lastdata
                    }

                    # Set the test set we used now as the trainingset for the next iteration.
                    # This could probably be done more general, by giving it as input everytime (all ML models need this)
                    private$lastdata <- test

                    # Create train and test matrices
                    dtrain <- xgb.DMatrix(data = as.matrix(train[, X, with = FALSE]),
                                          label = train[, Y, with = FALSE][[Y]])

                    dtest <- xgb.DMatrix(data = as.matrix(test[, X, with = FALSE]),
                                         label = test[, Y, with = FALSE][[Y]])

                    watchlist <- list(eval = dtest, train = dtrain)

                    # Fit the model, giving the previously fitted model as a parameter
                    self$model <- xgb.train(data = dtrain,
                                            params = private$param,
                                            nrounds = private$rounds,
                                            watchlist = watchlist,
                                            xgb_model = private$model.name,
                                            verbose = private$verbosity)
                    xgb.save(self$model, 'test.dump')
                    #private$model.name <- 'test.dump'
                  }
                  )
           )
