#' Base class for any XGBoost machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include ML.Base.R
ML.XGBoost <-
  R6Class(
          "ML.XGBoost",
          inherit = ML.Base,
          private = list(
                         data.previous = NULL
                         ),
          public = list(
                        param = NULL,
                        rounds = NULL,
                        model.name = NULL,
                        verbosity = 0,

                        initialize = function(){
                          file.remove(self$model.name)
                        },


                        fit = function (data, Y, A, W) {
                          # If we have not yet fit a model, we are using the first n observations as the training set,
                          # and use the last observation as test set.  If we have fitted a model before, we use the set
                          # we previously used as a test set as the new training set to update the current model using
                          # this set.
                          X  <- c(A, W)
                          if(is.null(self$model)){
                            model.name <- NULL
                            test <- tail(data, 1)
                            train <- head(data, nrow(data)-1)
                          } else {
                            model.name <- self$model.name
                            test <- data
                            train <- private$data.previous
                          }

                          # Set the test set we used now as the trainingset for the next iteration.
                          # This could probably be done more general, by giving it as input everytime (all ML models need this)
                          private$data.previous <- test

                          # Create train and test matrices
                          dtrain <- xgb.DMatrix(data = as.matrix(train[, X, with = FALSE]),
                                                label = train[, Y, with = FALSE][[Y]])

                          dtest <- xgb.DMatrix(data = as.matrix(test[, X, with = FALSE]),
                                               label = test[, Y, with = FALSE][[Y]])

                          watchlist <- list(eval = dtest, train = dtrain)

                          # Fit the model, giving the previously fitted model as a parameter
                          self$model <- xgb.train(data = dtrain,
                                                  params = self$param,
                                                  nrounds = self$rounds,
                                                  watchlist = watchlist,
                                                  xgb_model = self$model,
                                                  verbose = self$verbosity)
                          xgb.save(self$model, self$model.name)
                        }
                        )
          )
