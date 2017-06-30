#' Base class for any XGBoost machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom xgboost xgb.dump xgb.train xgb.DMatrix getinfo
#' @include ML.Base.R
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(booster = "gblinear", alpha = 0, lambda = 0, rounds = 200)}}{
#'     Initializes a new XGBoosted estimator. See the underlying xgboost packages for more details. This estimator
#'     allows to tweak several hyperparameters (see params). By default XGBoost uses elasticnet for penalizing the
#'     fitted model, the amount of penalization can be tweaked using the alpha (L1 regularization) and lambda (L2
#'     regularization). See https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
#'     @param booster = the booster to use for fitting the booster. Can be either of \code{gbtree},
#'                      \code{gblinear} or \code{dart}.
#'     @param eta = the stepsize used
#'     @param alpha = L1 regularization parameter
#'     @param lambda = L2 regularization parameter
#'     @param gamma = minimum loss reduction required to make a further partition on a leaf node of the tree.
#'                    The larger, the more conservative the algorithm will be.
#'     @param rounds = The number of rounds for boosting
#'   }
#'   \item{\code{get_validity}}{
#'     Function that shows wheter the current configuration of the booster is valid
#'   }
#' }
ML.XGBoost <- R6Class("ML.XGBoost",
  inherit = ML.Base,
  public =
    list(
      fitfunname='xgboost',
      lmclass='xgboostR6',
      initialize = function(booster = 'gblinear', alpha = 0, lambda = 0, rounds = 200, gamma = 0, eta = 0.3, objective = 'binary:logistic', verbose = FALSE) {

        private$rounds <- Arguments$getInteger(rounds, c(1, Inf))
        private$verbosity <- Arguments$getVerbose(verbose)

        private$params <- list(objective = Arguments$getCharacter(objective),
                              booster = Arguments$getCharacter(booster),
                              nthread = 8,
                              alpha   = Arguments$getNumeric(alpha, c(0, 1)),
                              gamma   = Arguments$getNumeric(gamma, c(0, Inf)),
                              eta     = Arguments$getNumeric(eta, c(1e-10, Inf)),
                              lambda  = Arguments$getNumeric(lambda, c(0, 1)))

        self$get_validity
        super$initialize()
      }
    ),
  active =
    list(
      get_validity = function() {
        errors <- c()
        allowed_boosters <- c('gbtree', 'gblinear', 'dart')
        if(!(private$params$booster %in% allowed_boosters)) {
          errors <- c(errors, paste('Booster',private$params$booster,'is not in list of allowed boosters:', paste(allowed_boosters, collapse=' ')))
        }
        if(length(errors) > 0) throw(errors)
        TRUE
      }
    ),
  private =
    list(
      params = NULL,
      rounds = NULL,
      verbosity = NULL,

      do.predict = function(X_mat, m.fit) {

        #result <- ifelse(any(is.na(m.fit$coef)),
                         #super$do.predict(X_mat, m.fit),
                         #predict(m.fit$coef, X_mat))
        if (any(is.na(m.fit$coef))) {
          result <- super$do.predict(X_mat, m.fit)
        } else {
          result <- predict(m.fit$coef, X_mat)
        }
        return(result)
      },

      do.update = function(X_mat, Y_vals, m.fit, ...) {
        # By default the xgbtrain function uses the old model as a parameter. Therefore we can just simply call
        # the fit function
        private$do.fit(X_mat, Y_vals, m.fit$coef)

      },

      do.fit = function (X_mat, Y_vals, coef = NULL) {
        # If we have not yet fit a model, we are using the first n observations as the training set,
        # and use the last observation as test set.  If we have fitted a model before, we use the set
        # we previously used as a test set as the new training set to update the current model using
        # this set.
        # Set the test set we used now as the trainingset for the next iteration.
        # This could probably be done more general, by giving it as input everytime (all ML models need this)

        # Create train and test matrices
        dtrain <- xgb.DMatrix(data = X_mat,
                              label = Y_vals)

        #dtest <- xgb.DMatrix(data = as.matrix(test[, X, with = FALSE]),
        #label = test[, Y, with = FALSE][[Y]])

        #watchlist <- list(eval = dtest, train = dtrain)

        # Fit the model, giving the previously fitted model as a parameter
        if (any(is.null(coef) || is.na(coef))) {
          coef <- NULL
        }

        estimator <- xgb.train(data = dtrain,
                  params     = private$params,
                  nrounds    = private$rounds,
                  #watchlist = watchlist,
                  xgb_model  = coef,
                  verbose    = private$verbosity)
        if(any(is.na(estimator))) browser()

        return(estimator)
    }
    )
)

