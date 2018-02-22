#' ML.XGBoost
#'
#' Base class for any XGBoost machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom xgboost xgb.dump xgb.train xgb.DMatrix getinfo
#' @include ML.Base.R
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(booster = 'gblinear', max_depth = 6, nthread = 1, alpha = 0, lambda = 0, rounds = 200, gamma = 0, eta = 0.3, objective = 'binary:logistic', verbose = FALSE)}}{
#'     Initializes a new XGBoosted estimator. See the underlying xgboost
#'     packages for more details. This estimator allows to tweak several
#'     hyperparameters (see params). By default XGBoost uses elasticnet for
#'     penalizing the fitted model, the amount of penalization can be tweaked
#'     using the alpha (L1 regularization) and lambda (L2 regularization). See
#'     https://github.com/dmlc/xgboost/blob/master/doc/parameter.md
#'
#'     @param booster string (default = 'gblinear') the booster to use for
#'      fitting the booster. Can be either of \code{gbtree}, \code{gblinear} or
#'      \code{dart}.
#'
#'     @param max_depth integer (default = 6) the max depth of the GBM.
#'
#'     @param nthread integer (default = 1) the number of threads to run the
#'      XBoost algortihm on. Note!! Setting this to a different setting might
#'      cause unwanted behavior! If set to -1, it will use all cores available.
#'
#'     @param alpha double L1 regularization parameter
#'
#'     @param lambda double L2 regularization parameter
#'
#'     @param rounds = The number of rounds for boosting
#'
#'     @param gamma minimum loss reduction required to make a further partition
#'      on a leaf node of the tree. The larger, the more conservative the algorithm will be.
#'
#'     @param eta double (default = 0.3) the stepsize used
#'
#'     @param objective string (default = 'binary:logistic') the objective to
#'      optimize.
#'
#'   }
#'
#'   \item{\code{get_nthread}}{
#'     Active method. Function that returns the number of threads the XGBoost
#'     algorithm runs on.
#'   }
#'
#'   \item{\code{get_validity}}{
#'     Active method. Function that shows wheter the current configuration of
#'     the booster is valid. The function returns \code{TRUE} if everything is
#'     specified correctly. It will throw an error (with the error messages)
#'     when something is misspecified. This function is automatically called
#'     after initialization.
#'   }
#' }
ML.XGBoost <- R6Class("ML.XGBoost",
  inherit = ML.Base,
  public =
    list(
      fitfunname='xgboost',
      lmclass='xgboostR6',
      initialize = function(booster = 'gblinear', max_depth = 6, nthread = 1, alpha = 0, lambda = 0, rounds = 200, gamma = 0, eta = 0.3, objective = 'binary:logistic', verbose = FALSE) {

        if (nthread == -1) nthread <- parallel::detectCores()
        private$nthread <- nthread

        private$rounds <- Arguments$getInteger(rounds, c(1, Inf))
        private$params <- list(objective = Arguments$getCharacter(objective),
          booster = Arguments$getCharacter(booster),
          nthread = nthread,
          max_depth =  Arguments$getNumeric(max_depth, c(1, Inf)),
          alpha   = Arguments$getNumeric(alpha, c(0, 1)),
          gamma   = Arguments$getNumeric(gamma, c(0, Inf)),
          eta     = Arguments$getNumeric(eta, c(1e-10, Inf)),
          lambda  = Arguments$getNumeric(lambda, c(0, 1))
        )

        private$verbosity <- Arguments$getVerbose(verbose)
        self$get_validity
        super$initialize()
      }
    ),
  active =
    list(
      get_nthread = function() {
        return(private$nthread)
      },

      get_validity = function() {
        errors <- c()
        allowed_boosters <- c('gbtree', 'gblinear', 'dart')
        if(!(private$params$booster %in% allowed_boosters)) {
          errors <- c(errors, paste('Booster',private$params$booster,'is not in list of allowed boosters:', paste(allowed_boosters, collapse=' ')))
        }
        if(length(errors) > 0) throw(errors)
        TRUE
      },

      get_rounds = function() {
        return(private$rounds)
      },

      get_params = function() {
        return(private$params)
      }
    ),
  private =
    list(
      params = NULL,
      rounds = NULL,
      verbosity = NULL,
      nthread = NULL,

      do.predict = function(X_mat, m.fit) {

        #if(!('Intercept' %in% colnames(X_mat))) browser()
        if (any(is.na(m.fit$coef))) {
          result <- super$do.predict(X_mat, m.fit)
        } else {
          result <- predict(m.fit$coef, X_mat, type='response')
        }
        if(any(is.na(result))) browser()
        return(result)
      },

      do.update = function(X_mat, Y_vals, m.fit, ...) {
        # By default the xgbtrain function uses the old model as a parameter.
        # Therefore we can just simply call the fit function
        if (self$get_params$booster != 'gblinear') {
          private$params <- modifyList(self$get_params, list(process_type = 'update', updater = 'refresh', refresh_leaf = FALSE))
        }
        private$do.fit(X_mat = X_mat, Y_vals = Y_vals, coef = m.fit$coef)
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

        estimator <- xgb.train(
          data = dtrain,
          params     = self$get_params,
          nrounds    = self$get_rounds,
          #watchlist = watchlist,
          xgb_model  = coef,
          verbose    = 0
        ) #private$verbosity)

        if(any(is.na(estimator))) browser()

        return(estimator)
    }
    )
)

