#' ML.Local.lm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stats lm
#' @include ML.Local.R
ML.Local.lm <-
  R6Class (
           "ML.Local.lm",
           inherit = ML.Local,
           private =
             list(
                  learning.rate = NULL,
                  family = NULL,
                  initialization.random = NULL
                  ),
           active =
             list(
                  score = function() {
                    if(is.null(self$model)) {
                      throw('Fit a model first!')
                    }
                    summary(self$model)
                  },

                  getValidity = function() {
                    if (!(private$family %in% c('binomial', 'gaussian'))) {
                      throw('Family not supported', private$family)
                    }
                    if (private$learning.rate <= 0) {
                      throw('The learning rate should be greater than 0')
                    }
                  }
                  ),
           public =
             list(
                  initialize = function(learning.rate = 0.1, family='gaussian', initialization.random=FALSE) {
                    private$learning.rate = learning.rate
                    private$family = family
                    private$initialization.random = initialization.random
                    self$getValidity
                  },

                  predict = function(data, A, W) {
                    X <- c(A, W)
                    Xmat <- data[, X, with = FALSE]
                    Xmat <- as.matrix(cbind(rep(1, nrow(data)), Xmat))

                    if(private$family %in% c('binomial')) {
                      return(expit(Xmat %*% self$model))
                    }
                    if(private$family %in% c('gaussian')) {
                      return(Xmat %*% self$model)
                    }
                    throw('Family not found')
                  },

                  fit = function(train, Y, A, W){
                    formula <- as.formula(self$createFormula(Y = Y, A = A, W = W))
                    # If there is no model, we need to fit a model based on Nl observations.
                    # If we already have a model, we update the old one, given the new measurement
                    if(is.null(self$model)){

                      # Instead of using a GLM for initialization, we can also do a random initialization
                      if(private$initialization.random) {
                        Xmat <- model.matrix(formula, train)
                        self$model <- runif(ncol(Xmat))
                      } else {
                        model.fitted <- glm(formula,
                                            data=train,
                                            family=private$family)$coefficients

                        # TODO: do something with the NA's
                        nas <- is.na(model.fitted)
                        if(length(nas) > 0) { model.fitted[nas] <- runif(length(nas) - 1) }

                        self$model <- model.fitted
                      }

                    } else {
                      Ymat <- as.matrix(train[, Y, with = FALSE])

                      # Add the intercept to the matrix
                      Xmat <- model.matrix(formula, train)

                      # Make a prediction.
                      # Note that this could throw warnings if the model has not converged yet
                      suppressWarnings(prediction <- self$predict(train, A, W))
                      gradient <- (t(Xmat) %*% (prediction - Ymat))
                      self$model <- self$model - private$learning.rate * gradient
                    }
                  }
                  )
           )
