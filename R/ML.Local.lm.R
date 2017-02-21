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
                  family = NULL
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
                  initialize = function(learning.rate = 0.1, family='gaussian') {
                    private$learning.rate = learning.rate
                    private$family = family
                    self$getValidity
                  },

                  predict = function(data, A, W) {
                    X <- c(A, W)
                    if(private$family %in% c('binomial')) {
                      return(predict(self$model, data[, X, with = FALSE], type='response'))
                    }
                    predict(self$model, data[, X, with = FALSE])
                  },

                  fit = function(train, Y, A, W){
                    # If there is no model, we need to fit a model based on Nl observations.
                    # If we already have a model, we update the old one, given the new measurement
                    if(is.null(self$model)){
                      formula <- self$createFormula(Y = Y, A = A, W = W)
                      self$model <- glm(formula,
                                        data=train,
                                        family=private$family)
                    } else {
                      # model matrix
                      X <- c(A, W)
                      Xmat <- as.matrix(train[, X, with = FALSE])
                      Ymat <- as.matrix(train[, Y, with = FALSE])

                      # Add the intercept to the matrix
                      Xmat <- cbind(intercept=c(1), Xmat)

                      # prediction
                      prediction <- self$predict(train, A, W)

                      # TODO: Calculate the gradient descent gradient.
                      grad <- (t(Xmat) %*% (prediction - Ymat))

                      # Update the original coefficients of our glm
                      coefs <- self$model$coefficients - (private$learning.rate * grad)

                      # Update the actual coefficients of our fit
                      self$model$coefficients <- coefs
                    }
                  }
                  )
           )
