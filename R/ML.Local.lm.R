#' ML.Local.lm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stats lm
#' @include ML.Local.R
#' @export
ML.Local.lm <-
  R6Class (
           "ML.Local.lm",
           inherit = ML.Local,
           private =
             list(
                  ),
           active =
             list(
                  score = function() {
                    if(is.null(self$model)) {
                      throw('Fit a model first!')
                    }
                    summary(self$model)$r.squared
                  }
                  ),
           public =
             list(
                  initialize = function() {
                  },

                  predict = function(data, A, W) {
                    X <- c(A, W)
                    predict(self$model, data[, X, with = FALSE], type = 'response')
                  },

                  fit = function(data, Y, A, W){
                    formula <- self$createFormula(Y = Y,A = A,W = W)
                    learningrate = .1

                    # If there is no model, we need to fit a model based on Nl observations.
                    # If we already have a model, we update the old one, given the new measurement
                    if(is.null(self$model)){

                      # We suppress warnings as we are doing our model fitting based on non binary data.
                      # R will complain about this.
                      suppressWarnings(
                        self$model <- glm(formula,
                                          data=data,
                                          family=binomial())
                      )
                    } else {
                      # model matrix
                      X <- c(A, W)
                      Xmat <- as.matrix(data[, X, with = FALSE])
                      Ymat <- as.matrix(data[, Y, with = FALSE])

                      # Add the intercept to the matrix
                      Xmat <- cbind(intercept=c(1), Xmat)

                      # prediction
                      prediction <- self$predict(data, A, W)

                      # Calculate the gradient
                      grad <- - t(Xmat) %*% (Ymat - prediction)

                      # Update the original coefficients of our glm
                      coef <- self$model$coefficients - learningrate * grad

                      # Update the actual coefficients of our fit
                      self$model$coefficients <- coef
                    }
                    self$model
                  }
                  )
           )
