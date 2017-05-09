#' ML.Local.lm
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stats lm
#' @include ML.Local.R
ML.Local.lm <- R6Class("ML.Local.lm",
  inherit = ML.Base,
  private =
    list(
      learning.rate = NULL,
      family = NULL,
      initialization.random = NULL,

      do.predict = function(X_mat, m.fit) {
        X_mat <- as.matrix(cbind(rep(1, nrow(X_mat)), X_mat))

        if(private$family %in% c('binomial')) {
          return(expit(X_mat %*% m.fit$coef$coefficients))
        }
        if(private$family %in% c('gaussian')) {
          return(X_mat %*% m.fit$coef$coefficients)
        }
        throw('Family not found')
      },

      do.update = function(X_mat, Y_vals, m.fit, ...) {
        Y <- 'Y_vals'
        X <- colnames(X_mat)
        formula <- as.formula(self$create_formula(Y = Y, X = X))
        # If we already have a model, we update the old one, given the new measurement
        # Add the intercept to the matrix
        X_mat <- model.matrix(formula, X_mat)

        # Make a prediction.
        # Note that this could throw warnings if the model has not converged yet
        suppressWarnings(prediction <- self$predict(train, A, W))
        gradient <- (t(X_mat) %*% (prediction - Y_vals))
        m.fit$coef - private$learning.rate * gradient
      },

      do.fit = function (X_mat, Y_vals) {
        Y <- 'Y_vals'
        X <- colnames(X_mat)
        formula <- as.formula(self$create_formula(Y = Y, X = X))
        # If there is no model, we need to fit a model based on Nl observations.

        # Instead of using a GLM for initialization, we can also do a random initialization
        if(private$initialization.random) {
          X_mat <- model.matrix(formula, X_mat)
          return(runif(ncol(X_mat)))
        } 

        model.fitted <- glm(formula, data=train, family=private$family)$coefficients

        # TODO: do something with the NA's
        nas <- is.na(model.fitted)
        if(length(nas) > 0) { model.fitted[nas] <- runif(length(nas) - 1) }

        model.fitted
      }

      ),
  active =
    list(
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

      create_formula = function(Y, X) {
        X <- paste(X, collapse = ' + ')
        paste(Y, '~', X)
      }
    )
)
