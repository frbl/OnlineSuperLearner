#' ML.Local.lm
#'
#' Initializes a linear model (glm) for locally training an estimator. This
#' uses the normal GLM implementation as provided by the R stats package.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom stats lm
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(learning.rate = 0.1, family = "binomial", initialization.random = FALSE) }}{ 
#'     Initializes a new generalized linear model to use in the online super learner.
#'
#'     @param learning.rate double (default = 0.1) the learning rate to use
#'      when training the model.
#'
#'     @param family string (default = "binomial") the family to use for the
#'      estimator. Generally this should be binomial (since we are running
#'      various logistic regressions). However, if you have a good reason to
#'      use a different specification, change it here.
#'
#'     @param initialization.random (default = FALSE) each of the parameters
#'      needs to have an intial state. If this argument is true, all parameters
#'      will be randomly initialized.
#'
#'     @param verbose (default = FALSE) the verbosity of the fitting procedure
#'   } 
#' 
#'  \item{\code{get_validity}}{ 
#'     Active Method. Determines whether the current specification is valid or
#'     not. Note that this function does not return anything, instead it throws
#'     whenever something is incorrect.
#'   } 
#' }  

ML.Local.lm <- R6Class("ML.Local.lm",
  inherit = ML.Base,
  public =
    list(
      initialize = function(learning.rate = 0.1, family='binomial', initialization.random=FALSE, verbose = FALSE) {
        private$learning.rate <- learning.rate
        private$family <- family
        private$initialization.random <- initialization.random
        self$getValidity
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
        formula <- as.formula(self$create_formula(X = X, Y = Y))
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
        formula <- as.formula(self$create_formula(X = X, Y = Y))
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
    )
)
