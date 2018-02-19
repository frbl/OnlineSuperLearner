#' ML.GLMnet
#'
#' Wrapper for a glmnet estimator. From their discription:
#' Extremely efficient procedures for fitting the entire lasso or elastic-net
#' regularization path for linear regression, logistic and multinomial
#' regression models, Poisson regression and the Cox model. Two recent
#' additions are the multiple-response Gaussian, and the grouped multinomial
#' regression. The algorithm uses cyclical coordinate descent in a path-wise
#' fashion, as described in \url{http://www.jstatsoft.org/v33/i01}.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom glmnet glmnet
#' @include ML.Base.R
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(family = "binomial", alpha = 0.5, nlambda = 100, verbose = FALSE) }}{ 
#'     Creates a new GLMnet model
#'
#'     @param family string (default = "binomial") the family to use for the
#'      estimator. Generally this should be binomial (since we are running
#'      various logistic regressions). However, if you have a good reason to
#'      use a different specification, change it here.
#'
#'     @param alpha double (default = 0.5) the elasticnet mixing parameter, with $0 <= alpha
#'      <= 1$. $alpha=1$ is the lasso penalty, and $alpha=0$ the ridge
#'      penalty.
#'
#'     @param nlambda integer (default = 100) the number of lambda values.
#'
#'     @param verbose (default = FALSE) the verbosity of the fitting procedure
#'   } 
#'
#'   \item{\code{get_validity }}{ 
#'     Active method. Checks whether the specified configuration is valid.
#'
#'     @return boolean, true if it is a valid configuration.
#'   } 
#' }  
ML.GLMnet <- R6Class("ML.GLMnet",
  inherit = ML.Base,
  public =
    list(
      fitfunname='glmnet',
      lmclass='glmnet',
      initialize = function(family = 'binomial', alpha = 0.5, nlambda = 100, verbose = FALSE) {
        private$verbosity <- Arguments$getVerbose(verbose)

        private$params <- list(family  = Arguments$getCharacter(family),
                              alpha   = Arguments$getNumeric(alpha, c(0, 1)),
                              nlambda  = Arguments$getNumeric(nlambda, c(1, Inf)),
                              standardize = FALSE,
                              intercept = TRUE
                              )

        self$get_validity
        super$initialize()
      }
    ),
  active =
    list(
      get_validity = function() {
        errors <- c()
        allowed_families <- c("gaussian","binomial","poisson","multinomial","cox","mgaussian")
        if(!(private$params$family %in% allowed_families)) {
          errors <- c(errors, paste('Family',private$params$family,'is not in list of allowed families:', paste(allowed_families, collapse=' ')))
        }
        if(length(errors) > 0) throw(errors)
        TRUE
      }
    ),
  private =
    list(
      params = NULL,
      verbosity = NULL,

      do.predict = function(X_mat, m.fit) {
        if (any(is.na(m.fit$coef))) {
          return(super$do.predict(X_mat, m.fit))
        } else {
          prediction <- predict(m.fit$coef, X_mat, s=0.01, type='response')
          if(length(prediction) != nrow(X_mat)) browser()
          return(prediction)
        }
      },

      do.fit = function (X_mat, Y_vals) {
        estimator <- do.call("glmnet", c(list(x=X_mat, y= Y_vals), private$params))
        return(estimator)
      }
    )
)
