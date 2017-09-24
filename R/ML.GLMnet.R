#' ML.GLMnet
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom glmnet glmnet
#' @include ML.Base.R
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
