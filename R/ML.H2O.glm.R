#' ML.H2O.glm
#'
#' Wrapper for an H2O glm estimator. From their discription:
#' Generalized Linear Models (GLM) estimate regression models for outcomes
#' following exponential distributions. In addition to the Gaussian (i.e.
#' normal) distribution, these include Poisson, binomial, and gamma
#' distributions. Each serves a different purpose, and depending on
#' distribution and link function choice, can be used either for prediction or
#' classification.
#'
#' @importFrom R6 R6Class
#' @include ML.H2O.R
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(nfolds = 1, alpha = 0.5, family = "gaussian") }}{ 
#'     Creates a new h2o glm model
#'
#'     @param nfold integer (default = 1) specify the number of folds for
#'      cross-validation.
#'     @param alpha double (default = 0.5) the alpha parameter controls the distribution
#'      between the l1 (LASSO) and l2 (ridge regression) penalties. A value of
#'      1.0 for alpha represents LASSO, and an alpha value of 0.0 produces
#'      ridge reguression.
#'
#'     @param family string (default = "binomial") the family to use for the
#'      estimator. Generally this should be binomial (since we are running
#'      various logistic regressions). However, if you have a good reason to
#'      use a different specification, change it here.
#'   } 
#' }  

ML.H2O.glm <- R6Class("ML.H2O.glm",
  inherit = ML.H2O,
  public =
    list(
        initialize = function(nfolds = 1, alpha = 0.5, family = 'binomial', verbose = FALSE) {
          super$initialize()
          private$nfolds <- nfolds
          private$alpha <- alpha
          private$family <- family
          private$verbose <- verbose
        }
    ),
  private =
    list(
        nfolds = NULL,
        alpha = NULL,
        family = NULL,
        verbose = NULL,
        do.fit = function(train, Y, A, W){
          X <- c(A, W)
          self$model <- h2o.glm(x = X, y = Y,
            training_frame = train,
            family = private$family,
            nfolds = private$nfolds,
            alpha = private$alpha,
            checkpoint = private$getCheckpoint()
          )

        }
    )
)
