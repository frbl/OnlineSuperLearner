#' ML.Local.lm
#' @importFrom R6 R6Class
#' @include ML.Local.R
ML.Local.lm <-
  R6Class (
           "ML.Local.lm",
           inherit = ML.Local,
           public =
             list(
                  nfolds = NULL,
                  alpha = NULL,
                  family = NULL,

                  initialize = function(data, nfolds= 10, alpha = 0.5, family = 'binomial') {
                    super$initialize(data = data)
                    self$nfolds <- nfolds
                    self$alpha <- alpha
                    self$family <- family
                  },

                  fit = function(X, y, oldmodel = NULL){
                    formula <- paste(y, '~', X)
                    if(is.null(oldmodel)){
                      model <- lm(formula, self$data)
                    } else {
                      model <- update(oldmodel, formula, self$data)
                    }
                  }
                  )
           )
