#' General Linear Model machine learning model, based on the XGBoost library
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom xgboost xgb.dump xgb.train xgb.DMatrix getinfo
#' @import xgboost
#' @field model the most recent / best model fitted.
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{This method is used to create object of this class. }
#'
#'   \item{\code{fit(data, X, Y)}}{Method to fit the linear model.}
#'}
ML.XGBoost.glm <- R6Class("ML.XGBoost.glm",
  inherit = ML.XGBoost,
  private =
    list(),
  public =
    list(
        initialize = function(booster = 'gblinear', alpha = 0, lambda = 0) {
          self$rounds <- 200
          self$param <- list(objective = 'binary:logistic',
                                booster = booster,
                                nthread = 8,
                                alpha = alpha,
                                lambda = lambda)

          self$model.name <- 'gblinear'
          super$initialize()
        }
    )
)
