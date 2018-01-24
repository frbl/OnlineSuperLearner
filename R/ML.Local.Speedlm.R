#' ML.Local.Speedlm
#'
#' Class to create speedlm linear models. Uses the speed glm package and can be
#' updated online in the case of the gaussian family. Note that the actual
#' online function is not yet implemented correctly. This requires us to use
#' the speedlm function, instead of the speed glm, wich does not support a
#' family for now.
#'
#' @docType class
#' @importFrom speedglm speedlm updateWithMoreData
#' @importFrom R6 R6Class
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}}{
#'     Initializes a new speedlm estimator. 
#'   }
#' }
#' @export
ML.Local.Speedlm <- R6Class("ML.Local.Speedlm",
  inherit = ML.Base,
  public =
    list(
      fitfunname='speedglm-local',
      lmclass='speedglm',
      initialize = function() { }
    ),
  active =
    list(
        ),
  private =
    list(
        do.fit = function(X_mat, Y_vals) {
          # , maxit=1000
          #suppressWarnings(
          hide_warning_rank_deficient_matrix({
            m.fit <- speedglm::speedglm.wfit(X = X_mat, y = Y_vals, family = binomial(), 
                                              method='Cholesky')
          })
          m.fit$coef
        },

        do.update = function(m.fit, X_mat, Y_vals) {
          ## !!! This function only workds with a speed LM funcion, not the speed GLM function!
          updateWithMoreData(m.fit, X = X_mat, y = Y_vals)
        },

        do.predict = function(X_mat, m.fit) {
          if (any(is.na(m.fit$coef))) {
            result <- super$do.predict(X_mat, m.fit)
          } else {
            result <- expit(X_mat %*% m.fit$coef)
          }
          if(any(is.na(result))) browser()
          return(result)
        }
    )
)

