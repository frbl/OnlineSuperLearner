#' ML.Speedglm
#'
#' @docType class
#' @export
ML.Speedglm <- R6Class("ML.Speedglm",
  inherit = ML.Base,
  public =
    list(
        initialize = function() { }
        ),
  active =
    list(
        ),
  private =
    list(
        do.fit = function(X_mat, Y_vals) {
          # , maxit=1000
            suppressWarnings(
              m.fit <- speedglm::speedglm.wfit(X = X_mat, y = Y_vals, family = binomial(), 
                                               method='Cholesky')
            )
          m.fit$coef
        },

        do.predict = function(X_mat, m.fit) {
          if (any(is.na(m.fit$coef))) {
            return(super$do.predict(X_mat, m.fit))
          } else {
            return(expit(X_mat %*% m.fit$coef))
          }
        }
    )
)

