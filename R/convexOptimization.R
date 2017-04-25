#' convexOptimization
#' tools for convex optimization
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#' \describe{
#' }
#' @export
convexOptimization <- R6Class("convexOptimization",
  private =
    list(
      transformParam = function(param, check = TRUE) {
        if (check) {
          param <- Arguments$getNumerics(param, c(0, 1))
          sumOfParam <- sum(param)
          if (sumOfParam > 1) {
            throw("'check' is 'TRUE' and 'sum(param) ',", sumOfParam, " is larger than 1")
          }
        } else {
          param <- Arguments$getNumerics(param)
          sumOfParam <- sum(param)
        }
        param <- c(param, 1-sumOfParam)
        marap <- logit(param)
        return(marap)
      },

      transformFun = function(fun, check = TRUE, ...) {
        modeOfFun <- mode(fun)
        if (modeOfFun != "function") {
          throw("Mode of 'fun' should be 'function', not ", modeOfFun)
        }
        nuf <- function(marap, FUN=fun, doCheck = check, object=self) {
          ## browser()
          param <- object$backTransformParam(marap, check = doCheck)
          param <- c(param, 1-sum(param))
          FUN(param, ...)
        }
        return(nuf)
      }
    ),
  public =
    list(
      backTransformParam = function(marap, check = TRUE) {
        marap <- Arguments$getNumerics(marap)
        param <- expit(marap)
        if (check) {
          sumOfParam <- sum(param)
          if (sumOfParam != 1) {
            throw("'check' is 'TRUE' and 'sum(param)', ", sumOfParam, " does not equal 1")
          }
        }
        param <- param[-length(param)]
        return(param)
      },

      convexOpt = function(fun, init, method = "Nelder-Mead", ...) {
        modeOfFun <- mode(fun)
        if (modeOfFun != "function") {
          throw("Mode of 'fun' should be 'function', not ", modeOfFun)
        }
        init <- Arguments$getNumerics(init, c(0, 1))
        sumOfInit <- sum(init)
        if (sumOfInit != 1) {
          throw("'sum(init)', ", sumOfInit, " does not equal 1")
        }
        tini <- private$transformParam(init[-length(init)], check = TRUE)
        nuf <- private$transformFun(fun, check = FALSE, ...)
        tpo <- optimr(tini, fn = nuf, method = method)
        opt <- tpo
        par <- self$backTransformParam(tpo$par, check = TRUE)
        opt$par <- c(par, 1-sum(par))
        return(opt)
      }
    )
)

expit <- plogis
logit <- qlogis     

n <- 100
X <- rnorm(n)
Y <- .3*X+.7*X^2+rnorm(n)
pred <- cbind(X, X^2, X^3, X^4)
data <- list(Y=Y, pred=pred)
fun <- function(alpha, data) {
  -2*t(alpha)%*%(t(data$pred)%*%data$Y) +
    t(alpha) %*% t(data$pred) %*% data$pred %*% alpha
}

cOpt <- convexOptimization$new()
optFirst <- cOpt$convexOpt(fun, init = c(1/4, 1/4, 1/4, 1/4), data = data, method = "Nelder-Mead")
optSecond <- cOpt$convexOpt(fun, init = optFirst$par, data = data, method = "BFGS")
