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
      transformParam = function(param, eps, check = TRUE) {
        lengthOfParam <- length(param)
        eps <- Arguments$getNumeric(eps, c(0, 0.1/lengthOfParam)) ## say
        if (check) {
          param <- Arguments$getNumerics(param, c(eps, 1 - lengthOfParam * eps))
          sumOfParam <- sum(param)
          if (sumOfParam > 1 - eps) {
            throw("'check' is 'TRUE' and 'sum(param) ',", sumOfParam, " is larger than '1 - eps', with 'eps' set to ", eps)
          }
        } else {
          param <- Arguments$getNumerics(param)
          sumOfParam <- sum(param)
        }
        marap <- param
        lower <- eps
        upper <- 1 - lengthOfParam * eps
        for (ii in 1:lengthOfParam) {
          marap[ii] <- log( (param[ii] - lower)/(upper - param[ii]) )
          upper <- upper + eps - param[ii]
        }
        return(marap)
      },

      transformFun = function(fun, eps, check = TRUE, ...) {
        modeOfFun <- mode(fun)
        eps <- Arguments$getNumeric(eps, c(0, 0.1)) ## say
        if (modeOfFun != "function") {
          throw("Mode of 'fun' should be 'function', not ", modeOfFun)
        }
        nuf <- function(marap, FUN=fun, epsilon = eps, doCheck = check, object=self) {
          param <- object$backTransformParam(marap, eps = epsilon, check = doCheck)
          param <- c(param, 1-sum(param))
          FUN(param, ...)
        }
        return(nuf)
      }
    ),
  public =
    list(
      backTransformParam = function(marap, eps, check = TRUE) {
        marap <- Arguments$getNumerics(marap)
        lengthOfMarap <- length(marap)
        eps <- Arguments$getNumeric(eps, c(0, 0.1/lengthOfMarap)) ## say
        param <- marap
        lower <- eps
        upper <- 1 - lengthOfMarap * eps
        for (ii in 1:lengthOfMarap) {
          param[ii] <- (lower + exp(marap[ii]) * upper)/(1 + exp(marap[ii]))
          upper <- upper + eps - param[ii]
        }
        if (check) {
          sumOfParam <- sum(param)
          if (sumOfParam > 1 - eps) {
            throw("'check' is 'TRUE' and 'sum(param)', ", sumOfParam, " is larger than '1 - eps', with 'eps' set to ", eps)
          }
        }
        return(param)
      },

      convexOpt = function(fun, init, epsilon = 1e-10, method = "Nelder-Mead", ...) {
        modeOfFun <- mode(fun)
        if (modeOfFun != "function") {
          throw("Mode of 'fun' should be 'function', not ", modeOfFun)
        }
        init <- Arguments$getNumerics(init, c(0, 1))
        lengthOfInit <- length(init)
        sumOfInit <- sum(init)
        epsilon <- Arguments$getNumeric(epsilon, c(0, 0.1/lengthOfInit)) ## say
        if (sumOfInit != 1) {
          throw("'sum(init)', ", sumOfInit, " does not equal 1")
        }
        ## browser()
        tini <- private$transformParam(init[-length(init)], eps = epsilon, check = TRUE)
        if (any(is.infinite(tini))) {
          msg <- "Trying to start from the border, so doing nothing..."
          warning(paste("\n", msg, "\n"))
          value <- fun(init, ...)
          value <- value[1, drop = TRUE]
          opt <- list(par = init,
                      value = value,
                      counts = c("function"=NA, "gradient"=NA),
                      convergence = NA,
                      message = msg)
        } else {
          nuf <- private$transformFun(fun, eps = epsilon, check = FALSE, ...)
          tpo <- optimr(tini, fn = nuf, method = method)
          opt <- tpo
          par <- self$backTransformParam(tpo$par, eps = epsilon, check = TRUE)
          opt$par <- c(par, 1-sum(par))
        }
        return(opt)
      }
    )
)


n <- 1e4
X <- rnorm(n)
Y <- .2*X+.8*X^2+rnorm(n)
pred <- cbind(X, X^2, X^3, X^4)
data <- list(Y=Y, pred=pred)
fun <- function(alpha, data) {
  -2*t(alpha)%*%(t(data$pred)%*%data$Y) +
    t(alpha) %*% t(data$pred) %*% data$pred %*% alpha
}

cOpt <- convexOptimization$new()
optFirst <- cOpt$convexOpt(fun,
                           init = c(1/4, 1/4, 1/4, 1/4),
                           epsilon = 1e-3,
                           method = "Nelder-Mead",
                           data = data)
optSecond <- cOpt$convexOpt(fun,
                            init = optFirst$par,
                            epsilon = 1e-3,
                            method = "BFGS",
                            data = data)

