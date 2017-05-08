#' WCC.NMBFGS
#' convex optimisation algorithm using a two step approach. First it uses the Nelder-Mead algorithm to find a rough
#' estimation, which it then updates using the BFGS algorithm.
#' 
#' We use the convention that a transformed variable is has the same variable name but mirrored:
#' param -> marap
#' fun -> nuf
#' etc
#'
#' @docType class
#' @importFrom R6 R6Class
#' @import optimr
WCC.NMBFGS <- R6Class("WCC.NMBFGS",
  inherit = WeightedCombinationComputer,
  private =
    list(
      function_to_optimize = NULL,
      epsilon = NULL,
      data = NULL,

      update_data = function(newdata) {
        private$data <- lapply(1:length(newdata), function(i) rbind(private$data[[i]], newdata[[i]]))
        names(private$data) <- names(newdata)
      },

      # Function to transform the provided params to the log space, which makes sure they can never be negative
      transform_parameters = function(param, epsilon, check = TRUE) {
        lengthOfParam <- length(param)
        epsilon <- Arguments$getNumeric(epsilon, c(0, 0.1 / lengthOfParam))
        if (check) {
          # There are rounding erros, and because of that the get numerics fails. Work around that for now.
          param <- sapply(param, function(x) min(max(x,epsilon), 1 - lengthOfParam * epsilon))

          param <- Arguments$getNumerics(param, c(epsilon, 1 - lengthOfParam * epsilon))
          sumOfParam <- sum(param)
          if (sumOfParam > 1 - epsilon) {
            throw("'check' is 'TRUE' and 'sum(param) ',", sumOfParam, " is larger than '1 - epsilon', with 'epsilon' set to ", epsilon)
          }
        } else {
          param <- Arguments$getNumerics(param)
          sumOfParam <- sum(param)
        }

        marap <- param
        lower <- epsilon
        upper <- 1 - lengthOfParam * epsilon
        for (ii in 1:lengthOfParam) {
          marap[ii] <- log( (param[ii] - lower) / (upper - param[ii]) )
          upper <- upper + epsilon - param[ii]
        }
        return(marap)
      },

      # Function to transform smarap back from the log space to the normal space
      back_transform_parameters = function(marap, epsilon, check = TRUE) {
        marap <- Arguments$getNumerics(marap)
        lengthOfMarap <- length(marap)
        epsilon <- Arguments$getNumeric(epsilon, c(0, 0.1/lengthOfMarap)) ## say
        param <- marap
        lower <- epsilon
        upper <- 1 - lengthOfMarap * epsilon
        for (ii in 1:lengthOfMarap) {
          param[ii]  <- (lower + exp(marap[ii]) * upper)/(1 + exp(marap[ii]))
          upper <- upper + epsilon - param[ii]
        }
        if (check) {
          sumOfParam <- sum(param)

          if (sumOfParam + epsilon > 1 + 1e-10) {
            browser()
            throw("'check' is 'TRUE' and 'sum(param)', ", sumOfParam, " is larger than '1 - epsilon', with 'epsilon' set to ", epsilon)
          }
        }
        return(param)
      },

      transform_function = function(optimizable_function, epsilon, check = TRUE, data,  ...) {
        function_mode <- mode(optimizable_function)
        epsilon <- Arguments$getNumeric(epsilon, c(0, 0.1)) ## say
        if (function_mode != "function") {
          throw("Mode of 'fun' should be 'function', not ", function_mode)
        }
        nuf <- function(marap, fn=optimizable_function, eps = epsilon, do_check = check,
                                         back_transform_param_function=private$back_transform_parameters) {

          param <- back_transform_param_function(marap, epsilon = eps, check = do_check)
          param <- c(param, 1-sum(param))
          fn(param, data= data,...)
        }
        return(nuf)
      },

      perform_optimization = function(weights, epsilon,  method = 'Nelder-Mead', data, ...) {

        sthgiew <- private$transform_parameters(weights[-length(weights)], epsilon = epsilon, check = TRUE)

        # It could be the case that one of the weights was right on the border (0 for example), and this will
        # result in Inf
        if (any(is.infinite(sthgiew))) {
          msg <- "Trying to start from the border, so doing nothing..."
          warning(paste("\n", msg, "\n"))
          value <- private$function_to_optimize(alpha = weights, data = data, ...)
          value <- value[1, drop = TRUE]
          opt <- list(par = weights,
                      value = value,
                      counts = c("function"=NA, "gradient"=NA),
                      convergence = NA,
                      message = msg)
        } else {
          nuf <- private$transform_function(private$function_to_optimize, 
                                                       epsilon = epsilon, check = FALSE, data = data, ...)

          # Note that after optimizing we still need to transform the weights back to the parameter world
          tpo <- optimr(sthgiew, fn = nuf, method = method)
          par <- private$back_transform_parameters(tpo$par, epsilon = epsilon, check = TRUE)
          opt <- tpo
          opt$par <- c(par, 1-sum(par))
        }
        return(opt)
      },

      # Z and Y are always matrices
      compute = function(Z, Y, libraryNames, ...) {
        # We have to store the new data in order to fake the online update (i.e., it gets trained on all 
        # level 1 data
        private$update_data(list(Z=Z, Y=Y))

        Z = private$data$Z
        Y = private$data$Y

        data <- list(Qa = t(Z) %*% Y, Qb=t(Z) %*% Z)
        # dimensions: Qa = x * 1, Qb = x * x
        optFirst <- private$perform_optimization(weights = private$weights,
                                  epsilon = private$epsilon,
                                  method = "Nelder-Mead",
                                  data = data, ...)

        private$weights <- optFirst$par
        #optSecond <- private$perform_optimization(weights = optFirst$par,
                                    #epsilon = private$epsilon,
                                    #method = "BFGS",
                                    #data = data, ...)

        #private$weights <- optSecond$par
        names(private$weights) <- libraryNames
        invisible(self)
      }
    ),
  public =
    list(
      initialize = function(weights.initial, function_to_optimize = NULL, epsilon = 1e-3) {
        super$initialize(weights.initial)
        if (is.null(function_to_optimize)) {
          function_to_optimize <- function(alpha, data) {
            -2 * t(alpha) %*% data$Qa + t(alpha) %*% data$Qb %*% alpha
          }
        }

        function_mode <- mode(function_to_optimize)
        if (function_mode != "function") {
          throw("Mode of 'fun' should be 'function', not ", function_mode)
        }
        private$function_to_optimize <- function_to_optimize 
        private$epsilon <- Arguments$getNumeric(epsilon, c(0, 0.1/length(private$weights))) ## say
      }
    )
)

