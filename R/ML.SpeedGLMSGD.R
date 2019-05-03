#' ML.SpeedGLMSGD
#'
#' Base class for Speedglm in combination with SGD machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom speedglm speedglm
#' @importFrom MASS write.matrix
#' @include ML.Base.R
#' @section Methods:
#' \describe{
#'   \item{\code{initialize()}
#'   }
#' }
#'
#' ## TO DO implement
ML.SpeedGLMSGD <- R6Class("ML.SpeedGLMSGD",
  inherit = ML.Base,
  public =
    list(
      fitfunname = 'speedglmsgd',
      lmclass = 'speedglmsgd',
      initialize = function(alpha = 0.001, loop = 1000, save_model = FALSE) {
        private$alpha <- Arguments$getNumeric(alpha, c(0, Inf))
        private$loop <- Arguments$getNumeric(loop, c(1, Inf))
        private$should_save_model <- Arguments$getLogical(save_model)
        private$no_intercept <- FALSE
        super$initialize()
      }
    ),
  active =
    list(
      get_file_name = function() {
        return(private$file_name)
      },
      get_alpha = function() {
        return(private$alpha)
      },
      get_loop = function() {
        return(private$loop)
      }
    ),
  private =
    list(
      alpha = NULL,
      loop = NULL,
      no_intercept = NULL,
      should_save_model = NULL,
      file_name = file.path('output', 'coef.prn'),

      ## Input: m.fit is a list here, of which $coef is a matrix of coefficients
      ## Output: the predicted results
      do.predict = function(X_mat, m.fit = NULL, ...) {
        if (!is(X_mat, 'data.frame')) { X_mat <- data.frame(X_mat) }

        ## Coef is the actual representation of coefficients now.
        coefs <- private$from_mfit(m.fit)
        result <- private$predict_lr(X_mat, coefs)

        ## Return the predicted outcomes
        return(result)
      },

      ## Input: m.fit is a list here, of which $coef is a matrix of coefficients
      ## Output: a matrix containing the updated coefficients
      do.update = function(X_mat, Y_vals, m.fit = NULL, ...) {
        if (!is(X_mat, 'data.frame')) { X_mat <- data.frame(X_mat) }

        data <- cbind(Y = Y_vals, X_mat)

        coef <- 0
        if(is.null(m.fit)) {
          model <- private$do.fit(X_mat = data, Y_vals = Y_vals)
        } else {
          theta <- private$from_mfit(m.fit)

          coef <- tryCatch({
            private$gradient_descent(X_mat, Y_vals, theta = theta)
          }, error = function(e) {
            print('Gradient decent in GLM SGD failed! Starting browser.')
            browser()
          })
        }

        if(any(is.na(coef))) browser()
        coef
      },

      ## Input from condensier: X_mat: matrix,
      ## Output: A matrix of coefficients
      do.fit = function (X_mat, Y_vals, coef = NULL, ...) {
        ## If we have not yet fit a model, we are using the first n observations as the training set,
        ## Create dataframe
        result = tryCatch({
          if (!is(X_mat, 'data.frame')) { X_mat <- data.frame(X_mat) }

          data <- cbind(Y = Y_vals, X_mat)

          ## Get the X names and remove the intercept, speedglm will add this.
          x_names <- colnames(data[,2:ncol(data)])
          x_names <- x_names[!x_names == 'Intercept']
          formula <- self$create_formula(x_names, intercept = FALSE, force_intercept_removal = private$no_intercept)

          ## We use speedglm to fit the initial coefficients of the model
          coef <- tryCatch({
            speedglm(formula, data = rbind(data) , family = binomial(logit))$coef %>% as.matrix
          }, error = function(e) {
            ## Fallback to GLM if everything else fails
            message('Fitting failed, falling back to glm')
            glm(formula, data = rbind(data) , family = binomial(logit))$coef %>% as.matrix
          })

          coef[is.na(coef)] <- 0
          coef
        }, error = function(e) {
          print('Fitting has failed for GLM sgd. Starting debugger')
          browser()
        })
      },

      predict_lr = function(X_mat, coef){
        X_mat_mtx <- as.matrix(X_mat)

        if (private$no_intercept) {
          X_mat_mtx <- X_mat_mtx[, colnames(X_mat_mtx) != 'Intercept']
        }

        ## Set the NA coefss to 0. This is usualy because some columns have perfect
        ## collinearity
        coef[is.na(coef)] <- 0
        y_pred <- X_mat_mtx %*% coef

        ## Convert the result into a logit function
        return(plogis(y_pred))
      },

      gradient_descent = function(X_mat, Y_vals, theta) {
        ## Make sure we're only dealing with matrices here
        X_mat %<>% as.matrix
        Y_vals %<>% as.matrix

        ## Drop=FALSE is needed in case X_mat is a 1xn matrix, it will stop R from
        ## turing it into a vector
        if (private$no_intercept) {
          X_mat <- X_mat[, colnames(X_mat) != 'Intercept', drop=FALSE]
        }

        ## Set the NA coefss to 0. This is usualy because some columns have perfect
        ## collinearity
        theta[is.na(theta)] <- 0
        m <- nrow(X_mat)

        if (all(theta == 0)) {
          theta <- rep(0, ncol(X_mat)) %>% as.matrix
        }
        
        if (m == 0) { return(theta = theta) }

        for (i in 1:self$get_loop) {
          # finding the predicted values
          h <- plogis(X_mat %*% theta)

          # updating the theta
          theta <- theta - ( (self$get_alpha / m) * ( t(X_mat) %*% (h - Y_vals) ))
        }

        return(theta = theta)
      },

      from_mfit = function(mfit) {
        mfit$coef
      }
    )
)

