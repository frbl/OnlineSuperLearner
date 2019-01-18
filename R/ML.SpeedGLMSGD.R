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
      should_save_model = NULL,
      file_name = file.path('output', 'coef.prn'),

      do.predict = function(X_mat, m.fit = NULL, ...) {
        ## m.fit is a list here.
        ## m.fit$coef$coef gives us the coefficients
        if (!is(X_mat, 'data.frame')) { X_mat <- data.frame(X_mat) }

        ## Coef is the actual representation of coefficients now.
        coefs <- private$from_mfit(m.fit)
        result <- private$predict_lr(X_mat, coefs)

        ## Return the predicted outcomes
        return(result)
      },

      do.update = function(X_mat, Y_vals, m.fit = NULL, ...) {
        if (!is(X_mat, 'data.frame')) { X_mat <- data.frame(X_mat) }

        data <- cbind(Y = Y_vals, X_mat)

        if(is.null(m.fit)) {
          model <- private$do.fit(X_mat = data, Y_vals = Y_vals)
          coef <- coef(model) %>% as.matrix
        } else {
          theta <- private$from_mfit(m.fit)
          coef <- private$gradient_descent(X_mat, Y_vals, theta = theta)
        }

        if(any(is.na(coef))) browser()
        private$to_mfit(coef)
      },

      ## Input from condensier: X_mat: matrix, 
      do.fit = function (X_mat, Y_vals, coef = NULL, ...) {
        ## If we have not yet fit a model, we are using the first n observations as the training set,
        ## Create dataframe
        if (!is(X_mat, 'data.frame')) { X_mat <- data.frame(X_mat) }

        data <- cbind(Y = Y_vals, X_mat)
        
        ## Get the X names and remove the intercept, speedglm will add this.
        x_names <- colnames(data[,2:ncol(data)])
        x_names <- x_names[!x_names == 'Intercept'] 
        formula <- self$create_formula(x_names, intercept = FALSE)

        ## We use speedglm to fit the initial coefficients of the model
        speedglm(formula, data = data, family = binomial(logit))
      },

      predict_lr = function(X_mat, coef){
        X_mat_mtx <- as.matrix(X_mat)
        coef_mtx <- as.matrix(coef)

        ## Set the NA coefss to 0. This is usualy because some columns have perfect 
        ## collinearity
        coef_mtx[is.na(coef_mtx)] <- 0
        y_pred <- X_mat_mtx %*% coef_mtx

        ## Convert the result into a logit function
        return(plogis(y_pred))
      },

      gradient_descent = function(X_mat, Y_vals, theta) {
        ## Make sure we're only dealing with matrices here
        X_mat %<>% as.matrix        
        Y_vals %<>% as.matrix        
        theta %<>% as.matrix

        ## Set the NA coefss to 0. This is usualy because some columns have perfect 
        ## collinearity
        theta[is.na(theta)] <- 0
        m <- nrow(X_mat)

        for (i in 1:self$get_loop) { 
          # finding the predicted values
          h = plogis(X_mat %*% theta)

          # updating the theta
          theta = theta - ( (self$get_alpha / m) * ( t(X_mat) %*% (h - Y_vals) ))
        }
        return(theta = theta)
      },

      to_mfit = function(coef) {
        list(coef = coef)
      },

      from_mfit = function(mfit) {
        mfit$coef$coef
      }
    )
)

