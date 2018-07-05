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
#'   \item{\code{initialize()
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

      do.predict = function(X_mat, m.fit = NULL) {
        #browser()
        X_mat <- data.frame(X_mat)
        #data <- cbind.data.frame(Y_vals, X_mat)

        #rename colums to standardize depending on the number of columns
        #data <- private$rename_columns(data)

        if(is.null(m.fit)){
          m.fit <- private$read_model()
        }

        #if (is.null(m.fit)){
          ### fit the model
          #m.fit <- coef(private$do.fit(X_mat = X_mat, Y_vals = Y_vals))
        #}

        result <- private$predict_lr(X_mat, m.fit$coef)
        private$conditionally_save_model(model = m.fit$coef)

        return(result)
      },

      do.update = function(X_mat, Y_vals, m.fit = NULL, ...) {
        # speedlm uses the coefficients of the last model to update with more data.
        X_mat <- data.frame(X_mat)
        Y_vals <- data.frame(Y_vals)
        data <- cbind.data.frame(Y_vals, X_mat)


        #rename colums to standardize depending on the number of columns
        #data <- private$rename_columns(data)

        if(is.null(m.fit)){
          m.fit <- private$read_model()
        }

        if(is.null(m.fit)) {
          model <- private$do.fit(X_mat = data, Y_vals = Y_vals)
          result <- coef(model)
        } else {
          result <- private$gradient_descent(X_mat, Y_vals, coef = m.fit)
        }

        private$conditionally_save_model(model = result)

        return(result)

      },

      do.fit = function (X_mat, Y_vals, coef = NULL) {
        # If we have not yet fit a model, we are using the first n observations as the training set,
        # Create dataframe
        X_mat <- data.frame(X_mat)
        Y_vals <- data.frame(Y_vals)
        data <- cbind.data.frame(Y_vals, X_mat)

        #rename colums to standardize depending on the number of columns
        #data <- private$rename_columns(data)
        x_names <- colnames(data[2:ncol(data)])
        formula <- self$create_formula(x_names)

        # We use speedglm to fit the initial coefficients of the model
        model <- speedglm(formula, data = data, family = binomial(logit))

        private$conditionally_save_model(model = coef(model))

        return(model)
      },

      conditionally_save_model = function(model) {
        result = NULL
        if (private$should_save_model){
          result = write.matrix(model, self$get_file_name, sep = "\t")
        }
        result
      },

      read_model = function() {
        if (!file.exists(self$get_file_name) || !private$should_save_model ) {
          return(NULL)
        }
        self$get_file_name %>%
          read.table(., as.is = TRUE) %>%
          as.matrix
      },

      predict_lr = function(X_mat, model){
        #prediction of the Y
        X_mat_mtx <- as.matrix(X_mat)
        #intercept_row <- cbind(intercept = 1, X_mat_mtx)
        model <- Arguments$getInstanceOf(model, 'speedlm')
        coef <- model$coef
        coef_mtx <- as.matrix(coef)
        browser()
        y_pred <- X_mat_mtx %*% coef_mtx

        #convert the result into a logit function
        return(plogis(y_pred))
      },

      gradient_descent = function(X_mat, Y_vals, coef) {
        #update the coeficients
        theta <- as.matrix(coef)
        X_mat <- cbind(intercept = 1, X_mat)

        m = nrow(X_mat)
        X_mat %<>% as.matrix        
        Y_vals %<>% as.matrix        

        for (i in 1:self$get_loop) { 
          # finding the predicted values
          h = plogis(X_mat %*% theta)

          # updating the theta
          theta = theta - ( (self$get_alpha / m) * ( t(X_mat) %*% (h - Y_vals) ))
        }
        return(theta = theta)
      },

      rename_columns = function(data) {
        for (i in (1:(ncol(data)))){
          if (i > 1){
            colnames(data)[i] <- paste("x", i - 1, sep = "")
            next
          } 
          colnames(data)[i] <- "Y"
        }
        data
      }
    )
)

