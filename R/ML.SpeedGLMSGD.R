#' ML.SpeedGLMSGD
#'
#' Base class for Speedglm in combination with SGD machine learning model.
#'
#' @docType class
#' @importFrom R6 R6Class
#' @importFrom Speedglm speedglm
#' @importFrom MASS write.matrix
#' @include ML.Base.R
#' @section Methods:
#' \describe{
#'   \item{\code{initialize(weights = NULL, 
#'offset = NULL, 
#'sparse = NULL, 
#'set.default = list(), 
#'method = c("eigen", "Cholesky", "qr"), 
#'model = FALSE, 
#' y = FALSE, 
#'fitted = FALSE, 
#'subset = NULL
#'family=binomial(link=logit)
#'

#'
#'     @param family string the statistic algorithm used to
#'      optimize.
#'
#'   }
#'
#'   \item{\code{get_nthread}}{
#'     Active method. Function that returns the number of threads the speedlm
#'     algorithm runs on.
#'   }
#'
#'   \item{\code{get_validity}}{
#'     Active method. Function that shows wheter the current configuration of
#'     the initialisation is valid. The function returns \code{TRUE} if everything is
#'     specified correctly. It will throw an error (with the error messages)
#'     when something is misspecified. This function is automatically called
#'     after initialization.
#'   }
#' }
#' 
#' ## TO DO implement 
ML.SpeedGLMSGD <- R6Class("ML.SpeedGLMSGD",
  inherit = ML.Base,
  public =
    list(
      fitfunname='speedglm',
      lmclass='speedglm',
      initialize = function() {
        self$get_validity
        super$initialize()
        
      }
    ),  active =
    list(get_file_name = function() {
         return(private$file_name)
    }
    
      ),
  private =
      list(
           alpha = 0.001,
           loop = 1000,
           file_name = file.path('output', 'coef.prn'),
      
      do.predict = function(X_mat,Y_vals,save_model=FALSE,m.fit=NULL) {
        X_mat <- data.frame(X_mat)
        Y_vals <-data.frame(Y_vals)
        data<-cbind.data.frame(Y_vals,X_mat)
        
        #rename colums to standardize depending on the number of columns
        for (i in (1:(ncol(data)))){
          if (i == 1){
            colnames(data)[i] <- "Y"
          }
          if (i>1){
            colnames(data)[i]<-paste("x",i-1, sep = "")
          }
        }
        
        
        if(!is.null(m.fit)){
          result<-private$predict_lr(X_mat,m.fit)
          
        }
        else {
            try(m.fit<-private$read_model(),silent = TRUE)
            if (!is.null(m.fit)){
             
            result<-private$predict_lr(X_mat,m.fit)
        }
          else {
            ## fit the model
            m.fit<- coef(private$do.fit(X_mat = X_mat, Y_vals = Y_vals, save_model = FALSE ))
            result<-private$predict_lr(X_mat,m.fit)
          }
        }
        
        if (save_model){
           private$save_model(model=coef(m.fit))
        }
        
        return(result)
      },

      do.update = function(X_mat, Y_vals,save_model=FALSE, m.fit=NULL, ...) {
        # speedlm uses the coefficients of the last model to update with more data.
        X_mat <- data.frame(X_mat)
        Y_vals <- data.frame(Y_vals)
        data <- cbind.data.frame(Y_vals,X_mat)
        
        
        #rename colums to standardize depending on the number of columns
        for (i in (1:(ncol(data)))){
          if (i == 1){
            colnames(data)[i] <- "Y"
          }
          if (i>1){
            colnames(data)[i]<-paste("x",i-1, sep = "")
          }
        }
       
       
        if(!is.null(m.fit)){
          result<-private$update_coef(X_mat,Y_vals,coef=m.fit,alpha=private$alpha,loop=private$loop)
          }
          else {
            try(m.fit<-private$read_model(),silent = TRUE)
            if(!is.null(m.fit)) {
            ## load model
            result<-private$update_coef(X_mat,Y_vals,coef=m.fit,alpha=private$alpha,loop=private$loop)
            }
           else {
            ## fit the model
            model <- private$do.fit(X_mat = data, Y_vals = Y_vals, save_model = FALSE )
            result <- coef(model)
            }
          }
         
        if (save_model){
          private$save_model(model = result)
           
        }
              
        return(result)
        
      },

      do.fit = function (X_mat, Y_vals, save_model = FALSE, coef = NULL) {
        # If we have not yet fit a model, we are using the first n observations as the training set,
        # Create dataframe
         X_mat <- data.frame(X_mat)
         Y_vals <- data.frame(Y_vals)
         data <- cbind.data.frame(Y_vals,X_mat)
        
         #rename colums to standardize depending on the number of columns
        for (i in (1:(ncol(data)))){
          if (i == 1){
            colnames(data)[i] <- "Y"
          }
          if (i>1){
            colnames(data)[i]<-paste("x",i-1, sep = "")
          }
        }
        
        
        x_names=colnames(data[2:ncol(data)])
        
        formula <- self$create_formula(x_names)
      
        model<-speedglm(formula,data = data, family = binomial(logit))
      
        if (save_model){
        private$save_model(model = coef(model))
        }
        
        return(model)
      },
      
      save_model = function(model) {
        write.matrix(model, self$get_file_name, sep = "\t")
      },
      
      read_model = function() {
        as.matrix(read.table(self$get_file_name, as.is = TRUE))
      },
      
      predict_lr = function(X_mat,coef){
        #prediction of the Y
        X_mat_mtx=as.matrix(X_mat)
        intercept_row=cbind(intercept=1,X_mat_mtx)
        coef_mtx=as.matrix(coef)
        y_pred=0
        
        y_pred <- intercept_row%*% coef_mtx
        #convert the result into a logit function
        yhat_return=plogis(y_pred)  
       
        return(yhat_return)
      },
      
      gradDescent = function(X_mat, Y_vals, theta, alpha, loop)
      {
        m = nrow(X_mat)
        theta<-as.matrix(theta)
        X_mat<-as.matrix(X_mat)
        Y_vals<-as.matrix(Y_vals)
        
        while ( loop > 0)
        { # finding the predicted values
          
          h = plogis(X_mat %*% theta)
          
          # updating the theta
          new_theta = theta - ( (alpha / m) * ( t(X_mat) %*% (h - Y_vals) ))
          theta = new_theta
          
          # decrease the loop by 1
          loop = loop - 1
        }
        return(theta = theta)   
      },
      
      update_coef = function (X_mat,Y_vals,coef,alpha=private$alpha,loop=private$loop){
        #update the coeficients
        coef_m=as.matrix(coef)
        
        x_i=cbind(intercept=1,X_mat)
        
        results=private$gradDescent(X_mat = x_i,Y_vals = Y_vals,theta = coef_m,alpha,loop)
        
        return(results)
      }  
      
      
    )
)

