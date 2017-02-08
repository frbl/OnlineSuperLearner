#' Simulator
#' @importFrom R6 R6Class
Simulator <-
  R6Class (
    "Simulator",
    public =
      list(
        initialize = function() {
        },
        
        getNext = function(){
          throw('This method needs to be inherited')
        },

        ## generateMechanism = function()

        binaryMechanism = function(param=0) {
          param <- Arguments$getNumerics(param)
          if (length(param)==1) {
            mechanism <- function(par=param) {
              expit(par)
            }
          } else {
            mechanism <- function(xx, par=param) {
              xx <- Arguments$getNumerics(xx)
              par <- Arguments$getNumerics(par)
              if (length(xx)!=length(param)-1) {
                throw("Length of 'xx' should equal length of 'par' minus one")
              }
              expit(param[1] + param[-1]%*%t(xx))
            }
          }
          return(mechanism)
        },
        
        simulateBinaryWAY = function(by=1,
                                     qw=binaryMechanism(),
                                     ga=binaryMechanism(),
                                     Qy=binaryMechanism()) {
          by <- Argument$getInteger(by, c(1, Inf))
          UU <- runif(3*by)
          WW <- rep(NA, by)
          AA <- rep(NA, by)
          YY <- rep(NA, by)
          for (ii in 1:by) {
            WW[ii] <- (UU[(by-1)*ii] <= qw())
            AA[ii] <- (UU[(by-1)*ii+1] <= ga())
            YY[ii] <- (UU[(by-1)*ii+2] <= Qy())
          }
          data <- cbind(W=WW, A=AA, Y=YY)
          return(data)
        }
      )
    private =
      list(
        
      )
  )

expit <- plogis
logit <- qlogis
