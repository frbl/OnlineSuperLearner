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

        generateMechanism = function(param, family=c("bernoulli", "gaussian")) {
          ## Retrieving arguments
          param <- Arguments$getNumerics(param)
          if ((length(param)-1)%%3 != 0) {
            throw("Length of 'param' minus 1 must be a multiple of 3")
          }
          memory <- (length(param)-1)/3
          family <- match.arg(family)
          link <- switch(family,
                         bernoulli=expit,
                         gaussian=identity)

          if (length(param)==1) {
            mechanism <- function(xx=numeric(0), par=param, lnk=link, verbose=FALSE) {
              ## Retrieving arguments
              if (!length(xx)==0) {
                verbose && enter(verbose, "Argument 'xx' not used when argument 'par' has length 1")
                verbose && exit(verbose)
              }
              par <- Arguments$getNumerics(par)
              lnk <- Arguments$getFunction(lnk)
              verbose <- Arguments$getVerbose(verbose)
              ##
              link(par)
            }
          } else {
            mechanism <- function(xx=NA, par=param, lnk=link, verbose=FALSE) {
              ## Retrieving arguments
              xx <- Arguments$getNumerics(xx)
              par <- Arguments$getNumerics(par)
              lnk <- Arguments$getFunction(lnk)
              verbose <- Arguments$getVerbose(verbose)
              if (length(xx)!=length(param)-1) {
                throw("Length of 'xx' should equal length of 'par' minus one")
              }
              ##
              link(param[1] + param[-1]%*%t(xx))
            }
          }
          attr(mechanism, "memory") <- memory
          attr(mechanism, "family") <- family
          return(mechanism)
        },
        
        simulateBinaryWAY = function(by=1,
                                     qw=generateMechanism(0, family="gaussian"),
                                     ga=generateMechanism(0, family="bernoulli"),
                                     Qy=generateMechanism(0, family="bernoulli"),
                                     intervention=NULL,
                                     verbose=FALSE) {
          ## Retrieving arguments
          by <- Arguments$getInteger(by, c(1, Inf))
          qw <- validateMechanism(qw)
          ga <- validateMechanism(ga)
          Qy <- validateMechanism(Qy)
          if (!is.null(intervention)) {
            intervention <- validateIntervention(intervention)
          }

          families <- c(W=attr(qw, "family"),
                        A=attr(ga, "family"),
                        Y=attr(Qy, "family"))

          memories <- c(W=attr(qw, "memory"),
                        A=attr(ga, "memory"),
                        Y=attr(Qy, "memory"))

          rsource <- list(bernoulli=runif,
                          gaussian=rnorm)

          rgen <- list(bernoulli=function(xx, yy){(xx <= yy)},
                       gaussian=function(xx, yy){xx+yy})
          
          ## verbose
          if (is.null(intervention)) {
            msg <- "Simulating...\n"
          } else {
            msg <- "Simulating under the specified intervention...\n"
          }
          verbose && cat(verbose, msg)

          init <- rep(NA, by)
          ## sources of randomness
          UU <- cbind(W=init, A=init, Y=init)
          for (ii in 1:3) {
            UU[, ii] <- rsource[[families[ii]]](by)
          }
          
          WAY <- rep(init, 3)
          ## -------------
          ## first version ## must be very slow; write faster version with "inline" and "Rcpp"?
          ## -------------
          for (ii in 1:by) {
            past.idx <- retrieveRelevantMemoryWAY("W", ii, memories("W"))
            which.pos <- which(past.idx>0)
            past <- rep(0, memories("W"))
            past[which.pos] <- WAY[past.idx[which.pos]]
            WAY[(ii-1)*3+1] <- rgen[[families[1]]](UU[ii, 1], qw(past))
            ##
            past.idx <- retrieveRelevantMemoryWAY("A", ii, memories("A"))
            which.pos <- which(past.idx>0)
            past <- rep(0, memories("A"))
            past[which.pos] <- WAY[past.idx[which.pos]]
            WAY[(ii-1)*3+2] <- rgen[[families[2]]](UU[ii, 2], ga(past))
            ##
            past.idx <- retrieveRelevantMemoryWAY("Y", ii, memories("Y"))
            which.pos <- which(past.idx>0)
            past <- rep(0, memories("Y"))
            past[which.pos] <- WAY[past.idx[which.pos]]
            WAY[(ii-1)*3+3] <- rgen[[families[3]]](UU[ii, 3], Qy(past))
          }
          WAY <- t(matrix(WAY, nrow=3, dimnames=list(c("W", "A", "Y"), NULL)))
          return(WAY)
        },

        validateMechanism = function(fun) {
          if (!mode(fun)=="function") {
            throw("A mechanism should be of mode 'function', not, ", mode(fun))
          }
          if (is.null(attr(fun, "memory"))) {
            throw("Attribute 'memory' is missing, this is not a valid mechanism")
          }
          if (is.null(attr(fun, "family"))) {
            throw("Attribute 'family' is missing, this is not a valid mechanism")
          }
          return(fun)
        },

        validateIntervention = function(ll) {
          ## ...
          return(ll)
        },

        retrieveRelevantMemoryWAY = function(of, at, mem) {
          ## Retrieving arguments
          of <- Arguments$getCharacter(of)
          at <- Arguments$getInteger(at, c(1, Inf))
          mem <- Arguments$getInteger(mem, c(0, Inf))
          ##
          first <- switch(of,
                          "W"=3*(at-1),
                          "A"=3*(at-1)+1,
                          "Y"=3*(at-1)+2)
          last <- first - mem
          idx <- seq.int(first, last)
          return(idx)
        }
      ),

    private =
      list( 
      )
  )

expit <- plogis
logit <- qlogis
