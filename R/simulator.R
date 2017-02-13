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

        validateMechanismScenarioTwo = function(ll) {
          if (!is.list(ll)) {
            throw("The argument should be a list, not ", mode(ll))
          }
          if (!length(ll)==3) {
            throw("The argument should be a list of length three, not ", length(ll))
          }
          stochMech <- ll[[1]]
          if (!mode(stochMech)=="function") {
            throw("The first element of the argument should be a function, not", mode(stochMech))
          }
          param <- Arguments$getNumerics(param)
          rgen <- ll[[3]]
          if (!mode(rgen)=="function") {
            throw("The first element of the argument should be a function, not", mode(rgen))
          }
          return(ll)
        },
        
        simulateWAYScenarioTwo = function(by=1,
                                          qw=list(stochMech=rnorm,
                                                  param=rep(1, 2),
                                                  rgen=identity),
                                          ga=list(stochMech={function(ww){rbinom(length(ww), 1, expit(ww))}},
                                                  param=rep(1, 2),
                                                  rgen={function(xx, delta=0.05){rbinom(length(xx), 1, delta+(1-2*delta)*expit(xx))}}),
                                          Qy=list(stochMech={function(aa, ww){aa*ww+(1-aa)*(-ww)}},
                                                  param=rep(1, 2),
                                                  rgen=identity),
                                          intervention=NULL,
                                          verbose=FALSE) {
          ##
          ## directed acyclic graph approach to simulation
          ##
          
          ## Retrieving arguments
          by <- Arguments$getInteger(by, c(1, Inf))
          qw <- self$validateMechanismScenarioTwo(qw)
          ga <- self$validateMechanismScenarioTwo(ga)
          Qy <- self$validateMechanismScenarioTwo(Qy)
          if (!is.null(intervention)) {
            intervention <- self$validateIntervention(intervention)
          }
          verbose <- Arguments$getVerbose(verbose)
          
          memories <- c(W=length(qw$param)-1,
                        A=length(ga$param)-1,
                        Y=length(Qy$param)-1)

          ## verbose
          if (is.null(intervention)) {
            msg <- "Simulating...\n"
          } else {
            msg <- "Simulating under the specified intervention...\n"
          }
          verbose && cat(verbose, msg)

          by <- by+max(memories)
          ## backbone
          UW <- qw$stochMech(by)
          UA <- ga$stoch(UW)
          UY <- Qy$stochMech(UA, UW)
          ## W
          idx <- (max(memories)+1):length(UW)
          MATW <- UW[idx]
          if (memories["W"]>=1) {
            for (ii in 1:memories["W"]) {
              idx <- idx-1
              MATW <- cbind(MATW, UW[idx])
            }
          }
          W <- qw$rgen(MATW %*% qw$param)
          rm(MATW)
          ## A
          idx <- (max(memories)+1):length(UA)
          MATA <- UA[idx]
          if (memories["A"]>=1) {
            for (ii in 1:memories["A"]) {
              idx <- idx-1
              MATA <- cbind(MATA, UA[idx])
            }
          }
          A <- ga$rgen(MATA %*% ga$param)          
          rm(MATA)
          ## Y
          idx <- (max(memories)+1):length(UY)
          MATY <- UY[idx]
          if (memories["Y"]>=1) {
            for (ii in 1:memories["Y"]) {
              idx <- idx-1
              MATY <- cbind(MATY, UY[idx])
            }
          }
          Y <- Qy$rgen(MATY %*% Qy$param)          
          rm(MATY)
          ##
          WAY <- cbind(W=W, A=A, Y=Y)
          return(WAY)
        },

        generateMechanismScenarioTwo = function(stochMech=rnorm, param=rep(1, 2), rgen=identity) {
          ## Retrieving arguments
          stochMech <- Arguments$getFunction(stochMech)
          param <- Arguments$getNumerics(param)
          rgen <- Arguments$getFunction(rgen)
          memory <- length(param)-1

          if (length(param)==1) {
            mechanism <- function(xx=numeric(0), par=param, lnk=link, verbose=FALSE) {
              ## Retrieving arguments
              verbose <- Arguments$getVerbose(verbose)
              if (!length(xx)==0) {
                verbose && enter(verbose, "Argument 'xx' not used when argument 'par' has length 1")
                verbose && exit(verbose)
              }
              par <- Arguments$getNumerics(par)
              if (mode(lnk)!="function") {
                throw("Argument 'lnk' must be a link function, not ", mode(lnk))
              }
              ##
              link(par)
            }
          } else {
            mechanism <- function(xx=NA, par=param, lnk=link, verbose=FALSE) {
              ## Retrieving arguments
              xx <- Arguments$getNumerics(xx)
              par <- Arguments$getNumerics(par)
              if (mode(lnk)!="function") {
                throw("Argument 'lnk' must be a link function, not ", mode(lnk))
              }
              verbose <- Arguments$getVerbose(verbose)
              if (length(xx)!=length(param)-1) {
                throw("Length of 'xx' should equal length of 'par' minus one")
              }
              ##
              if (FALSE) {
                link(param[1] + param[-1]%*%xx)
              } else {
                link(param[1] + sum(param[-1]*xx))
              }
            }
          }
          attr(mechanism, "memory") <- memory
          attr(mechanism, "family") <- family
          return(mechanism)
        },

        
        simulateWAYScenarioOne = function(by=1,
                                          qw=self$generateMechanismScenarioOne(0, family="gaussian"),
                                          ga=self$generateMechanismScenarioOne(0, family="bernoulli"),
                                          Qy=self$generateMechanismScenarioOne(0, family="bernoulli"),
                                          intervention=NULL,
                                          verbose=FALSE,
                                          version="slow") {
          ## Retrieving arguments
          by <- Arguments$getInteger(by, c(1, Inf))
          qw <- self$validateMechanismScenarioOne(qw)
          ga <- self$validateMechanismScenarioOne(ga)
          Qy <- self$validateMechanismScenarioOne(Qy)
          if (!is.null(intervention)) {
            intervention <- self$validateIntervention(intervention)
          }
          verbose <- Arguments$getVerbose(verbose)
          
          families <- c(W=attr(qw, "family"),
                        A=attr(ga, "family"),
                        Y=attr(Qy, "family"))

          memories <- c(W=attr(qw, "memory"),
                        A=attr(ga, "memory"),
                        Y=attr(Qy, "memory"))

          rsource <- list(bernoulli=runif,
                          gaussian=rnorm)

          rgen <- list(bernoulli=function(xx, yy){as.integer(xx <= yy)},
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
          if (version=="slow") {
            ## -------------
            ## first version ## must be very slow
            ## -------------
            for (ii in 1:by) {
              past.idx <- self$retrieveRelevantPastWAYScenarioOne("W", ii, memories["W"])
              which.pos <- which(past.idx>0)
              past <- rep(0, memories["W"])
              past[which.pos] <- WAY[past.idx[which.pos]]
              WAY[(ii-1)*3+1] <- rgen[[families[1]]](UU[ii, 1], qw(past))
              ##
              past.idx <- self$retrieveRelevantPastWAYScenarioOne("A", ii, memories["A"])
              which.pos <- which(past.idx>0)
              past <- rep(0, memories["A"])
              past[which.pos] <- WAY[past.idx[which.pos]]
              WAY[(ii-1)*3+2] <- rgen[[families[2]]](UU[ii, 2], ga(past))
              ##
              past.idx <- self$retrieveRelevantPastWAYScenarioOne("Y", ii, memories["Y"])
              which.pos <- which(past.idx>0)
              past <- rep(0, memories["Y"])
              past[which.pos] <- WAY[past.idx[which.pos]]
              WAY[(ii-1)*3+3] <- rgen[[families[3]]](UU[ii, 3], Qy(past))
            }
          } else if (version=="faster") {
            ## --------------
            ## second version ## significantly faster than previous one?
            ## --------------

#### check how to require libraries 'inline' and 'Rcpp'...

            throw("'Rcpp' version not implemented yet...")
            
            ## #############
            ## A TEMPLATE...
            ## #############
            ##
            ## generateWAY <- cxxfunction(signature(x="numeric", y="numeric", wt="numeric", param="numeric"),
            ##                            body="
            ##              Rcpp::NumericVector xx(x);
            ##              Rcpp::NumericVector yy(y);
            ##              /*Rcpp::NumericVector aa(alpha);*/
            ##              Rcpp::NumericVector wwtt(wt);
            ##              Rcpp::NumericVector bb(param);
            ##              int n=xx.size();
            ##              Rcpp::NumericVector out(1);
            ##              Rcpp::NumericVector Nb(1);
            
            ##              Nb[0]=0;
            ##              out[0]=0;
            ##              for(int i=0; i < n; i++){
            ##                Nb[0] +=wwtt[i];
            ##                for(int j=0; j<n; j++){
            ##                  out[0] = out[0] + (1/(1+exp(bb[0]*(xx[i]-xx[j])*(yy[i]-yy[j]))))*wwtt[i]*wwtt[j];
            ##                }
            ##               }
            ##              out[0] = out[0]/(Nb[0]*Nb[0]);
            
            
            ##              return out;",
            ##              plugin="Rcpp")            
          }
          WAY <- t(matrix(WAY, nrow=3, dimnames=list(c("W", "A", "Y"), NULL)))
          return(WAY)
        },

        generateMechanismScenarioOne = function(param, family=c("bernoulli", "gaussian")) {
          ## Retrieving arguments
          param <- Arguments$getNumerics(param)
          memory <- length(param)-1
          family <- match.arg(family)
          link <- switch(family,
                         bernoulli=expit,
                         gaussian=identity)

          if (length(param)==1) {
            mechanism <- function(xx=numeric(0), par=param, lnk=link, verbose=FALSE) {
              ## Retrieving arguments
              verbose <- Arguments$getVerbose(verbose)
              if (!length(xx)==0) {
                verbose && enter(verbose, "Argument 'xx' not used when argument 'par' has length 1")
                verbose && exit(verbose)
              }
              par <- Arguments$getNumerics(par)
              if (mode(lnk)!="function") {
                throw("Argument 'lnk' must be a link function, not ", mode(lnk))
              }
              ##
              link(par)
            }
          } else {
            mechanism <- function(xx=NA, par=param, lnk=link, verbose=FALSE) {
              ## Retrieving arguments
              xx <- Arguments$getNumerics(xx)
              par <- Arguments$getNumerics(par)
              if (mode(lnk)!="function") {
                throw("Argument 'lnk' must be a link function, not ", mode(lnk))
              }
              verbose <- Arguments$getVerbose(verbose)
              if (length(xx)!=length(param)-1) {
                throw("Length of 'xx' should equal length of 'par' minus one")
              }
              ##
              if (FALSE) {
                link(param[1] + param[-1]%*%xx)
              } else {
                link(param[1] + sum(param[-1]*xx))
              }
            }
          }
          attr(mechanism, "memory") <- memory
          attr(mechanism, "family") <- family
          return(mechanism)
        },
        
        validateMechanismScenarioOne = function(fun) {
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

        retrieveRelevantPastWAYScenarioOne = function(of, at, mem) {
          ## Retrieving arguments
          of <- Arguments$getCharacter(of)
          at <- Arguments$getInteger(at, c(1, Inf))
          mem <- Arguments$getInteger(mem, c(0, Inf))
          ##
          if (mem==0) {
            idx <- integer(0)
          } else {
            from <- switch(of,
                           "W"=3*(at-1),
                           "A"=3*(at-1)+1,
                           "Y"=3*(at-1)+2)
            to <- from - mem + 1
            idx <- seq.int(from, to)
          }
          return(idx)
        }
      ),

    private =
      list( 
      )
  )

expit <- plogis
logit <- qlogis
