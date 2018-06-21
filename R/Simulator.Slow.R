#' Simulator.Slow
#'
#' @docType class
#' @importFrom R6 R6Class
#' @include InterventionParser.R
Simulator.Slow <- R6Class("Simulator.Slow",
  public =
    list(
        initialize = function(qw=self$generateMechanism(0, family="gaussian"),
                              ga=self$generateMechanism(0, family="bernoulli"),
                              Qy=self$generateMechanism(0, family="bernoulli")) {

          private$rgen <- list(bernoulli=function(xx, yy){as.integer(xx <= yy)},
                        gaussian=function(xx, yy){xx+yy})

          private$qw %<>% private$validateMechanism
          private$ga %<>% private$validateMechanism
          private$Qy %<>% private$validateMechanism

          private$families <- c(W=attr(private$qw, "family"),
                                A=attr(private$ga, "family"),
                                Y=attr(private$Qy, "family"))

          private$memories <- c(W=attr(private$qw, "memory"),
                                A=attr(private$ga, "memory"),
                                Y=attr(private$Qy, "memory"))
        },

        simulateWAY = function(numberOfBlocks=1,
                               numberOfTrajectories = 1,
                               intervention=NULL,
                               verbose=FALSE,
                               version="slow") {
          ## Retrieving arguments
          numberOfBlocks <- Arguments$getInteger(numberOfBlocks, c(1, Inf))


          if (!is.null(intervention)) {
            InterventionParser.valid_intervention(intervention)
          }
          verbose <- Arguments$getVerbose(verbose)


          rsource <- list(bernoulli=runif,
                          gaussian=rnorm)


          ## verbose
          if (is.null(intervention)) {
            msg <- "Simulating...\n"
          } else {
            msg <- "Simulating under the specified intervention...\n"
          }
          verbose && cat(verbose, msg)

          init <- rep(NA, numberOfBlocks)
          ## sources of randomness
          UU <- cbind(W=init, A=init, Y=init)
          for (ii in 1:3) {
            UU[, ii] <- rsource[[self$get_families[ii]]](numberOfBlocks)
          }

          #WAY <- rep(init, 3)
          WAY <- matrix(rep(init, 3), ncol=3, dimnames=list(NULL, c("W", "A", "Y")))
          if (version=="slow") {
            ## -------------
            ## first version ## must be very slow
            ## -------------
            functions = list(list(name = 'W', fun = private$qw),
                             list(name = 'A', fun = private$ga),
                             list(name = 'Y', fun = private$Qy))
            for (ii in 1:numberOfBlocks) {
              for (j in seq_along(functions)) {
                fun <- functions[[j]]
                variable = fun$name
                past <- self$get_past(variable, ii, WAY)
                WAY[ii, variable] <- self$get_rgen(variable)(UU[ii, j], fun$fun(past))
              }
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

        generateMechanism = function(param, family=c("bernoulli", "gaussian")) {
          ## Retrieving arguments
          param <- Arguments$getNumerics(param)
          memory <- length(param)-1
          family <- match.arg(family)
          link <- switch(family,
                         bernoulli = expit,
                         gaussian = identity)

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
                link(par[1] + par[-1] %*% xx)
              } else {
                link(par[1] + sum(par[-1] * xx))
              }
            }
          }
          attr(mechanism, "memory") <- memory
          attr(mechanism, "family") <- family
          return(mechanism)
        },


        retrieve_relevant_past = function(of, at, mem) {
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
  active = 
    list(
      get_families = function(){
        return(private$families)
      },

      get_memories = function(){
        return(private$memories)
      }
    ),
  private =
    list(
      qw = NULL,
      ga = NULL,
      Qy = NULL,
      rgen = NULL,
      memories = NULL,
      families = NULL,

      get_rgen = function(variable) {
        private$rgen[[self$get_families[variable]]]
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

      get_past = function(variable, ii, WAY){
        past.idx <- self$retrieve_relevant_past(variable, ii, self$get_memories[variable])
        which.pos <- which(past.idx > 0)
        past <- rep(0, self$get_memories[variable])
        past[which.pos] <- WAY[past.idx[which.pos]]
      }
    )
)
