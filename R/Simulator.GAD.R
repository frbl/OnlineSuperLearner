#' Simulator.GAD
#'
#' @docType class
#' @importFrom R6 R6Class
Simulator.GAD <-
  R6Class (
           "Simulator.GAD",
           private =
             list(
                  validateMechanism = function(ll) {
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
                    param <- Arguments$getNumerics(ll[[2]])
                    rgen <- ll[[3]]
                    if (!mode(rgen)=="function") {
                      throw("The first element of the argument should be a function, not", mode(rgen))
                    }
                    return(ll)
                  }
                  ),
           public =
             list(
                  initialize = function() {
                  },
                  simulateWAY = function(by=1,
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
                    qw <- private$validateMechanism(qw)
                    ga <- private$validateMechanism(ga)
                    Qy <- private$validateMechanism(Qy)
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
                  }
                  )
           )
