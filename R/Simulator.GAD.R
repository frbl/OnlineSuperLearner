#' Simulator.GAD
#' directed acyclic graph approach to simulation
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
                  },

                  validateIntervention = function(ll) {
                    ## ...
                    return(ll)
                  }
                  ),
           public =
             list(
                  initialize = function() {
                  },

                  # TODO: Convert the mechanisms to classes
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

                    ## Retrieving arguments
                    by <- Arguments$getInteger(by, c(1, Inf))
                    qw <- private$validateMechanism(qw)
                    ga <- private$validateMechanism(ga)
                    Qy <- private$validateMechanism(Qy)

                    if (!is.null(intervention)) {
                      intervention <- private$validateIntervention(intervention)
                    }
                    verbose <- Arguments$getVerbose(verbose)

                    memories <- c(W=length(qw$param)-1,
                                  A=length(ga$param)-1,
                                  Y=length(Qy$param)-1)


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

                    configuration <- list(list(var = 'W',q = qw, U = UW),
                                          list(var = 'A',q = ga, U = UA),
                                          list(var = 'Y',q = Qy, U = UY))

                    WAY <- lapply(configuration, function(entry) {
                      idx <- (max(memories)+1):length(entry$U)
                      tempMAT <- entry$U[idx]
                      if (memories[entry$var] >= 1) {
                        for (ii in 1:memories[entry$var]) {
                          idx <- idx-1
                          tempMAT <- cbind(tempMAT, entry$U[idx])
                        }
                      }
                      outcome <- entry$q$rgen(tempMAT %*% entry$q$param)

                      # TODO: Set names for the vector outcome column as well
                      if(!is.matrix(outcome)) {
                       return(outcome)
                      }

                      # Set the colnames of the outcome, only if it has more than 1 dimension
                      col.names <- paste(entry$var, seq(ncol(outcome)), sep='')
                      colnames(outcome) <- col.names

                      # Return the outcome
                      outcome

                    })

                    return(as.data.table(WAY))
                  }
                  )
           )
