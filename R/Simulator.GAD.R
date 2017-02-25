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

        ## TODO: Convert the mechanisms to classes
        simulateWAYOneTrajectory = function(numberOfBlocks=1,
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
                                            verbose=FALSE,
                                            msg=NULL) {

          ## retrieving arguments
          numberOfBlocks <- Arguments$getInteger(numberOfBlocks, c(1, Inf))
          qw <- private$validateMechanism(qw)
          ga <- private$validateMechanism(ga)
          Qy <- private$validateMechanism(Qy)

          if (!is.null(intervention)) {
            intervention <- private$validateIntervention(intervention)
          }
          verbose <- Arguments$getVerbose(verbose)

          if (missing(msg)) {
            what <- "one trajectory"
          } else {
            what <- Arguments$getCharacter(msg)
          }

          memories <- c(W=length(qw$param)-1,
                        A=length(ga$param)-1,
                        Y=length(Qy$param)-1)


          if (is.null(intervention)) {
            msg <- paste("Simulating ", what, "...\n", sep="")
          } else {
            msg <- paste("Simulating ", what, " under the specified intervention...\n", sep="")
          }
          verbose && cat(verbose, msg)

          numberOfBlocksPrime <- numberOfBlocks+max(memories)
          ## backbone
          UW <- qw$stochMech(numberOfBlocksPrime)
          UA <- ga$stoch(UW)
          UY <- Qy$stochMech(UA, UW)

          configuration <- list(list(var="W", mech=qw, U=UW),
                                list(var="A", mech=ga, U=UA),
                                list(var="Y", mech=Qy, U=UY))

          WAY <- lapply(configuration, function(entry) {
            idx <- (max(memories)+1):length(entry$U)
            tempMAT <- entry$U[idx]
            if (memories[entry$var] >= 1) {
              for (ii in 1:memories[entry$var]) {
                idx <- idx-1
                tempMAT <- cbind(tempMAT, entry$U[idx])
              }
            }
            outcome <- entry$mech$rgen(tempMAT %*% entry$mech$param)
            ## naming
            if (is.vector(outcome)) {
              outcome <- matrix(outcome, ncol=1)
            }
            if (ncol(outcome)==1) {
              col.names <- entry$var
            } else if (ncol(outcome)>=2) {## this should not happen, yet
              col.names <- paste(entry$var, seq(ncol(outcome)), sep="")
            } else {## nor this, ever
              throw("Matrix 'outcome' should have at least one column...")
            }
            attr(outcome, "col.names") <- col.names
            return(outcome)
          })
          WAY.mat <- matrix(unlist(WAY), nrow=numberOfBlocks, byrow=FALSE)
          colnames(WAY.mat) <- sapply(WAY, function(ll){attr(ll, "col.names")})
          
          return(as.data.table(WAY.mat))
        },

        simulateWAYiidTrajectories = function(numberOfBlocks=1,
                                              numberOfTrajectories=2,
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
          ## retrieving arguments
          numberOfTrajectories <- Arguments$getInteger(numberOfTrajectories, c(1, Inf))
          if (numberOfTrajectories==1) {
            throw("To simulate one single trajectory, better call 'simulateWAYOneTrajectory' than 'simulateWAYiidTrajectories'...\n")
          }
          what <- paste(numberOfTrajectories, "trajectories")

          WAYs <- lapply(rep(numberOfBlocks, numberOfTrajectories),
                         self$simulateWAYOneTrajectory,
                         qw=qw, ga=ga, Qy=Qy, intervention=intervention, verbose=verbose, msg=what)

          browser()
          
          col.names <- colnames(WAYs[[1]])
          WAYs <- Reduce(rbind, WAYs)
          WAYs <- matrix(unlist(WAYs), ncol=length(col.names)*numberOfTrajectories)
          colnames(WAYs) <- paste(rep(col.names, each=numberOfTrajectories),
                                  1:numberOfTrajectories, sep=".")
          
          return(WAYs)
        }))
