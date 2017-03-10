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
        validateMechanism = function(ll, what=c("qw", "ga", "Qy")) {
          what <- match.arg(what)
          if (what %in% c("qw", "ga")) {
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
          } else {
            if (!is.list(ll)) {
              throw("The argument should be a list, not ", mode(ll))
            }
            if (!length(ll)==1) {
              throw("The argument should be a list of length one, not ", length(ll))
            }
            rgen <- ll[[1]]
            if (!mode(rgen)=="function") {
              throw("The unique element of the argument should be a function, not", mode(rgen))
            }            
          }
          return(ll)
        },

        validateIntervention = function(ll) {
          if (!is.list(ll)) {
            throw("The argument should be a list, not ", mode(ll))
          }
          nms <- names(ll)
          if (!identical(nms, c("when", "what"))) {
            thow("The argument should be a list with two tags, 'when' and 'what', not ", nms)
          }
          when <- Arguments$getIntegers(ll$when, c(1, Inf))
          what <- Arguments$getIntegers(ll$what, c(0, 1))
          if (length(when)!=length(what)) {
            throw("Entries 'when' and 'what' of the argument should be of same length, not ",
                  paste(length(when), length(what)))
          }
          when.idx <- sort(when, index.return=TRUE)$ix
          ll <- list(when=when[when.idx], what=what[when.idx])
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
                                            Qy=list(rgen={function(AW){
                                              aa <- AW[, "A"]
                                              ww <- AW[, grep("[^A]", colnames(AW))]
                                              mu <- aa*(0.4-0.2*sin(ww[,1])+0.05*ww[,1]) +
                                                (1-aa)*(0.2+0.1*cos(ww[,1])-0.03*ww[,1])
                                              rnorm(length(mu), mu, sd=1)}}),
                                            intervention=NULL,
                                            verbose=FALSE,
                                            msg=NULL) {

          ## retrieving arguments
          numberOfBlocks <- Arguments$getInteger(numberOfBlocks, c(1, Inf))
          qw <- private$validateMechanism(qw, what="qw")
          ga <- private$validateMechanism(ga, what="ga")
          Qy <- private$validateMechanism(Qy, what="Qy")

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
          ## --------
          ## backbone
          ## --------
          UW <- qw$stochMech(numberOfBlocksPrime)
          UA <- ga$stoch(UW)

          configuration <- list(list(var="W", mech=qw, U=UW),
                                list(var="A", mech=ga, U=UA))

          WA <- lapply(configuration, function(entry) {
            idx <- (max(memories)+1):length(entry$U)
            tempMAT <- entry$U[idx]
            if (memories[entry$var] >= 1) {
              for (ii in 1:memories[entry$var]) {
                idx <- idx-1
                tempMAT <- cbind(tempMAT, entry$U[idx])
              }
            }
            outcome <- entry$mech$rgen(tempMAT %*% entry$mech$param)
            if (is.vector(outcome)) {
              outcome <- matrix(outcome, ncol=1)
            }
            ## naming
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

          WA.mat <- matrix(unlist(WA), nrow=numberOfBlocks, byrow=FALSE)
          colnames(WA.mat) <- sapply(WA, function(ll){attr(ll, "col.names")})

          if (!is.null(intervention)) {
            ## column 'A' is the last one, but nevertheless:
            which.col <- grep("A", colnames(WA.mat))
            ##
            when <- intervention$when
            when.idx <- which(when<=numberOfBlocks)
            when <- when[when.idx]
            what <- intervention$what[when.idx]
            ##
            WA.mat[cbind(when, which.col)] <- what
          }
          
          Y <- Qy$rgen(WA.mat)
          WAY.mat <- cbind(WA.mat, Y=Y)
          
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
                                              Qy=list(rgen={function(AW){
                                              aa <- AW[, "A"]
                                              ww <- AW[, grep("[^A]", colnames(AW))]
                                              mu <- aa*(0.4-0.2*sin(ww)+0.05*ww) +
                                                (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
                                              rnorm(length(mu), mu, sd=1)}}),
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

          col.names <- colnames(WAYs[[1]])
          WAYs <- Reduce(rbind, WAYs)
          WAYs <- matrix(unlist(WAYs), ncol=length(col.names)*numberOfTrajectories)
          colnames(WAYs) <- paste(rep(col.names, each=numberOfTrajectories),
                                  1:numberOfTrajectories, sep=".")
          
          return(WAYs)
        }))
