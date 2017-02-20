#' Simulator.GAD
#' directed acyclic graph approach to simulation
#'
#' @docType class
#' @importFrom R6 R6Class
#' @section Methods:
#' \describe{
#'   \item{\code{new()}}{
#'     Starts a new GAD based simulator.
#'   }
#'   \item{\code{new(by, qw, ga, Qy, intervention, verbose)}}{
#'     Runs the simulation using the parameters provided.
#'     \code{by} is used to specify the number of required simulated observations.
#'     \code{qw} is the underlying mechanism creating the covariate measurements.
#'     \code{ga} is the underlying mechanism creating the exposure measurements.
#'     \code{Qy} is the underlying mechanism creating the outcome measurements.
#'     \code{intervention}
#'     \code{verbose}
#'    The each of the \code{qw}, \code{ga}, \code{Qy} requires a list as parmeter, each having 3 fields:
#'    \code{stochMech} function the mechanism that is used to generate the underlying unmeasured confounders. These 'observations' form the basis of the data generative process.
#'    \code{param} vector with memories used to generate the dataset. Each entry in this vector is a coefficient for that point in history.
#'                 Note that this also includes preceeding measurements within the current observation, i.e. W A Y -> A precedes Y, W precedes A, etc. 
#'                 If we would thus have a memory of \code{c(0.1,0.5)} for \code{W}, that would mean that the current measurement of W is generated using the Y in the 
#'                 previous measurment for 0.1, and A in the previous measurement for 0.5.
#'    \code{rgen} function 
#'   }
#'   \item{\code{run(data, Y, A, W,  initial.data.size = 10)}}{
#'     Runs the actual OnlineSuperLearning calculation
#'   }
#'   \item{\code{getModel()}}{
#'     Returns the final OnlineSuperLearner model
#'   }
#'   \item{\code{predict(data, X)}}{
#'     returns an actual prediction using the superlearning model
#'   }
#' }
#' @export
Simulator.GAD <-
  R6Class (
           "Simulator.GAD",
           private =
             list(
                  validateMechanism = function(ll) {
                    # TODO: Don't use the index, use the actual key to the value (that is what is used in the code)
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

                  #TODO: Change the parameters qw ga en Qy to an object
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

                    # Each observation has n memories, which are provided in the list params.
                    # The length of the list is therefore the number of memories, the value
                    # the actual 'influence' of the memory at that moment of time in history.
                    memories <- c(W=length(qw$param)-1,
                                  A=length(ga$param)-1,
                                  Y=length(Qy$param)-1)


                    if (is.null(intervention)) {
                      msg <- "Simulating...\n"
                    } else {
                      msg <- "Simulating under the specified intervention...\n"
                    }
                    verbose && cat(verbose, msg)

                    by <- by + max(memories)
                    ## backbone

                    # Create #by (actually unmeasured) confounder observations to serve
                    # as a base for the stochasticMechanism. These observations are in
                    # a later step used for creating the UA and UY observations.
                    UW <- qw$stochMech(by)
                    UA <- ga$stochMech(UW)
                    UY <- Qy$stochMech(UA, UW)

                    configuration <- list(list(var = 'W',q = qw, U = UW),
                                          list(var = 'A',q = ga, U = UA),
                                          list(var = 'Y',q = Qy, U = UY))

                    WAY <- lapply(configuration, function(entry) {
                      # TODO: Why do we take the max here, instead of just the memory of the current variable?
                      idx <- (max(memories)+1):length(entry$U)

                      tempMAT <- entry$U[idx]
                      if (memories[entry$var] >= 1) {
                        for (ii in 1:memories[entry$var]) {
                          idx <- idx-1
                          tempMAT <- cbind(tempMAT, entry$U[idx])
                        }
                      }

                      # Create linear combination of the parameters / coefficients and the historical data
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
