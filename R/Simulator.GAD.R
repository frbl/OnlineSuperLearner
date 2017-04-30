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
#'   \item{\code{simulateWAY(numberOfBlocks, qw, ga, Qy, intervention, verbose)}}{
#'     Runs the simulation using the parameters provided.
#'     \code{numberOfBlocks} integer is used to specify the number of required simulated observations.
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
#'   \item{\code{simulateWAYiidTrajectories(numberOfBlocks, numberOfTrajectories, qw, ga, Qy, intervention, verbose)}}{
#'     does the exact same as simulateWAY, however, this function also takes an numberOfTrajectories argument, in which one can specify how many concurrent time series should be generated.
#'     \code{numberOfBlocks} integer is used to specify the number of required simulated observations.
#'     \code{numberOfTrajectories} integer the number of concurrent time series should be generated
#'     \code{qw} is the underlying mechanism creating the covariate measurements.
#'     \code{ga} is the underlying mechanism creating the exposure measurements.
#'     \code{Qy} is the underlying mechanism creating the outcome measurements.
#'     \code{intervention}
#'     \code{verbose}
#'   }
#' }
#' @export
Simulator.GAD <- R6Class("Simulator.GAD",
  private =
    list(
        validateMechanism = function(ll, what = c("qw", "ga", "Qy")) {
          # TODO: Don't use the index, use the actual key to the value (that is what is used in the code)
          what <- match.arg(what)
          if (what %in% c("qw", "ga")) {
            if (!is.list(ll)) {
              throw("The argument should be a list, not ", mode(ll))
            }
            if (!length(ll) == 3) {
              throw("The argument should be a list of length three, not ", length(ll))
            }
            stochMech <- ll[[1]]
            if (!mode(stochMech) == "function") {
              throw("The first element of the argument should be a function, not", mode(stochMech))
            }
            param <- Arguments$getNumerics(ll[[2]])
            rgen <- ll[[3]]
            if (!mode(rgen) == "function") {
              throw("The first element of the argument should be a function, not", mode(rgen))
            }
          } else {
            if (!is.list(ll)) {
              throw("The argument should be a list, not ", mode(ll))
            }
            if (!length(ll) == 1) {
              throw("The argument should be a list of length one, not ", length(ll))
            }
            rgen <- ll[[1]]
            if (!mode(rgen) == "function") {
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
          if (length(when)!= length(what)) {
            throw("Entries 'when' and 'what' of the argument should be of same length, not ",
                  paste(length(when), length(what)))
          }
          when.idx <- sort(when, index.return = TRUE)$ix
          ll <- list(when = when[when.idx], what = what[when.idx])
          return(ll)
        },

        calclate_configuration = function(configuration, memory_max, numberOfBlocks) {
          outcomes <- lapply(configuration, function(entry) {
            # TODO: Describe why we take the max here, instead of just the memory of the current variable.
            # TODO: Deine memory_max variable
            idx <- memory_max:length(entry$U)
            tempMAT <- entry$U[idx]
            if (entry$memory >= 1) {                
              for (ii in 1:entry$memory) {
                idx <- idx-1
                tempMAT <- cbind(tempMAT, entry$U[idx])
              }
            }
            if (!is.matrix(tempMAT)) tempMAT <- as.matrix(tempMAT)
            outcome <- entry$mech$rgen(tempMAT %*% entry$mech$param)

            if (is.vector(outcome)) {
              outcome <- matrix(outcome, ncol = 1)
            }
            # naming
            if (ncol(outcome) == 1) {
              col.names <- entry$var
            } else if (ncol(outcome) >= 2) {# this should not happen, yet
              col.names <- paste(entry$var, seq(ncol(outcome)), sep = "")
            } else {# nor this, ever
              throw("Matrix 'outcome' should have at least one column...")
            }
            attr(outcome, "col.names") <- col.names
            return(outcome)
          }) %>% 
            unlist %>%
            matrix(unlist(.), nrow = numberOfBlocks, byrow = FALSE)
          colnames(outcomes) <- sapply(outcomes, function(ll){attr(ll, "col.names")})
          outcomes
        },

      #TODO: Change the parameters qw ga en Qy to an object
      # Antoine:
      #
      # Now, argument  'qw' can  be a list  of what used  to be  the argument
      # 'qw'. The entries  of the list are  each used in the  same fashion to
      # create  a sequence  (W(t) :  t) of  (every W(t)  is one-dimensional).
      # Only the first sequence plays a  role in the random generation of the
      # sequences  (A(t)  :  t)  and  (Y(t) :  t).   This  sequence  and  the
      # corresponding mechanism are said 'relevant'.
        simulateWAYOneTrajectory = function(numberOfBlocks = 1,
                                            qw = list(
                                              # this  describes the  'relevant' mechanism
                                              list(stochMech = rnorm,
                                                   param = rep(1, 2),
                                                   rgen = identity),
                                              list(
                                                stochMech = rnorm,
                                                param = rep(1, 2),
                                                rgen = identity)),
                                            ga = list(stochMech ={ function(ww){rbinom(length(ww), 1, expit(ww))}},
                                                      param = rep(1, 2),
                                                      rgen ={ function(xx, delta = 0.05){rbinom(length(xx), 1, delta+(1-2*delta)*expit(xx))}}),
                                            Qy = list(rgen ={ function(AW){
                                              aa <- AW[, "A"]
                                              ww <- AW[, grep("[^A]", colnames(AW))]
                                              mu <- aa*(0.4-0.2*sin(ww[,1])+0.05*ww[,1]) +
                                                (1-aa)*(0.2+0.1*cos(ww[,1])-0.03*ww[,1])
                                              rnorm(length(mu), mu, sd = 1)}}),
                                            intervention = NULL,
                                            verbose = FALSE,
                                            msg = NULL) {
          
          # retrieving arguments
          numberOfBlocks <- Arguments$getInteger(numberOfBlocks, c(1, Inf))
          if (!is.null(names(qw)) && names(qw)[1] == "stochMech") {
            # only a 'relevant' mechanism
            qw <- list(private$validateMechanism(qw, what = "qw"))
          } else {
            # a 'relevant' mechanism and at least an 'irrelevant' mechanism
            qw <- lapply(qw, private$validateMechanism, what = "qw")
          }
          ga <- private$validateMechanism(ga, what = "ga")
          Qy <- private$validateMechanism(Qy, what = "Qy")

          if (!is.null(intervention)) {
            intervention <- private$validateIntervention(intervention)
          }
          verbose <- Arguments$getVerbose(verbose)

          if (missing(msg)) {
            what <- "one trajectory"
          } else {
            what <- Arguments$getCharacter(msg)
          }

          # Each observation has n memories, which are provided in the list params.
          # The length of the list is therefore the number of memories, the value
          # the actual 'influence' of the memory at that moment of time in history.
          memories <- c(W = length(qw[[1]]$param)-1,
                        A=length(ga$param)-1)

          if (is.null(intervention)) {
            msg <- paste("Simulating ", what, "...\n", sep = "")
          } else {
            msg <- paste("Simulating ", what, " under the specified intervention...\n", sep = "")
          }
          verbose && cat(verbose, msg)

          numberOfBlocksPrime <- numberOfBlocks+max(memories)

          # backbone
          # Create #by (actually unmeasured) confounder observations to serve
          # as a base for the stochasticMechanism. These observations are in
          # a later step used for creating the UA and UY observations.

          # only the 'relevant'  sequence is considered here;  if needed, the
          # 'irrelevant' sequences will be generated later
          UW <- (qw[[1]])$stochMech(numberOfBlocksPrime)
          UA <- ga$stoch(UW)

          configuration <- list(list(var = "W", mech = qw[[1]], U = UW, memory = memories['W']),
                                list(var = "A", mech = ga, U = UA, memory = memories["A"]))

          WA.mat <- private$calclate_configuration(configuration  = configuration,
                                                   memory_max = max(memories)+1,
                                                   numberOfBlocks = numberOfBlocks)

          if (!is.null(intervention)) {
            # column 'A' is the last one, but nevertheless:
            which.col <- grep("A", colnames(WA.mat))
            #
            when <- intervention$when
            when.idx <- which(when <= numberOfBlocks)
            when <- when[when.idx]
            what <- intervention$what[when.idx]
            #
            WA.mat[cbind(when, which.col)] <- what
          }

          Y <- Qy$rgen(WA.mat)
          WAY.mat <- cbind(WA.mat, Y = Y)

          # TODO: We can probbly merge this whole calculation with the previous one. In that case we just define
          # one big 'configuration' object, and run that through the data generator (instead of two small configs).
          if (length(qw) > 1) {
            # 'irrelevant' sequences
            for (cov in 2:length(qw)) {
              UW <- qw[[cov]]$stochMech(numberOfBlocksPrime)
              configuration <- list(list(var = paste("W", cov, sep=""), mech = qw[[cov]],
                                         U = UW, memory = length(qw[[cov]]$param)-1))

              irrelevantW.mat <- private$calclate_configuration(configuration  = configuration,
                                                                memory_max = max(memories)+1,
                                                                numberOfBlocks = numberOfBlocks)
              WAY.mat <- cbind(irrelevantW.mat, WAY.mat)
            }
          }
          return(as.data.table(WAY.mat))
        }
    ),
  public =
    list(
      initialize = function() {
      },

      simulateWAY = function(numberOfBlocks = 1,
                             numberOfTrajectories = 1,
                             qw = list(stochMech = rnorm,
                                       param = rep(1, 2),
                                       rgen = identity),
                             ga = list(stochMech = {function(ww){rbinom(length(ww), 1, expit(ww))}},
                                       param = rep(1, 2),
                                       rgen = {function(xx, delta = 0.05){rbinom(length(xx), 1, delta+(1-2*delta)*expit(xx))}}),
                             Qy = list(rgen = {function(AW){
                               aa <- AW[, "A"]
                               ww <- AW[, grep("[^A]", colnames(AW))]
                               mu <- aa*(0.4-0.2*sin(ww)+0.05*ww) +
                                 (1-aa)*(0.2+0.1*cos(ww)-0.03*ww)
                               rnorm(length(mu), mu, sd = 1)}}),
                             intervention = NULL,
                             verbose = FALSE) {
        # retrieving arguments
        numberOfTrajectories <- Arguments$getInteger(numberOfTrajectories, c(1, Inf))
        if (numberOfTrajectories == 1) {
          return(private$simulateWAYOneTrajectory(numberOfBlocks = numberOfBlocks,
                                                  qw = qw, ga = ga, Qy = Qy,
                                                  intervention = intervention,
                                                  verbose = verbose))
        }

        what <- paste(numberOfTrajectories, "trajectories")
        WAYs <- lapply(rep(numberOfBlocks, numberOfTrajectories),
                       private$simulateWAYOneTrajectory,
                       qw = qw, ga = ga, Qy = Qy, intervention = intervention, verbose = verbose, msg = what)

        col.names <- colnames(WAYs[[1]])
        WAYs <- Reduce(rbind, WAYs)
        WAYs <- matrix(unlist(WAYs), ncol = length(col.names)*numberOfTrajectories)
        colnames(WAYs) <- paste(rep(col.names, each = numberOfTrajectories),
                                1:numberOfTrajectories, sep = ".")

        return(WAYs)
      }
    )
  )
