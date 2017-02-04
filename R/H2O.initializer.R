setOldClass("h2o")

#' H2O.initializer
H2O.initializer <- setClass("H2O.initializer",
  representation = representation(
      h2o = "H2OConnection"
    ),

  prototype(),

  validity = function(object) {
    errors <- character()
    if(!h2o.clusterIsUp()) {
      msg <- 'The provided cluster is down. Please use a cluster that is up, or run a local one.'
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
  }
)

#' H2O.initializer constructor function
#'
#' @param host the host h2o is running on (defaults to localhost)
#' @param port the port h2o runs on (defaults to 54321)
#' @param runlocal whether or not we should start h2o, or whether it is running somewhere already
#' @import h2o
#' @rdname H2O.initializer
#' @return an instance of H2O.initializer
setGeneric("H2O.initializer", function(host = "localhost", port = 54321, runlocal = TRUE) standardGeneric("H2O.initializer"))

#' @rdname H2O.initializer
setMethod("H2O.initializer", signature(host = "ANY", port ="ANY", runlocal="ANY"),
          #TODO: Can we make the signature more explicit?
  function(host, port, runlocal) {
    localH2O = h2o.init(ip = host, port = port, startH2O = runlocal)
    new("H2O.initializer", h2o = localH2O)
  }
)

