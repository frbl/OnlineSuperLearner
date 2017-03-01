setOldClass("h2o")

#' H2O.Initializer
#' Idealy I'd like to use a singletonpattern for this, but this also works.
#'
#' @docType class
#' @import h2o
H2O.Initializer <- function(host = "localhost", port = 54321, runlocal = TRUE, verbose = FALSE) {
  if(h2o.clusterIsUp()) {
    verbose && cat(verbose, 'Cluster is up, not initializing.')
    return(FALSE)
  }
  verbose && enter(verbose, 'Initializing cluster...')
  GlobalH2OCluster <<- h2o.init(ip = host,
                                port = port,
                                startH2O = runlocal)

  if(!h2o.clusterIsUp()) {
    throw('Connecting to cluster failed, at host', host)
  }
  verbose && exit(verbose)
  return(TRUE)
}

#H2O.Initializer <-
#R6Class(
#"H2O.Initializer",
#private =
#list(
#localH2O = NULL
#),
#active =
#list(
#getValidity = function() {
#errors <- character()
#if(!h2o.clusterIsUp()) {
#msg <- 'The provided cluster is down. Please use a cluster that is up, or run a local one.'
#errors <- c(errors, msg)
#}
#if (length(errors) == 0) TRUE else errors
#}
#),
#public =
#list(
#initialize = function(host = "localhost", port = 54321, runlocal = TRUE) {
#if(!h2o.clusterIsUp()) {
#private$localH2O = h2o.init(ip = host, port = port, startH2O = runlocal)
#}
#}
#)
#)

