setOldClass("h2o")

#' H2O.Initializer
#'
#' @importFrom R6 R6Class
#' @export
H2O.Initializer <-
  R6Class(
          "H2O.Initializer",
          private =
            list(
                 localH2O = NULL
                 ),

          public =
            list(
                 initialize = function(host = "localhost", port = 54321, runlocal = TRUE) {
                   if(!h2o.clusterIsUp()) {
                     private$localH2O = h2o.init(ip = host, port = port, startH2O = runlocal)
                   }
                 },

                 getValidity = function() {
                   errors <- character()
                   if(!h2o.clusterIsUp()) {
                     msg <- 'The provided cluster is down. Please use a cluster that is up, or run a local one.'
                     errors <- c(errors, msg)
                   }
                   if (length(errors) == 0) TRUE else errors
                 }
                 )
          )

