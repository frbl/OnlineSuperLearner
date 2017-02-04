#' ML.H2O.glm
#' @include ML.H2O.R
#' @include Data.Base.R
setOldClass("h2o")
.ML.H2O.glm <- setClass("ML.H2O.glm",
  representation = representation(
     data = "Data.Base",
     nfolds = "numeric",
     alpha = "numeric",
     family = "character"
    ),


  validity = function(object) {
    errors <- character()
    print('hoi')
    if (is.null(object@data)) {
      msg <- 'You have to provide some data!'
      errors <- c(errors, msg)
    }
    if(!h2o.clusterIsUp()) {
      msg <- 'Cluster is not yet started'
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
  },
  contains = 'ML.H2O'
)

ML.H2O.glm <- function(data, nfolds = 10, alpha = 0.5, family = 'binomial') {
  browser()
 .ML.H2O.glm(data=data, nfolds = nfolds, alpha = alpha, family = family)
 callNextMethod()
}

setMethod("fit", signature(obj = "ML.H2O.glm", X = "character", y ="vector", data="ANY"),
  function(obj, X, y, data) {
    h2o.glm(x=X, y=y, training_frame=obj@data, family=obj@family,
            nfolds=obj@nfolds, alpha=obj@alpha)
  }
)

