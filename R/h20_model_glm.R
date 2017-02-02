#' H2o.model.GLM
setOldClass("h2o")
H2o.model.GLM <- setClass("H2o.model.GLM",
  representation = representation(
     nfolds = "numeric",
     alpha = "numeric",
     family = "character"
    ),

  prototype(
     nfolds = 10,
     alpha = 0.5,
     family = 'binomial'
    ),

  validity = function(object) {
    errors <- character()
    if(!h2o.clusterIsUp()) {
      msg <- 'Cluster is not yet started'
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) TRUE else errors
    TRUE
  }
)


setGeneric("fit", function(obj, X, y, data) standardGeneric("fit"))

#' @rdname H2oInitializer
setMethod("fit", signature(obj = "H2o.model.GLM", X = "character", y ="vector", data="ANY"),
  function(obj, X, y, data) {
    h2o.glm(x=X, y=y, training_frame=data, family=obj@family,
            nfolds=obj@nfolds, alpha=obj@alpha)
  }
)

