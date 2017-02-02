#' H2o.model.gbm
setOldClass("h2o")
H2o.model.GBM <- setClass("H2o.model.gbm",
  representation = representation(
      h2o = "H2OConnection"
    ),

  prototype(),

  validity = function(object) {
    TRUE
  }
)
