#' ML
#' @include Data.Base.R
ML <- setClass("ML.Base", representation(data = "Data.Base"))

setGeneric("fit", function(obj, X, y, data) standardGeneric("fit"))

setMethod("fit", signature(obj = "ML.Base", X = "character", y ="vector", data="ANY"),
  function(obj, X, y, data) {
    throw('This method needs to be inherited')
  }
)
