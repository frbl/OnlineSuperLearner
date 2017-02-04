#' ML.Local.lm
#' @include ML.Local.R
ML.Local.lm <- setClass("ML.Local.lm", contains = 'ML.Local')

setMethod("fit", signature(obj = "ML.Local.lm", X = "character", y ="vector", data="ANY"),
  function(obj, X, y, data) {
    formula = paste(y, '~', X)
    lm(formula, data)
  }
)

