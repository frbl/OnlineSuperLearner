#' @include ML.Base.R
#' @include Data.Base.R
.ML.H2O <- setClass("ML.H2O", representation(data = "Data.Base"), contains = 'ML.Base')

ML.H2O <- function(data, ...) {
  print('SUPER!!!!')
  data = h2o.importFile(getAll(data))
  .ML.Base(data=data, ...)
}
