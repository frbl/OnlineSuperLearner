#' General packages used by all of the other classes
#' @import methods
#' @import R.oo
#' @import R.utils
#' @import magrittr
#' @import parallel
generalImports <- list()

# General fixes, for usability
expit <- plogis
logit <- qlogis
throw <- stop

