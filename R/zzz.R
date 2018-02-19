#' General packages used by all of the other classes
#' @import methods
#' @import foreach
#' @import parallel
#' @import R.utils
#' @importFrom R.oo equals
#' @importFrom R.utils Verbose Arguments
#' @importFrom R.methodsS3 throw
#' @importFrom data.table data.table as.data.table is.data.table rbindlist copy shift setDT
#' @importFrom magrittr %>% %<>% %T>%
#' @import assertthat
generalImports <- list()

# General fixes, for usability
expit <- plogis
logit <- qlogis
#throw <- stop

# Helper functions
#' Checks whether an object is an instance of the provided class
#' @param obj the object to check the class from
#' @param obj.class the class which we expect it to enherit from
is.a <- function(obj, obj.class) {
  obj.class <- Arguments$getCharacter(obj.class)
  obj.class %in% class(obj)
}

create_object_from_string <- function(string_object_name, args=list()) {
  return(do.call(eval(parse(text=string_object_name))$new, args = args))
}

.onAttach <- function(...) {
  packageStartupMessage('OnlineSuperLearner')
  packageStartupMessage('The OnlineSuperLearner package is still in beta testing. Interpret results with caution.')
}

hide_warning_rank_deficient_fit_prediction <- function(the_function){
  h <- function(w) if( any( grepl( "prediction from a rank-deficient fit may be misleading", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(the_function, warning = h)
}

hide_warning_rank_deficient_matrix <- function(the_function){
  h <- function(w) if( any( grepl( "the matrix is either rank-deficient or indefinite", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(the_function, warning = h)
}

hide_warning_probabilities_numerically_zero_or_one <- function(the_function){
  h <- function(w) if( any( grepl( "fitted probabilities numerically 0 or 1 occurred", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(the_function, warning = h)
}

hide_warning_convergence <- function(the_function){
  h <- function(w) if( any( grepl( "converge", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(the_function, warning = h)
}

hide_warning_high_h_ratio <- function(the_function){
  h <- function(w) if( any( grepl( "H-ratio is very high", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(the_function, warning = h)
}

hide_warning_test <- function(the_function){
  h <- function(w) if( any( grepl( "Test function was called!", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(the_function, warning = h)
}

hide_warning_replace_weights_osl <- function(the_function) {
  h <- function(w) if( any( grepl( "The weights provided will be overridden by a random vector", w) ) ) invokeRestart( "muffleWarning" )
  withCallingHandlers(the_function, warning = h)
}
