#' General packages used by all of the other classes
#' @import methods
#' @import R.oo
#' @import R.utils
#' @import foreach
#' @import parallel
#' @importFrom R.utils Arguments
#' @importFrom R.methodsS3 throw
#' @import magrittr
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

