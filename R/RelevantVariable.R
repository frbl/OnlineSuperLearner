#' RelevantVariable
#'
#' This class represents the structure we see as a relevant variable. A relevant
#' variable in our case is a variable for which we want to estimate its
#' conditional distribution. This class helps organizing / creating its formula
#' (the parametric form of the covariates that could predict this relevant
#' variable).
#'
#' @docType class
#' @importFrom R6 R6Class
#'
#' @section Methods: 
#' \describe{  
#'   \item{\code{initialize(formula, family) }}{ 
#'     Creates a new \code{RelevantVariable} object. It creates this object
#'     according to a formula (the y ~ x1 + x2 representation we expect to
#'     predict this relevant variable) and the family (gaussian if it is a double
#'     / integer value, and binomial if it is a binary variable)
#'
#'     @param formula formula the formula to use for predicting this relevant
#'      variable.
#'     @param family string a string representing the type of relevant variable
#'      this is.
#'   } 
#' 
#'   \item{\code{get_formula_string(X = NULL, Y = NULL) }}{ 
#'     Returns a string representation as a formula of the current relevant
#'     variable and its predictors.
#'
#'     @param X string (default = NULL) the variables to use as the predicting
#'      variables in this formula. If \code{NULL}, it will use the X of the
#'      current relevant variable.
#'
#'     @param Y string (default = NULL) the variables to use as the outcome
#'      variable in this formula. If \code{NULL}, it will use the X of the
#'      current relevant variable.
#'   } 
#' 
#'   \item{\code{parse_formula(formula) }}{ 
#'     Converts a formula to its separate X and Y components.
#'
#'     @param formula formula the formula to convert.
#'
#'     @return list a list with two attributes: the \code{Y} which is the
#'      output of the formula and the \code{X} the independent variables in
#'      this formula.
#'   } 
#' 
#'   \item{\code{getFamilty}}{ 
#'     Active method. Returns the family provided on initialization.
#' 
#'     @return string the family.
#'   } 
#' 
#'   \item{\code{getX}}{ 
#'     Active method. Returns the X component of the formula provided on
#'     initialization.
#' 
#'     @return list the X component of the formula
#'   } 
#' 
#'   \item{\code{getY}}{ 
#'     Active method. Returns the Y component of the formula provided on
#'     initialization.
#' 
#'     @return string the outcome variable name
#'   } 
#'
#'   \item{\code{getValidity}}{ 
#'     Active method. Checks the validity of the \code{RelevantVariable}
#'     instance. This method is called automatically on initialization.
#' 
#'     @return boolean true if everything is valid, or it throws if not valid.
#'   } 
#' 
#' }  
#' @export
RelevantVariable <- R6Class("RelevantVariable",
  public =
    list(
      initialize = function(formula, family) {
        formula <- Arguments$getInstanceOf(formula, 'formula')
        formula.parsed  <- self$parse_formula(formula)

        private$formula = formula
        private$formula.X = formula.parsed$X
        private$formula.Y = formula.parsed$Y
        private$family = family
        self$getValidity
      },

      get_formula_string = function(X = NULL, Y = NULL) {
        if (is.null(X)) {
          X <- self$getX 
        }
        if (is.null(Y)) {
          Y <- self$getY 
        }
        paste(Y, '~', paste(sort(X), collapse = ' + '))
      },

      parse_formula = function(formula){
        vars <- all.vars(formula)
        depvar <- head(vars, 1)
        names(depvar) <- depvar

        indepvar <- tail(vars, -1)
        if(length(indepvar) == 1 & indepvar[1] == c('.')){
          indepvar <- c()
        } else {
          names(indepvar) <- indepvar
        }

        list(Y = depvar, X = indepvar)
      }
    ),
  active =
    list(
        getFamily = function() {
          return(private$family)
        },
        getX = function() {
          return(private$formula.X)
        },
        getY = function() {
          private$formula.Y 
        },
        getValidity = function() {
          errors <- character()
          if(!is.a(private$formula, 'formula')){
            msg <- 'Provided formula should be a formula'
            errors <- c(errors, msg)
          }
          if(!private$family %in% RelevantVariable.get_supported_families()){
            msg <- paste('Provided family', private$family, 'not supported')
            errors <- c(errors, msg)
          }
          if (length(errors) > 0) throw(errors)

          if(length(private$formula.X) > 0) {
            labels <- unique(attr(terms(private$formula), 'term.labels'))
            needed <- c(self$getX, self$getY)
            interactionTerms <- setdiff(labels, needed)
            if(length(interactionTerms) != 0) warning(paste('Interactions are not yet supported and are ignored',interactionTerms))
          }
          return(TRUE) 
        }
        ),
  private =
  list(
        formula = NULL,
        formula.X = NULL,
        formula.Y = NULL,
        family = NULL
      )
)
# Static functions
# ================
RelevantVariable.get_supported_families <- function() {
  return(c('binomial', 'gaussian'))
}

#' Algorithm to find a possible ordering of the functions.
#' The worst case run time of this algorithm is pretty bad, and can it
#' probably done more efficiently
#' 
#' @param relevantVariables the relevant variables to sort
#' @param verbose (default false) whether or not to be verbose when sorting
#' @export 
RelevantVariable.find_ordering <- function(relevantVariables, verbose=FALSE) {
  dependencies <- list()
  order <- c()
  managed_deps <- c()

  for (rv in relevantVariables) {
    dependencies[[rv$getY]] <- list(deps = rv$getX, rv= rv)
  }

  for (Y in names(dependencies)) {
    dependencies[[Y]]$deps <- intersect(dependencies[[Y]][['deps']], names(dependencies)) 
  }

  i <- 1
  while(length(dependencies) > 0) {
    if(i > length(dependencies)) {
      throw('Intractable problem! There are interdependencies that cannot be solved!')
    }

    delta <- setdiff(dependencies[[i]]$deps, managed_deps)
    if (length(delta) == 0) {
      current <- names(dependencies)[[i]]
      verbose && cat(verbose, 'Adding', current,'\n')
      order <- c(order, dependencies[[i]]$rv) 
      managed_deps <- c(managed_deps, current) 
      dependencies[[i]] <- NULL
      i <- 1
      next
    }
    i <- i + 1
  }
  names(order) <- managed_deps
  order
}
